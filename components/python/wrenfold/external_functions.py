"""
Utilities for defining user-specified external functions.
"""
import typing as T

from .code_generation import codegen
from .sym import Expr, MatrixExpr, CompoundExpr, AnyExpression, \
  create_compound_expression_elements, create_custom_type_construction
from . import custom_types


class ExternalFunc:
    """
    Callable object that represents a user-declared external function.
    """

    def __init__(self, wrapped_func: codegen.ExternalFunction) -> None:
        self._inner: codegen.ExternalFunction = wrapped_func

    def __repr__(self) -> str:
        return repr(self._inner)

    def __call__(self, *args, **kwargs) -> T.Any:
        """
        Generate expressions to represent an invocation of the external function.
        """
        return _invoke_external_function(self._inner, *args, **kwargs)

    @property
    def name(self) -> str:
        return self._inner.name

    @property
    def num_arguments(self) -> int:
        return self._inner.num_arguments

    @property
    def return_type(self) -> T.Union[codegen.ScalarType, codegen.MatrixType, codegen.CustomType]:
        return self._inner.return_type

    def __hash__(self) -> int:
        return hash(self._inner)

    def __eq__(self, other: T.Union['ExternalFunc', codegen.ExternalFunction]) -> bool:
        if isinstance(other, ExternalFunc):
            return self._inner == other._inner
        elif isinstance(other, codegen.ExternalFunction):
            return self._inner == other
        else:
            return False


def declare_external_function(
    name: str,
    arguments: T.Iterable[T.Tuple[str, T.Type]],
    return_type: T.Type,
) -> ExternalFunc:
    """
    Declare an external function. External functions are implemented by the user outside of the code
    generation framework. They can be invoked on symbolic expressions, and their returned values
    can be utilized as part of further symbolic expressions.

    At code-generation time, the user can override the `format_custom_function_call` formatter to
    specify how their external function is mapped to actual output code. The `name` field does not
    need to match the actual function name in the target language, since this mapping is implemented
    by the the code-generator.

    By necessity, wrenfold must assume that external functions are pure (without side effects). Any
    two identical calls (ie. having the same function and argument lists) are assumed to be
    interchangeable, and will be de-duplicated during transpilation.

    :param name: String name for the function.
    :arguments: Iterable of (name, type) pairs that define the argument list.

    :return_type: An ExternalFunc object that can be invoked via the __call__ operator. The args to
    __call__ should have types matching `arguments`.
    """
    type_cache: T.Dict[T.Type, custom_types.CodegenType] = dict()

    # If custom types are specified, we need to construct `CustomType` objects to pass to C++:
    converted_args: T.List[str, T.Union[codegen.ScalarType, codegen.MatrixType,
                                        codegen.CustomType]] = []
    for (arg_name, python_arg_type) in arguments:
        internal_type = custom_types.convert_to_internal_type(
            python_type=python_arg_type, cached_custom_types=type_cache)
        converted_args.append((arg_name, internal_type))

    converted_return_type = custom_types.convert_to_internal_type(
        python_type=return_type, cached_custom_types=type_cache)

    # Create c++ object that will represent this external function.
    wrapper_func = codegen.ExternalFunction(
        name=name, arguments=converted_args, return_type=converted_return_type)

    # Next we make a python wrapper it that will handle mapping of data in/out of custom types.
    return ExternalFunc(wrapped_func=wrapper_func)


def _combine_args(func: codegen.ExternalFunction, args: T.Sequence[T.Any],
                  kwargs: T.Dict[str, T.Any]) -> T.List[T.Any]:
    """
    Combine args and kwargs into one ordered list.
    """
    if len(args) + len(kwargs) != func.num_arguments:
        raise RuntimeError(f"Incorrect number of arguments passed to function `{func.name}`. " +
                           f"Expected: {func.num_arguments}, Actual: {len(args) + len(kwargs)}")

    index_and_value: T.List[T.Tuple[int, T.Any]] = []
    for (key, value) in kwargs.items():
        position = func.arg_position(key)
        if position is None:
            raise KeyError(
                f"Function `{func.name}` does not accept a keyword argument with name `{key}`.")
        if position < len(args):
            raise RuntimeError(
                f"More than one value specified for argument `{key}` to function `{func.name}`.")
        index_and_value.append((position, value))

    return list(args) + [v for (_, v) in sorted(index_and_value, key=lambda pair: pair[0])]


def _invoke_external_function(func: codegen.ExternalFunction, *args, **kwargs):
    """
    Call `wrapper_func` with the provided arguments.
    """
    # Flatten args to gether and make sure there are no duplicates:
    combined_args = _combine_args(func=func, args=args, kwargs=kwargs)

    # The expected types for each argument:
    arg_names_and_types = [(a.name, a.type) for a in func.arguments]

    converted_args: T.List[AnyExpression] = []
    for arg, (arg_name, arg_type) in zip(combined_args, arg_names_and_types):
        if isinstance(arg, (Expr, MatrixExpr)):
            converted_args.append(arg)  # Type is checked later in custom_function.cc
            continue
        elif isinstance(arg, (int, float)):
            converted_args.append(Expr(arg))
            continue

        if not isinstance(arg_type, codegen.CustomType):
            raise TypeError(
                f"Argument `{arg_name}` of function `{func.name}` should be of type {arg_type}, " +
                f"but we received type {type(arg)}.")

        # Check that the type of `arg` matches the type we constructed our `custom_type` with:
        if type(arg) is not arg_type.python_type:
            raise TypeError(f"Argument `{arg_name}` of function `{func.name}` should be of type " +
                            f"{arg_type.python_type}, but we received {type(arg)}")

        if isinstance(arg, custom_types.Opaque):
            assert arg_type.total_size == 0, "Custom type should not have any members"
            # An opaque type handed to us by the user, it may be a function input or the result of
            # an external call:
            if arg._provenance is None:
                raise RuntimeError(
                    f"Opaque argument {arg_name} to function {func.name} has unclear provenance.")
            converted_args.append(arg._provenance)
        else:
            expressions = custom_types.map_expressions_out_of_custom_type(instance=arg)
            converted_args.append(create_custom_type_construction(arg_type, expressions))

    result: AnyExpression = func(converted_args)
    if not isinstance(result, CompoundExpr):
        # Scalars and matrices are directly returned.
        return result

    # If this is a compound expression, we should convert it to a python type.
    returned_custom_type = func.return_type
    assert isinstance(
        returned_custom_type,
        codegen.CustomType), f"Return type should be custom type: {returned_custom_type}"

    if issubclass(returned_custom_type.python_type, custom_types.Opaque):
        return returned_custom_type.python_type(provenance=result)
    else:
        # This is a dataclass type, fill it with expressions:
        expressions = create_compound_expression_elements(
            provenance=result, num=returned_custom_type.total_size)
        result, _ = custom_types.map_expressions_into_custom_type(
            expressions=expressions, custom_type=returned_custom_type.python_type)
        return result
