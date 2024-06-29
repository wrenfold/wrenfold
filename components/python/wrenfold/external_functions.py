"""
Utilities for defining user-specified external functions.
"""
import typing as T

from . import sym
from . import custom_types
from . import type_annotations
from . import type_info

from pywrenfold.gen import PyExternalFunction


class ExternalFunction(PyExternalFunction):
    """
    Callable object that represents a user-provided external function. External functions may be
    handwritten methods that live in the user's codebase. Wrenfold can invoke these from generated
    code, provided they are declared with :func:`declare_external_function`.
    """

    def __init__(self, super_val: PyExternalFunction) -> None:
        super(ExternalFunction, self).__init__(super_val)

    def __call__(self, *args, **kwargs) -> T.Any:
        """
        Generate expressions to represent an invocation of ``self``.
        """
        return _invoke_external_function(self, *args, **kwargs)


def declare_external_function(
    name: str,
    arguments: T.Iterable[T.Tuple[str, T.Type]],
    return_type: T.Type,
) -> ExternalFunction:
    """
    Declare an external function. External functions are implemented by the user outside of the code
    generation framework. They can be invoked on symbolic expressions, and their returned values
    can be utilized as part of further symbolic expressions.

    At code-generation time, the user can override the ``format_custom_function_call`` formatter to
    specify how their external function is mapped to actual output code. This can be used to achieve
    certain customizations:

      * Mapping ``name`` to different functions in different target languages.
      * Inserting casts or type conversions around arguments.

    By necessity, wrenfold **must assume that external functions are pure** (without side effects).
    Any two identical calls (ie. having the same function and argument lists) are assumed to be
    interchangeable, and will be de-duplicated during transpilation.

    The ``arguments`` sequence specifies the names and expected types of the function arguments. The
    types may be:

      * One of the scalar/matrix types from :doc:`type annotations <type_annotations>`.
      * A `dataclass <https://docs.python.org/3/library/dataclasses.html>`_ type corresponding to a
        user-declared type.
      * A subclass of :class:`wrenfold.type_annotations.Opaque`.

    Args:
      name: String name for the function.
      arguments: Iterable of (name, type) pairs that define the argument list of the function.
      return_type: Return type of the function.

    Returns:
        An ExternalFunc object that can be invoked via the ``__call__`` operator. The args to
        ``__call__`` should have types matching `arguments`.
    """
    type_cache: T.Dict[T.Type, custom_types.CodegenType] = dict()

    # If custom types are specified, we need to construct `CustomType` objects to pass to C++:
    converted_args: T.List[str, T.Union[type_info.ScalarType, type_info.MatrixType,
                                        type_info.CustomType]] = []
    for (arg_name, python_arg_type) in arguments:
        internal_type = custom_types.convert_to_internal_type(
            python_type=python_arg_type, cached_custom_types=type_cache)
        converted_args.append((arg_name, internal_type))

    converted_return_type = custom_types.convert_to_internal_type(
        python_type=return_type, cached_custom_types=type_cache)

    # Create c++ object that will represent this external function.
    wrapper_func = PyExternalFunction(
        name=name, arguments=converted_args, return_type=converted_return_type)

    # Next we make a python wrapper that will handle mapping of data in/out of custom types.
    return ExternalFunction(wrapper_func)


def _combine_args(func: PyExternalFunction, args: T.Sequence[T.Any],
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


def _invoke_external_function(func: PyExternalFunction, *args, **kwargs):
    """
    Call `wrapper_func` with the provided arguments.
    """
    # Flatten args together and make sure there are no duplicates:
    combined_args = _combine_args(func=func, args=args, kwargs=kwargs)

    # The expected types for each argument:
    arg_names_and_types = [(a.name, a.type) for a in func.arguments]

    converted_args: T.List[sym.AnyExpression] = []
    for arg, (arg_name, arg_type) in zip(combined_args, arg_names_and_types):
        if isinstance(arg, (sym.Expr, sym.MatrixExpr)):
            converted_args.append(arg)  # Type is checked later in custom_function.cc
            continue
        elif isinstance(arg, (int, float)):
            converted_args.append(sym.Expr(arg))
            continue

        if not isinstance(arg_type, type_info.CustomType):
            raise TypeError(
                f"Argument `{arg_name}` of function `{func.name}` should be of type {arg_type}, " +
                f"but we received type {type(arg)}.")

        # Check that the type of `arg` matches the type we constructed our `custom_type` with:
        if type(arg) is not arg_type.python_type:
            raise TypeError(f"Argument `{arg_name}` of function `{func.name}` should be of type " +
                            f"{arg_type.python_type}, but we received {type(arg)}")

        if isinstance(arg, type_annotations.Opaque):
            assert arg_type.total_size == 0, "Custom type should not have any members"
            # An opaque type handed to us by the user, it may be a function input or the result of
            # an external call:
            if arg._provenance is None:
                raise RuntimeError(
                    f"Opaque argument {arg_name} to function {func.name} has unclear provenance.")
            converted_args.append(arg._provenance)
        else:
            expressions = custom_types.map_expressions_out_of_custom_type(instance=arg)
            converted_args.append(sym.create_custom_type_construction(arg_type, expressions))

    result: sym.AnyExpression = func.call(converted_args)
    if not isinstance(result, sym.CompoundExpr):
        # Scalars and matrices are directly returned.
        return result

    # If this is a compound expression, we should convert it to a python type.
    returned_custom_type = func.return_type
    assert isinstance(
        returned_custom_type,
        type_info.CustomType), f"Return type should be custom type: {returned_custom_type}"

    if issubclass(returned_custom_type.python_type, type_annotations.Opaque):
        return returned_custom_type.python_type(provenance=result)
    else:
        # This is a dataclass type, fill it with expressions:
        expressions = sym.create_compound_expression_elements(
            provenance=result, num=returned_custom_type.total_size)
        result, _ = custom_types.map_expressions_into_custom_type(
            expressions=expressions, custom_type=returned_custom_type.python_type)
        return result
