"""
Utility functions to support code-generation.
"""
import dataclasses
import inspect
import pathlib
import typing as T

from . import sym
from . import custom_types
from . import type_annotations
from . import type_info

from pywrenfold.gen import (
    Argument,
    ArgumentDirection,
    BaseGenerator,
    CppGenerator,
    FunctionDescription,
    OptimizationParams,
    OutputKey,
    RustGenerator,
    cse_function_description,
    transpile,
)


@dataclasses.dataclass
class ReturnValue:
    """
    Designate a return value in the result of symbolic function invocation.

    Attributes:
      expression: The returned value. This may be an expression, or an instance of a user-provided
        custom type.
    """
    expression: T.Union[sym.Expr, sym.MatrixExpr, T.Any]


@dataclasses.dataclass
class OutputArg:
    """
    Designate an output argument in the result of a symbolic function invocation.

    Attributes:
      expression: Value of the output argument. This may be an expression, or an instance of a
        user-provided custom type.
      name: Name of the argument.
      is_optional: Specify whether the output argument is optional or not.
    """
    expression: T.Union[sym.Expr, sym.MatrixExpr, T.Any]
    name: str
    is_optional: bool = False


# Union of ReturnValue and OutputArg
ReturnValueOrOutputArg = T.Union[ReturnValue, OutputArg]

# Things that can be returned from symbolic python functions.
CodegenFuncInvocationResult = T.Union[sym.Expr, sym.MatrixExpr, T.Sequence[ReturnValueOrOutputArg]]


def create_function_description(func: T.Callable[..., CodegenFuncInvocationResult],
                                name: T.Optional[str] = None) -> FunctionDescription:
    """
    Accept a python function that manipulates symbolic mathematical expressions, and convert it
    to a :class:`wrenfold.code_generation.FunctionDescription` object. The provided function is
    invoked, and its output expressions are captured and stored, along with a signature that
    carries type information required to emit code.

    Tip:
      The provided callable must be type annotated so that wrenfold can deduce the type of input
      expressions required to invoke. See :doc:`type_annotations` for built-in types that can be
      used to annotate functions. The function should return either:

        * A sequence of :class:`wrenfold.code_generation.ReturnValue` or
          :class:`wrenfold.code_generation.OutputArg` objects, OR
        * A single :class:`wrenfold.sym.Expr` or :class:`wrenfold.sym.MatrixExpr` object, which will
          be interpreted as a ``ReturnValue``.

    Args:
      func: A python function with type-annotated arguments.
      name: String name of the function.

    Returns:
      An instance of :class:`wrenfold.code_generation.FunctionDescription`.

    Example:
      >>> from wrenfold.type_annotations import FloatScalar
      >>> from wrenfold import code_generation
      >>>
      >>> def foo(x: FloatScalar, y: FloatScalar):
      >>>     # One return value, and one output argument named `z`:
      >>>     return [code_generation.ReturnValue(x * y), code_generation.OutputArg(x + y, "z")]
      >>> description = code_generation.create_function_description(func=foo)
      >>> print(description)
      FunctionDescription('foo', 3 args)

      The description can then be transpiled to a target language:

      >>> definition = code_generation.transpile(description)
      >>> print(definition)
      FunctionDefinition('foo', <3 arguments>, <7 elements>)
      >>> code = code_generation.CppGenerator().generate(definition=definition)
      >>> print(code)

      .. code-block:: cpp
        :linenos:

        template <typename Scalar>
        Scalar foo(const Scalar x, const Scalar y, Scalar& z)
        {
            // Operation counts:
            // add: 1
            // multiply: 1
            // total: 2

            const Scalar v01 = y;
            const Scalar v00 = x;
            const Scalar v04 = v00 + v01;
            const Scalar v02 = v00 * v01;
            z = v04;
            return v02;
        }
    """
    spec = inspect.getfullargspec(func=func)
    description = FunctionDescription(name=name or func.__name__)

    cached_types: T.Dict[T.Type, type_info.CustomType] = dict()
    kwargs = dict()
    for arg_name in spec.args:
        if arg_name not in spec.annotations:
            raise KeyError(f'Missing type annotation for argument: {arg_name}')
        # Map argument types to something the code-generation logic can understand:
        annotated_type = spec.annotations[arg_name]
        arg_type = custom_types.convert_to_internal_type(
            python_type=annotated_type, cached_custom_types=cached_types)

        input_expression = description.add_input_argument(arg_name, arg_type)
        if isinstance(arg_type, type_info.CustomType):
            if issubclass(annotated_type, type_annotations.Opaque):
                kwargs[arg_name] = annotated_type(provenance=input_expression)
            else:
                arg_elements = sym.create_compound_expression_elements(
                    provenance=input_expression, num=arg_type.total_size)
                kwargs[arg_name], _ = custom_types.map_expressions_into_custom_type(
                    expressions=arg_elements, custom_type=annotated_type)
        else:
            kwargs[arg_name] = input_expression

    # run the function
    result_expressions: CodegenFuncInvocationResult = func(**kwargs)

    if isinstance(result_expressions,
                  (sym.Expr, sym.MatrixExpr)) or dataclasses.is_dataclass(result_expressions):
        # if one thing was returned, interpret it as the return value:
        result_expressions = (ReturnValue(expression=result_expressions),)

    for val in result_expressions:
        if isinstance(val, ReturnValue):
            if dataclasses.is_dataclass(val.expression):
                custom_type = custom_types.convert_to_internal_type(
                    python_type=type(val.expression), cached_custom_types=cached_types)
                description.set_return_value(
                    custom_type=custom_type,
                    expressions=custom_types.map_expressions_out_of_custom_type(
                        instance=val.expression))
            else:
                description.set_return_value(val.expression)
        elif isinstance(val, OutputArg):
            if dataclasses.is_dataclass(val.expression):
                custom_type = custom_types.convert_to_internal_type(
                    python_type=type(val.expression), cached_custom_types=cached_types)
                description.add_output_argument(
                    name=val.name,
                    is_optional=val.is_optional,
                    custom_type=custom_type,
                    expressions=custom_types.map_expressions_out_of_custom_type(
                        instance=val.expression))
            else:
                description.add_output_argument(
                    name=val.name, is_optional=val.is_optional, value=val.expression)
        else:
            raise TypeError(f"Returned values must be: {ReturnValueOrOutputArg}, got: {type(val)}")

    return description


def generate_function(func: T.Callable[..., CodegenFuncInvocationResult],
                      generator: T.Union[CppGenerator, RustGenerator, BaseGenerator],
                      name: T.Optional[str] = None,
                      optimization_params: T.Optional[OptimizationParams] = None) -> str:
    """
    Accept a python function that manipulates symbolic mathematical expressions, and convert it
    to code in the language emitted by ``generator``. This is a three-step process:

        #. The signature of the provided function is inspected to generate input arguments. Next, it is invoked
           and the symbolic outputs are captured in a :class:`wrenfold.code_generation.FunctionDescription`.
        #. This description is converted to AST using :func:`wrenfold.code_generation.transpile`. Duplicate
           operations are eliminated during this step, and conditionals are converted to control flow.
        #. Lastly, the AST is passed to the provided generator to emit usable code.

    Tip:

      The steps above can also be performed individually. For instance, you might wish to pass the output
      AST to multiple generators if many simultaneous target languages are required. See
      :func:`wrenfold.code_generation.create_function_description` for an example.

    Args:
        func: A python function with type-annotated arguments. See
          :func:`wrenfold.code_generation.create_function_description` for notes on the expected signature.
        generator: Instance of a code generator, eg. :class:`wrenfold.code_generation.CppGenerator`.
        name: Name of the function. If unspecified, ``func.__name__`` will be used.
        optimization_params: Parameters governing simplifications/optimizations applied to the output code.

    Returns: Generated code.

    Example:
      >>> from wrenfold.type_annotations import FloatScalar
      >>> from wrenfold import code_generation
      >>>
      >>> def foo(x: FloatScalar, y: FloatScalar):
      >>>     # One return value, and one output argument named `z`:
      >>>     return [code_generation.ReturnValue(x * y), code_generation.OutputArg(x + y, "z")]
      >>>
      >>> code = code_generation.generate_function(func=foo)
      >>> print(code)

      .. code-block:: cpp
        :linenos:

        template <typename Scalar>
        Scalar foo(const Scalar x, const Scalar y, Scalar& z)
        {
            // Operation counts:
            // add: 1
            // multiply: 1
            // total: 2

            const Scalar v01 = y;
            const Scalar v00 = x;
            const Scalar v04 = v00 + v01;
            const Scalar v02 = v00 * v01;
            z = v04;
            return v02;
        }
    """
    description = create_function_description(func=func, name=name)
    func_ast = transpile(description, params=optimization_params)
    return generator.generate(definition=func_ast)


def mkdir_and_write_file(code: str, path: T.Union[str, pathlib.Path]) -> None:
    """
    Write ``code`` to the specified path. Create intermediate directories as required.

    Args:
      code: String containing file contents.
      path: Path to the destination file.
    """
    if isinstance(path, str):
        path = pathlib.Path(path)
    if not path.parent.exists():
        path.parent.mkdir(parents=True)
    with open(path, 'wb') as handle:
        # Encode to UTF8 and write binary so we get \n and not \r\n
        handle.write(code.encode('utf-8'))
        handle.flush()
