"""Utility functions to support code-generation."""
import dataclasses
import inspect
import pathlib
import typing as T

from . import sym

# Import the C++ code-generation module into this file.
from pywrenfold.wf_wrapper import codegen
from pywrenfold.wf_wrapper.codegen import transpile, generate_cpp, generate_rust

AstVariantAnnotation = T.Union[codegen.Add, codegen.AssignTemporary, codegen.AssignOutputArgument,
                               codegen.Branch, codegen.Call, codegen.Cast, codegen.Compare,
                               codegen.ConstructReturnValue, codegen.Declaration,
                               codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                               codegen.Multiply, codegen.OptionalOutputBranch, codegen.VariableRef,]


@dataclasses.dataclass
class ReturnValue:
    """Designate a return value in the result of symbolic function invocation."""
    expression: T.Union[sym.Expr, sym.MatrixExpr]


@dataclasses.dataclass
class OutputArg:
    """Designate an output argument in the result of a symbolic function invocation."""
    expression: T.Union[sym.Expr, sym.MatrixExpr]
    name: str
    is_optional: bool = False


# Union of ReturnValue and OutputArg
ReturnValueOrOutputArg = T.Union[ReturnValue, OutputArg]

# Things that can be returned from symbolic python functions.
CodegenFuncInvocationResult = T.Union[sym.Expr, sym.MatrixExpr, T.Iterable[ReturnValueOrOutputArg]]


def create_function_description(func: T.Callable[..., CodegenFuncInvocationResult],
                                name: T.Optional[str] = None) -> codegen.FunctionDescription:
    """
    Accept a python function that manipulates symbolic mathematical expressions, and convert it
    to a `FunctionDescription` object. The provided function is invoked, and its output expressions
    are captured and stored in the FunctionDescription, along with a signature that carries type
    information required to emit code.

    >>> def foo(x: RealScalar, y: RealScalar):
    >>>     return [OutputArg(x + y, "z")]
    >>>
    >>> description = create_function_description(func=foo)
    >>> definition = transpile(description=description)
    >>> code = generate_cpp(definitions=[definition])

    TODO: Add support saving and emitting the docstring.

    :param func: A python function whose arguments are type-annotated (see type_annotations.py) so
      that we may extract type signatures. The function should return a list of ReturnValue or
      OutputArg objects. These indicate how outputs are passed out of the function.

    :param name: String name of the function.

    :return: An instance of `FunctionDescription`.
    """
    spec = inspect.getfullargspec(func=func)
    description = codegen.FunctionDescription(name=name or func.__name__)

    kwargs = dict()
    for arg_name in spec.args:
        if arg_name not in spec.annotations:
            raise KeyError(f'Missing type annotation for argument: {arg_name}')
        type_annotation = spec.annotations[arg_name]
        if issubclass(type_annotation, sym.Expr):
            arg_type = codegen.ScalarType(codegen.NumericType.Real)
        elif issubclass(type_annotation, sym.MatrixExpr):
            arg_type = codegen.MatrixType(*type_annotation.SHAPE)
        else:
            raise TypeError(f'Invalid type used in annotation: {type_annotation}')

        kwargs[arg_name] = description.add_input_argument(arg_name, arg_type)

    # run the function
    result_expressions: CodegenFuncInvocationResult = func(**kwargs)
    if isinstance(result_expressions, (sym.Expr, sym.MatrixExpr)):
        # if one thing was returned, interpret it as the return value:
        result_expressions = (ReturnValue(expression=result_expressions),)

    for val in result_expressions:
        if isinstance(val, ReturnValue):
            description.set_return_value(val.expression)
        elif isinstance(val, OutputArg):
            description.add_output_argument(val.name, val.is_optional, val.expression)
        else:
            raise TypeError(f"Returned values must be: {ReturnValueOrOutputArg}, got: {type(val)}")

    return description


CPP_PREAMBLE_TEMPLATE = \
"""// Machine generated code.
#include <cmath>
#include <cstdint>

#include <wf_runtime/span.h>

namespace {namespace} {{

{code}

}} // namespace {namespace}
"""


def apply_cpp_preamble(code: str, namespace: str) -> str:
    """
    Wrap C++ code in a preamble that includes the necessary headers.
    :param code: Output C++ code.
    :param namespace: Namespace to put generated code in.
    :return: Formatted string.
    """
    return CPP_PREAMBLE_TEMPLATE.format(code=code, namespace=namespace)


def mkdir_and_write_file(code: str, path: T.Union[str, pathlib.Path]):
    """
    Write `code` to the specified path. Create intermediate directories as required.

    :param code: File contents.
    :param path: The output path.
    :return: None
    """
    if isinstance(path, str):
        path = pathlib.Path(path)
    if not path.parent.exists():
        path.parent.mkdir(parents=True)
    with open(path, 'w') as handle:
        handle.write(code)
        handle.flush()
