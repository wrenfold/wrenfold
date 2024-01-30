"""Utility functions to support code-generation."""
import dataclasses
import inspect
import pathlib
import string
import typing as T

from . import sym
from . import custom_types

# Import the C++ code-generation module into this file.
from pywrenfold.wf_wrapper import codegen
from pywrenfold.wf_wrapper.codegen import transpile, CppGenerator, RustGenerator


@dataclasses.dataclass
class ReturnValue:
    """Designate a return value in the result of symbolic function invocation."""
    expression: T.Union[sym.Expr, sym.MatrixExpr, T.Any]


@dataclasses.dataclass
class OutputArg:
    """Designate an output argument in the result of a symbolic function invocation."""
    expression: T.Union[sym.Expr, sym.MatrixExpr, T.Any]
    name: str
    is_optional: bool = False


# Union of ReturnValue and OutputArg
ReturnValueOrOutputArg = T.Union[ReturnValue, OutputArg]

# Things that can be returned from symbolic python functions.
CodegenFuncInvocationResult = T.Union[sym.Expr, sym.MatrixExpr, T.Iterable[ReturnValueOrOutputArg]]

# The different wrapped generator types.
GeneratorTypes = T.Union[CppGenerator, RustGenerator]


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
    >>> code = CppGenerator.generate(definitions=definition)

    TODO: Add support saving and emitting the docstring.

    :param func: A python function whose arguments are type-annotated (see type_annotations.py) so
      that we may extract type signatures. The function should return a list of ReturnValue or
      OutputArg objects. These indicate how outputs are passed out of the function.

    :param name: String name of the function.

    :return: An instance of `FunctionDescription`.
    """
    spec = inspect.getfullargspec(func=func)
    description = codegen.FunctionDescription(name=name or func.__name__)

    cached_types: T.Dict[T.Type, codegen.CustomType] = dict()
    kwargs = dict()
    for arg_name in spec.args:
        if arg_name not in spec.annotations:
            raise KeyError(f'Missing type annotation for argument: {arg_name}')
        # Map argument types to something the code-generation logic can understand:
        annotated_type = spec.annotations[arg_name]
        arg_type = custom_types.convert_to_internal_type(
            python_type=annotated_type, cached_custom_types=cached_types)

        input_expression = description.add_input_argument(arg_name, arg_type)
        if isinstance(arg_type, codegen.CustomType):
            if issubclass(annotated_type, custom_types.Opaque):
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


class Formatter(string.Formatter):
    """
    Custom string formatter that automatically delegates formatting of ast types to
    a pybind11 wrapped code-generator.
    """

    def __init__(self, generator: GeneratorTypes) -> None:
        super().__init__()
        self._generator: GeneratorTypes = generator

    def format_field(self, value: T.Any, format_spec: str) -> str:
        if codegen.is_formattable_type(type(value)):
            return self._generator.format(value)
        return super().format_field(value, format_spec)


CPP_PREAMBLE_TEMPLATE = \
"""// Machine generated code.
#pragma once
#include <cmath>
#include <cstdint>

#include <wf_runtime/span.h>

namespace {namespace} {{

{code}

}} // namespace {namespace}
"""


def apply_cpp_preamble(code: T.Union[str, T.Sequence[str]], namespace: str) -> str:
    """
    Wrap C++ code in a preamble that includes the necessary headers.
    :param code: Output C++ code, either as a string of sequence of strings.
    :param namespace: Namespace to put generated code in.
    :return: Formatted string.
    """
    if not isinstance(code, str):
        code = '\n\n'.join(code)
    return CPP_PREAMBLE_TEMPLATE.format(code=code, namespace=namespace)


RUST_PREAMBLE_TEMPLATE = \
"""//! Machine generated code.
#![cfg_attr(rustfmt, rustfmt_skip)]

{code}
"""


def apply_rust_preamble(code: T.Union[str, T.Sequence[str]]) -> str:
    """
    Wrap Rust code with a preamble that disables formatting.
    :param code: Output Rust code, either as a string of sequence of strings.
    :return: Formatted string.
    """
    if not isinstance(code, str):
        code = '\n\n'.join(code)
    return RUST_PREAMBLE_TEMPLATE.format(code=code)


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
