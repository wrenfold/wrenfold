"""Utility functions to support code-generation."""
import dataclasses
import inspect
import itertools
import pathlib
import string
import typing as T

from . import sym

# Import the C++ code-generation module into this file.
from pywrenfold.wf_wrapper import codegen
from pywrenfold.wf_wrapper.codegen import transpile, CppGenerator, RustGenerator

U = T.TypeVar("U")


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

# Refer to possible types we put in the generated code.
CodegenType = T.Union[codegen.ScalarType, codegen.MatrixType, codegen.CustomType]

# The different wrapped generator types.
GeneratorTypes = T.Union[CppGenerator, RustGenerator]


def convert_to_internal_type(python_type: T.Type,
                             cached_custom_types: T.Dict[T.Type, CodegenType]) -> CodegenType:
    """
    Convert a python type to the internal C++ representation.
    """
    if issubclass(python_type, sym.Expr):
        return codegen.ScalarType(codegen.NumericType.Real)
    elif issubclass(python_type, sym.MatrixExpr):
        shape = _get_matrix_shape(matrix_type=python_type)
        return codegen.MatrixType(*shape)
    elif dataclasses.is_dataclass(python_type):
        if python_type in cached_custom_types:
            return cached_custom_types[python_type]
        # Type is not in the cache - convert it and cache it before returning:
        custom_type = convert_dataclass_type(
            dataclass_type=python_type, cached_custom_types=cached_custom_types)
        cached_custom_types[python_type] = custom_type
        return custom_type
    else:
        raise TypeError(f'Invalid type used in annotation: {python_type}')


def convert_dataclass_type(dataclass_type: T.Type,
                           cached_custom_types: T.Dict[T.Type, CodegenType]) -> codegen.CustomType:
    """
    Convert a dataclass type to a CustomType.
    """
    assert dataclasses.is_dataclass(
        dataclass_type), f"Provided type `{dataclass_type}` is not a dataclass"

    fields_converted: T.List[T.Tuple[str, CodegenType]] = []
    for field in dataclasses.fields(dataclass_type):
        field_type = convert_to_internal_type(
            python_type=field.type, cached_custom_types=cached_custom_types)
        fields_converted.append((field.name, field_type))

    return codegen.CustomType(
        name=dataclass_type.__name__, fields=fields_converted, python_type=dataclass_type)


def _get_matrix_shape(matrix_type: T.Type) -> T.Tuple[int, int]:
    """
    Get the SHAPE field off of a matrix type annotation. Check that it has the correct type.
    """
    shape = getattr(matrix_type, "SHAPE")
    if (shape is None or not isinstance(shape, tuple) or len(shape) != 2 or
            not isinstance(shape[0], int) or not isinstance(shape[1], int)):
        raise KeyError("Matrix types must be annotated with the SHAPE field with type (int, int)")
    return T.cast(T.Tuple[int, int], shape)


def map_expressions_into_custom_type(expressions: T.List[sym.Expr],
                                     custom_type: T.Type[U]) -> T.Tuple[U, T.List[sym.Expr]]:
    """
    Given a flat list of expressions, construct an instance of `custom_type` by recursively
    filling its members with the provided expressions. Thus we construct a symbolic instance
    of the user-provided type.

    Expressions are inserted into fields in the order the fields appear.
    """
    assert dataclasses.is_dataclass(
        custom_type), f"Provided type `{custom_type}` is not a dataclass"

    constructor_kwargs = dict()
    for field in dataclasses.fields(custom_type):
        if not expressions:
            # This shouldn't ever happen, the C++ side has allocated enough expressions based on fields.
            raise RuntimeError("Ran out of expressions while building custom type")

        if issubclass(field.type, sym.Expr):
            constructor_kwargs[field.name] = expressions[0]
            expressions = expressions[1:]
        elif issubclass(field.type, sym.MatrixExpr):
            # Expressions were packed in row-major order as expected by our matrix type.
            mat = sym.matrix(expressions).reshape(_get_matrix_shape(field.type))
            constructor_kwargs[field.name] = mat
            expressions = expressions[mat.size:]
        elif dataclasses.is_dataclass(field.type):
            # Recurse
            constructor_kwargs[field.name], expressions = map_expressions_into_custom_type(
                expressions=expressions, custom_type=field.type)
        else:
            raise TypeError(
                f'Invalid member type `{field.name}: {field.type}` used in type `{custom_type}`')

    # All the constructor arguments have been assembled.
    return T.cast(U, custom_type(**constructor_kwargs)), expressions


def map_expressions_out_of_custom_type(instance: T.Any) -> T.List[sym.Expr]:
    """
    Given an instance of a custom dataclass type, flatten its contents into a list of expressions.

    This is effectively the inverse of `map_expressions_into_custom_type`.
    """
    assert dataclasses.is_dataclass(
        instance), f"Provided object of type `{type(instance)}` is not a dataclass"

    expressions: T.List[sym.Expr] = []
    for field in dataclasses.fields(instance):
        value = getattr(instance, field.name)
        if issubclass(field.type, sym.Expr):
            expressions.append(value)
        elif issubclass(field.type, sym.MatrixExpr):
            expressions.extend(itertools.chain.from_iterable(value))
        elif dataclasses.is_dataclass(field.type):
            expressions += map_expressions_out_of_custom_type(instance=value)
        else:
            raise TypeError(
                f'Invalid member type `{field.name}: {field.type}` used in type `{type(instance)}`')

    return expressions


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
        arg_type = convert_to_internal_type(
            python_type=annotated_type, cached_custom_types=cached_types)

        arg_elements = description.add_input_argument(arg_name, arg_type)
        if isinstance(arg_type, codegen.CustomType):
            kwargs[arg_name], _ = map_expressions_into_custom_type(
                expressions=arg_elements, custom_type=annotated_type)
        else:
            kwargs[arg_name] = arg_elements

    # run the function
    result_expressions: CodegenFuncInvocationResult = func(**kwargs)

    if isinstance(result_expressions,
                  (sym.Expr, sym.MatrixExpr)) or dataclasses.is_dataclass(result_expressions):
        # if one thing was returned, interpret it as the return value:
        result_expressions = (ReturnValue(expression=result_expressions),)

    for val in result_expressions:
        if isinstance(val, ReturnValue):
            if dataclasses.is_dataclass(val.expression):
                custom_type = convert_to_internal_type(
                    python_type=type(val.expression), cached_custom_types=cached_types)
                description.set_return_value(
                    custom_type=custom_type,
                    expressions=map_expressions_out_of_custom_type(instance=val.expression))
            else:
                description.set_return_value(val.expression)
        elif isinstance(val, OutputArg):
            if dataclasses.is_dataclass(val.expression):
                custom_type = convert_to_internal_type(
                    python_type=type(val.expression), cached_custom_types=cached_types)
                description.set_return_value(
                    custom_type=custom_type,
                    expressions=map_expressions_out_of_custom_type(instance=val.expression))
            else:
                description.add_output_argument(val.name, val.is_optional, val.expression)
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
