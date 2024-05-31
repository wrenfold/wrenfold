"""
Supporting methods for interfacing with user-provided types.
"""
import dataclasses
import typing as T

from . import sym
from . import type_annotations
from . import type_info

# Refer to possible types we put in the generated code.
CodegenType = T.Union[type_info.ScalarType, type_info.MatrixType, type_info.CustomType]

U = T.TypeVar("U")


def convert_to_internal_type(
        python_type: T.Type, cached_custom_types: T.Dict[T.Type,
                                                         type_info.CustomType]) -> CodegenType:
    """
    Convert a python type to the internal C++ representation.

    OMIT_FROM_SPHINX
    """
    if issubclass(python_type, sym.Expr):
        return type_info.ScalarType(type_info.NumericType.Float)
    elif issubclass(python_type, sym.MatrixExpr):
        shape = _get_matrix_shape(matrix_type=python_type)
        return type_info.MatrixType(*shape)
    elif dataclasses.is_dataclass(python_type) or issubclass(python_type, type_annotations.Opaque):
        if python_type in cached_custom_types:
            return cached_custom_types[python_type]
        # Type is not in the cache - convert it and cache it before returning:
        custom_type = create_custom_type(
            python_type=python_type, cached_custom_types=cached_custom_types)
        cached_custom_types[python_type] = custom_type
        return custom_type
    else:
        raise TypeError(f'Invalid type used in annotation: {python_type}')


def create_custom_type(
        python_type: T.Type,
        cached_custom_types: T.Dict[T.Type, type_info.CustomType]) -> type_info.CustomType:
    """
    Convert a python type to a type_info.CustomType representation we can store in C++.

    OMIT_FROM_SPHINX
    """
    fields_converted: T.List[T.Tuple[str, CodegenType]] = []
    if dataclasses.is_dataclass(python_type):
        # noinspection PyDataclass
        for field in dataclasses.fields(python_type):
            field_type = convert_to_internal_type(
                python_type=field.type, cached_custom_types=cached_custom_types)
            fields_converted.append((field.name, field_type))

        if len(fields_converted) == 0:
            raise TypeError("Dataclass types need to have at least one expression member. " +
                            f"Offending type: {repr(python_type)}")

    return type_info.CustomType(
        name=python_type.__name__, fields=fields_converted, python_type=python_type)


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
    filling its members with the provided expressions. Thus, we construct a symbolic instance
    of the user-provided type.

    Expressions are inserted into fields in the order the fields appear.

    OMIT_FROM_SPHINX
    """
    assert dataclasses.is_dataclass(
        custom_type), f"Provided type `{custom_type}` is not a dataclass"

    constructor_kwargs = dict()
    for field in dataclasses.fields(custom_type):
        if not expressions:
            # This shouldn't ever happen, the C++ side has allocated enough expressions based on
            # the number and type of fields.
            raise RuntimeError("Ran out of expressions while building custom type")

        if issubclass(field.type, sym.Expr):
            constructor_kwargs[field.name] = expressions[0]
            expressions = expressions[1:]
        elif issubclass(field.type, sym.MatrixExpr):
            # Expressions were packed in row-major order as expected by our matrix type.
            mat_rows, mat_cols = _get_matrix_shape(field.type)
            mat = sym.matrix(expressions[:(mat_rows * mat_cols)]).reshape(mat_rows, mat_cols)
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

    OMIT_FROM_SPHINX
    """
    assert dataclasses.is_dataclass(
        instance), f"Provided object of type `{type(instance)}` is not a dataclass"

    expressions: T.List[sym.Expr] = []
    for field in dataclasses.fields(instance):
        value = getattr(instance, field.name)
        if issubclass(field.type, sym.Expr):
            expressions.append(value)
        elif issubclass(field.type, sym.MatrixExpr):
            expressions.extend(value.to_flat_list())
        elif dataclasses.is_dataclass(field.type):
            expressions += map_expressions_out_of_custom_type(instance=value)
        else:
            raise TypeError(
                f'Invalid member type `{field.name}: {field.type}` used in type `{type(instance)}`')

    return expressions
