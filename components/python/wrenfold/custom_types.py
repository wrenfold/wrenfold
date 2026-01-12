"""
Supporting methods for interfacing with user-provided types.
"""

import dataclasses
import typing

from . import sym, type_annotations, type_info

# Refer to possible types we put in the generated code.
CodegenType: typing.TypeAlias = type_info.ScalarType | type_info.MatrixType | type_info.CustomType

U = typing.TypeVar("U")


def _is_annotated_alias(python_type: typing.Any) -> bool:
    # No way to annotate an annotated alias afaik, so check the origin of the annotation.
    return typing.get_origin(python_type) is typing.Annotated


def convert_to_internal_type(
    python_type: typing.Any,
    cached_custom_types: dict[type, type_info.CustomType],
    context: str,
) -> CodegenType:
    """
    Convert a python type to the internal C++ representation.

    OMIT_FROM_SPHINX
    """
    if _is_annotated_alias(python_type):
        origin_type = python_type.__origin__
        if origin_type is sym.Expr:
            numeric_type = _get_scalar_numeric_type(python_type=python_type)
            if numeric_type == type_info.NumericType.Bool:
                raise TypeError(
                    "Boolean arguments and fields are not supported yet. "
                    "https://github.com/wrenfold/wrenfold/issues/163"
                )
            return type_info.ScalarType(numeric_type=numeric_type)
        elif origin_type is sym.MatrixExpr:
            shape = _get_matrix_shape(python_type=python_type)
            return type_info.MatrixType(rows=shape[0], cols=shape[1])
        else:
            raise TypeError(
                f"Invalid type used in annotation: `{origin_type}`. "
                f"This error was thrown while processing: {context}"
            )
    elif (
        isinstance(python_type, type)
        and dataclasses.is_dataclass(python_type)
        or issubclass(python_type, type_annotations.Opaque)
    ):
        return _maybe_create_custom_type(
            python_type=python_type, cached_custom_types=cached_custom_types
        )
    elif isinstance(python_type, type) and issubclass(python_type, (sym.Expr, sym.MatrixExpr)):
        raise _scalar_matrix_type_err(context=context)
    else:
        raise TypeError(
            f"Invalid type used in annotation: `{python_type}`. "
            f"This error was thrown while processing: {context}"
        )


def _maybe_create_custom_type(
    python_type: type[typing.Any],
    cached_custom_types: dict[type, type_info.CustomType],
) -> type_info.CustomType:
    if python_type in cached_custom_types:
        return cached_custom_types[python_type]
    custom_type = _create_custom_type(
        python_type=python_type, cached_custom_types=cached_custom_types
    )
    cached_custom_types[python_type] = custom_type
    return custom_type


def _create_custom_type(
    python_type: type[typing.Any], cached_custom_types: dict[type, type_info.CustomType]
) -> type_info.CustomType:
    """
    Convert a python type to a type_info.CustomType representation we can store in C++.

    OMIT_FROM_SPHINX
    """
    fields_converted: list[tuple[str, CodegenType]] = []
    if dataclasses.is_dataclass(python_type):
        # noinspection PyDataclass
        for field in dataclasses.fields(python_type):
            field_type = convert_to_internal_type(
                python_type=field.type,
                cached_custom_types=cached_custom_types,
                context=f"Dataclass field `{field.name}` on type `{python_type}`",
            )
            fields_converted.append((field.name, field_type))

        if len(fields_converted) == 0:
            raise TypeError(
                "Dataclass types need to have at least one expression member. "
                + f"Offending type: {repr(python_type)}"
            )

    return type_info.CustomType(
        name=python_type.__name__, fields=fields_converted, python_type=python_type
    )


def _get_scalar_numeric_type(python_type: type[typing.Any]) -> type_info.NumericType:
    numeric_types = [x for x in python_type.__metadata__ if isinstance(x, type_info.NumericType)]
    if len(numeric_types) != 1:
        raise TypeError(
            "Type annotations should have exactly one instance "
            "of `type_info.NumericType` in their metadata. "
            f"Provided type `{python_type}` specifies metadata: {python_type.__metadata__}"
        )
    return numeric_types[0]


def _get_matrix_shape(python_type: type[typing.Any]) -> tuple[int, int]:
    """
    Get the SHAPE field off of a matrix type annotation. Check that it has the correct type.
    """
    shapes = [x for x in python_type.__metadata__ if isinstance(x, type_annotations.Shape)]
    if len(shapes) != 1:
        raise TypeError(
            "Type annotations should have exactly one instance "
            "of `type_annotations.Shape` in their metadata. "
            f"Provided type `{python_type}` specifies metadata: {python_type.__metadata__}"
        )
    return (shapes[0].rows, shapes[0].cols)


def map_expressions_into_custom_type(
    expressions: list[sym.Expr], custom_type: type_info.CustomType
) -> tuple[typing.Any, list[sym.Expr]]:
    """
    Given a flat list of expressions, construct an instance of the python type represented by
    `custom_type`. We descend through the type recursively,  filling its members with the provided
    expressions. Expressions are inserted into fields in the order the fields appear.

    OMIT_FROM_SPHINX
    """
    python_type = custom_type.python_type
    assert python_type is not None and dataclasses.is_dataclass(python_type), (
        f"Provided type `{custom_type}` is not a dataclass"
    )

    constructor_kwargs = dict()
    for field in custom_type.fields:
        if not expressions:
            # This shouldn't ever happen, the C++ side has allocated enough expressions based on
            # the number and type of fields.
            raise RuntimeError("Ran out of expressions while building custom type")
        if isinstance(field.type, type_info.ScalarType):
            constructor_kwargs[field.name] = expressions[0]
            expressions = expressions[1:]
        elif isinstance(field.type, type_info.MatrixType):
            rows, cols = field.type.shape
            mat = sym.matrix(expressions[: (rows * cols)]).reshape(rows, cols)
            constructor_kwargs[field.name] = mat
            expressions = expressions[mat.size :]
        else:
            constructor_kwargs[field.name], expressions = map_expressions_into_custom_type(
                expressions=expressions, custom_type=field.type
            )

    # All the constructor arguments have been assembled.
    return python_type(**constructor_kwargs), expressions


def map_expressions_out_of_custom_type(
    instance: typing.Any, custom_type: type_info.CustomType
) -> list[sym.Expr]:
    """
    Given an instance of a custom dataclass type, flatten its contents into a list of expressions.

    This is effectively the inverse of `map_expressions_into_custom_type`.

    OMIT_FROM_SPHINX
    """
    if not dataclasses.is_dataclass(instance):
        raise TypeError(
            f"Expected an instance of type `{custom_type.name}` (which should be a dataclass) "
            f"but found `{type(instance)}`."
        )

    expressions: list[sym.Expr] = []
    for field in custom_type.fields:
        value = getattr(instance, field.name)
        if value is None:
            raise AttributeError(
                f"Missing attribute `{field.name}` (with expected type `{field.type}`) on instance "
                f"of `{custom_type.name}`."
            )
        if isinstance(field.type, type_info.ScalarType):
            if not isinstance(value, sym.Expr):
                raise TypeError(
                    f"Expected field `{field.name}` on type `{custom_type.name}` to be an "
                    f"instance of sym.Expr, but found `{type(value)}`."
                )
            expressions.append(value)
        elif isinstance(field.type, type_info.MatrixType):
            if not isinstance(value, sym.MatrixExpr):
                raise TypeError(
                    f"Expected field `{field.name}` on type `{custom_type.name}` to be an "
                    f"instance of sym.MatrixExpr, but found `{type(value)}`."
                )
            expressions.extend(value.to_flat_list())
        else:
            expressions += map_expressions_out_of_custom_type(value, custom_type=field.type)

    return expressions


def _scalar_matrix_type_err(context: str) -> TypeError:
    return TypeError(
        "Scalar and matrix arguments must be annotated (using typing.Annotated) with "
        "metadata about their shape and type. For example, use wf.FloatScalar to denote "
        "floating-point scalar arguments, or wf.Vector3 to denote a 3D vector of "
        "floating-point values. See wrenfold.type_annotations for examples. "
        f"This error was thrown while processing: {context}"
    )
