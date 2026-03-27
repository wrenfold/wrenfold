"""
Type annotations for decorating function arguments so that the code-generator can instantiate the
correct inputs.

You can define new vector/matrix annotations in your own code by following the pattern below:

>>> from typing import Annotated
>>> from wrenfold import Shape
>>> Matrix11x12 = typing.Annotated[sym.MatrixExpr, Shape(rows=11, cols=12)]
"""

import dataclasses
import typing

from . import manifolds, sym
from .type_info import NumericType

FloatScalar = typing.Annotated[
    sym.Expr, NumericType.Float, "Denote a floating-point scalar variable."
]

IntScalar = typing.Annotated[
    sym.Expr, NumericType.Integer, "Denote an integer valued scalar variable."
]


@dataclasses.dataclass(frozen=True)
class Shape:
    """Used to annotate Matrix arguments to symbolic functions."""

    rows: int
    cols: int


@dataclasses.dataclass
class Jacobian:
    """
    Annotate an input argument to indicate that a Jacobian should be computed with
    respect to those input variables.

    Attributes:
      of: Indicate the output that the Jacobian should be computed on.
      manifold: Manifold used to differentiate the argument.
      optional: If true, the output argument will be an optional output.
    """

    of: str | None = None
    manifold: manifolds.Manifold | None = None
    optional: bool = True


Vector1 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=1), "A 1x1 column vector."]
Vector2 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=1), "A 2x1 column vector."]
Vector3 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=1), "A 3x1 column vector."]
Vector4 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=1), "A 4x1 column vector."]
Vector5 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=1), "A 5x1 column vector."]
Vector6 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=1), "A 6x1 column vector."]
Vector7 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=1), "A 7x1 column vector."]
Vector8 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=1), "A 8x1 column vector."]
Vector9 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=1), "A 9x1 column vector."]

Matrix1 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=1), "A 1x1 square matrix."]
Matrix2 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=2), "A 2x2 square matrix."]
Matrix3 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=3), "A 3x3 square matrix."]
Matrix4 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=4), "A 4x4 square matrix."]
Matrix5 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=5), "A 5x5 square matrix."]
Matrix6 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=6), "A 6x6 square matrix."]
Matrix7 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=7), "A 7x7 square matrix."]
Matrix8 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=8), "A 8x8 square matrix."]
Matrix9 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=9), "A 9x9 square matrix."]

Matrix1x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=2), "A 1x2 matrix."]
Matrix1x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=3), "A 1x3 matrix."]
Matrix1x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=4), "A 1x4 matrix."]
Matrix1x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=5), "A 1x5 matrix."]
Matrix1x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=6), "A 1x6 matrix."]
Matrix1x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=7), "A 1x7 matrix."]
Matrix1x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=8), "A 1x8 matrix."]
Matrix1x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=1, cols=9), "A 1x9 matrix."]
Matrix2x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=3), "A 2x3 matrix."]
Matrix2x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=4), "A 2x4 matrix."]
Matrix2x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=5), "A 2x5 matrix."]
Matrix2x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=6), "A 2x6 matrix."]
Matrix2x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=7), "A 2x7 matrix."]
Matrix2x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=8), "A 2x8 matrix."]
Matrix2x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=2, cols=9), "A 2x9 matrix."]
Matrix3x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=2), "A 3x2 matrix."]
Matrix3x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=4), "A 3x4 matrix."]
Matrix3x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=5), "A 3x5 matrix."]
Matrix3x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=6), "A 3x6 matrix."]
Matrix3x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=7), "A 3x7 matrix."]
Matrix3x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=8), "A 3x8 matrix."]
Matrix3x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=3, cols=9), "A 3x9 matrix."]
Matrix4x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=2), "A 4x2 matrix."]
Matrix4x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=3), "A 4x3 matrix."]
Matrix4x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=5), "A 4x5 matrix."]
Matrix4x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=6), "A 4x6 matrix."]
Matrix4x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=7), "A 4x7 matrix."]
Matrix4x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=8), "A 4x8 matrix."]
Matrix4x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=4, cols=9), "A 4x9 matrix."]
Matrix5x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=2), "A 5x2 matrix."]
Matrix5x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=3), "A 5x3 matrix."]
Matrix5x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=4), "A 5x4 matrix."]
Matrix5x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=6), "A 5x6 matrix."]
Matrix5x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=7), "A 5x7 matrix."]
Matrix5x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=8), "A 5x8 matrix."]
Matrix5x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=5, cols=9), "A 5x9 matrix."]
Matrix6x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=2), "A 6x2 matrix."]
Matrix6x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=3), "A 6x3 matrix."]
Matrix6x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=4), "A 6x4 matrix."]
Matrix6x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=5), "A 6x5 matrix."]
Matrix6x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=7), "A 6x7 matrix."]
Matrix6x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=8), "A 6x8 matrix."]
Matrix6x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=6, cols=9), "A 6x9 matrix."]
Matrix7x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=2), "A 7x2 matrix."]
Matrix7x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=3), "A 7x3 matrix."]
Matrix7x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=4), "A 7x4 matrix."]
Matrix7x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=5), "A 7x5 matrix."]
Matrix7x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=6), "A 7x6 matrix."]
Matrix7x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=8), "A 7x8 matrix."]
Matrix7x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=7, cols=9), "A 7x9 matrix."]
Matrix8x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=2), "A 8x2 matrix."]
Matrix8x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=3), "A 8x3 matrix."]
Matrix8x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=4), "A 8x4 matrix."]
Matrix8x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=5), "A 8x5 matrix."]
Matrix8x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=6), "A 8x6 matrix."]
Matrix8x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=7), "A 8x7 matrix."]
Matrix8x9 = typing.Annotated[sym.MatrixExpr, Shape(rows=8, cols=9), "A 8x9 matrix."]
Matrix9x2 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=2), "A 9x2 matrix."]
Matrix9x3 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=3), "A 9x3 matrix."]
Matrix9x4 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=4), "A 9x4 matrix."]
Matrix9x5 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=5), "A 9x5 matrix."]
Matrix9x6 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=6), "A 9x6 matrix."]
Matrix9x7 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=7), "A 9x7 matrix."]
Matrix9x8 = typing.Annotated[sym.MatrixExpr, Shape(rows=9, cols=8), "A 9x8 matrix."]


class Opaque:
    """
    Base class used to indicate a custom type that exposes no symbolic expression members. Opaque
    types are employed to represent user-provided types the user may wish to pass to their generated
    functions (and subsequently to external functions).

    Caution:
      You should not construct ``Opaque`` directly. Instead, inherit from it to create a new type.
      This new type is then intended for use as a type annotation on functions passed to
      :func:`wrenfold.code_generation.create_function_description`.
    """

    def __init__(self, provenance: sym.CompoundExpr | None = None) -> None:
        self._provenance: sym.CompoundExpr | None = provenance
