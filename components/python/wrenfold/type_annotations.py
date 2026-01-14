"""
Type annotations for decorating function arguments so that the code-generator can instantiate the
correct inputs.

You can define new vector/matrix annotations in your own code by following the pattern below:

>>> from typing import Annotated
>>> from wrenfold import Shape
>>> Matrix2x3 = Annotated[sym.MatrixExpr, Shape(rows=2, cols=3)]
"""

import dataclasses
from typing import Annotated

from . import sym
from .type_info import NumericType

FloatScalar = Annotated[sym.Expr, NumericType.Float, "Denote a floating-point scalar variable."]

IntScalar = Annotated[sym.Expr, NumericType.Integer, "Denote an integer valued scalar variable."]


@dataclasses.dataclass(frozen=True)
class Shape:
    """Used to annotate Matrix arguments to symbolic functions."""

    rows: int
    cols: int


Vector1 = Annotated[sym.MatrixExpr, Shape(rows=1, cols=1), "A 1x1 column vector."]
Vector2 = Annotated[sym.MatrixExpr, Shape(rows=2, cols=1), "A 2x1 column vector."]
Vector3 = Annotated[sym.MatrixExpr, Shape(rows=3, cols=1), "A 3x1 column vector."]
Vector4 = Annotated[sym.MatrixExpr, Shape(rows=4, cols=1), "A 4x1 column vector."]
Vector5 = Annotated[sym.MatrixExpr, Shape(rows=5, cols=1), "A 5x1 column vector."]
Vector6 = Annotated[sym.MatrixExpr, Shape(rows=6, cols=1), "A 6x1 column vector."]
Vector7 = Annotated[sym.MatrixExpr, Shape(rows=7, cols=1), "A 7x1 column vector."]
Vector8 = Annotated[sym.MatrixExpr, Shape(rows=8, cols=1), "A 8x1 column vector."]
Vector9 = Annotated[sym.MatrixExpr, Shape(rows=9, cols=1), "A 9x1 column vector."]

Matrix1 = Annotated[sym.MatrixExpr, Shape(rows=1, cols=1), "A 1x1 square matrix."]
Matrix2 = Annotated[sym.MatrixExpr, Shape(rows=2, cols=2), "A 2x2 square matrix."]
Matrix3 = Annotated[sym.MatrixExpr, Shape(rows=3, cols=3), "A 3x3 square matrix."]
Matrix4 = Annotated[sym.MatrixExpr, Shape(rows=4, cols=4), "A 4x4 square matrix."]
Matrix5 = Annotated[sym.MatrixExpr, Shape(rows=5, cols=5), "A 5x5 square matrix."]
Matrix6 = Annotated[sym.MatrixExpr, Shape(rows=6, cols=6), "A 6x6 square matrix."]
Matrix7 = Annotated[sym.MatrixExpr, Shape(rows=7, cols=7), "A 7x7 square matrix."]
Matrix8 = Annotated[sym.MatrixExpr, Shape(rows=8, cols=8), "A 8x8 square matrix."]
Matrix9 = Annotated[sym.MatrixExpr, Shape(rows=9, cols=9), "A 9x9 square matrix."]


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
