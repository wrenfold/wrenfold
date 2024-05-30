"""
Type annotations for decorating function arguments so that the code-generator can instantiate the
correct inputs.

You can define new vector/matrix annotations anywhere. They need only inherit from MatrixExpr and
expose the SHAPE tuple.
"""
import typing as T

from . import sym


class FloatScalar(sym.Expr):
    """Denote a floating-point scalar variable."""


class Vector1(sym.MatrixExpr):
    """A 1x1 column vector."""
    SHAPE = (1, 1)


class Vector2(sym.MatrixExpr):
    """A 2x1 column vector."""
    SHAPE = (2, 1)


class Vector3(sym.MatrixExpr):
    """A 3x1 column vector."""
    SHAPE = (3, 1)


class Vector4(sym.MatrixExpr):
    """A 4x1 column vector."""
    SHAPE = (4, 1)


class Vector5(sym.MatrixExpr):
    """A 5x1 column vector."""
    SHAPE = (5, 1)


class Vector6(sym.MatrixExpr):
    """A 6x1 column vector."""
    SHAPE = (6, 1)


class Vector7(sym.MatrixExpr):
    """A 7x1 column vector."""
    SHAPE = (7, 1)


class Vector8(sym.MatrixExpr):
    """A 8x1 column vector."""
    SHAPE = (8, 1)


class Vector9(sym.MatrixExpr):
    """A 9x1 column vector."""
    SHAPE = (9, 1)


class Matrix1(sym.MatrixExpr):
    """A 1x1 square matrix."""
    SHAPE = (1, 1)


class Matrix2(sym.MatrixExpr):
    """A 2x2 square matrix."""
    SHAPE = (2, 2)


class Matrix3(sym.MatrixExpr):
    """A 3x3 square matrix."""
    SHAPE = (3, 3)


class Matrix4(sym.MatrixExpr):
    """A 4x4 square matrix."""
    SHAPE = (4, 4)


class Matrix5(sym.MatrixExpr):
    """A 5x5 square matrix."""
    SHAPE = (5, 5)


class Matrix6(sym.MatrixExpr):
    """A 6x6 square matrix."""
    SHAPE = (6, 6)


class Matrix7(sym.MatrixExpr):
    """A 7x7 square matrix."""
    SHAPE = (7, 7)


class Matrix8(sym.MatrixExpr):
    """A 8x8 square matrix."""
    SHAPE = (8, 8)


class Matrix9(sym.MatrixExpr):
    """A 9x9 square matrix."""
    SHAPE = (9, 9)


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

    def __init__(self, provenance: T.Optional[sym.CompoundExpr] = None) -> None:
        self._provenance: T.Optional[sym.CompoundExpr] = provenance
