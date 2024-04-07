"""
Type annotations for decorating function arguments so that the code-generator can instantiate the
correct inputs.

You can define new vector/matrix annotations anywhere. They need only inherit from MatrixExpr and
expose the SHAPE tuple.
"""
import typing as T

from . import sym


class RealScalar(sym.Expr):
    """Denote a floating-point scalar variable."""


class Vector1(sym.MatrixExpr):
    """A 1x1 matrix."""
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
