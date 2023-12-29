"""
Type annotations for decorating function arguments so that the code-generator
can instantiate the correct inputs.

You can define new vector/matrix annotations anywhere. They need only inherit from MatrixExpr and
expose the SHAPE tuple.
"""

from .sym import (Expr, MatrixExpr)


class RealScalar(Expr):
    """Denote a floating-point scalar variable."""


class Vector2(MatrixExpr):
    SHAPE = (2, 1)


class Vector3(MatrixExpr):
    SHAPE = (3, 1)


class Vector4(MatrixExpr):
    SHAPE = (4, 1)


class Vector5(MatrixExpr):
    SHAPE = (5, 1)


class Vector6(MatrixExpr):
    SHAPE = (6, 1)
