"""
Type annotations for decorating function arguments so that the code-generator
can instantiate the correct inputs.
"""

from .sym import (Expr, MatrixExpr)


class RealScalar(Expr):
    """Denote a floating-point scalar variable."""


class Vector2(MatrixExpr):
    SHAPE = (2, 1)