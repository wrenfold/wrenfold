"""
Alias for the pywrenfold `sym` module.
"""

from pywrenfold.sym import *  # noqa: F403

AnyExpression = Expr | BooleanExpr | MatrixExpr | CompoundExpr  # noqa: F405
