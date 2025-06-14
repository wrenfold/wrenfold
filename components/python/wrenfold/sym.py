"""
Alias for the pywrenfold `sym` module.
"""

import typing as T

from pywrenfold.sym import *  # noqa: F403

AnyExpression = T.Union[Expr, BooleanExpr, MatrixExpr, CompoundExpr]  # noqa: F405
