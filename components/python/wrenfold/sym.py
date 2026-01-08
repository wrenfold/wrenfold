"""
Alias for the pywrenfold `sym` module.
"""

import typing

from pywrenfold.sym import *  # noqa: F403

AnyExpression: typing.TypeAlias = Expr | BooleanExpr | MatrixExpr | CompoundExpr  # noqa: F405
