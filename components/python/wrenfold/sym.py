"""
Alias for the pywrenfold `sym` module.
"""
import typing as T

from pywrenfold.sym import *

AnyExpression = T.Union[Expr, BooleanExpr, MatrixExpr, CompoundExpr]
