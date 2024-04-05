"""Alias for the main pywrenfold module."""
import typing as T

from pywrenfold import *

AnyExpression = T.Union[Expr, BooleanExpr, MatrixExpr, CompoundExpr]
