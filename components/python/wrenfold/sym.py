"""Alias for the main pywrenfold module."""
import typing as T

from pywrenfold.wf_wrapper import *

AnyExpression = T.Union[Expr, BooleanExpr, MatrixExpr, CompoundExpr]
