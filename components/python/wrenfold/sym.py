"""Import the wrapper functions into this namespace."""
import typing as T

from pywrenfold.wf_wrapper import *

AnyExpression = T.Union[Expr, BooleanExpr, MatrixExpr, CompoundExpr]
