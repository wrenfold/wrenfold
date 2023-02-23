"""Shared utilities for unit testing."""
import unittest
import typing as T

import mc


class MathTestBase(unittest.TestCase):
    """Base class for unit tests."""

    def assertIdentical(self, a: T.Union[mc.Expr, int, float], b: mc.Expr):
        """Assert that two expressions are identical."""
        if isinstance(a, (int, float)):
            a = mc.Expr(a)

        if a.is_identical_to(b):
            return
        message = (f'The expressions a=`{repr(a)}` b=`{repr(b)}` are not identical.\n' +
                   f'The expression tree for `a` is:\n{a.expression_tree_str()}\n' +
                   f'The expression tree for `b` is:\n{b.expression_tree_str()}')
        raise self.failureException(message)

    def assertNotIdentical(self, a: T.Union[mc.Expr, int, float], b: mc.Expr):
        """Assert that two expressions are not identical."""
        if isinstance(a, (int, float)):
            a = mc.Expr(a)

        if not a.is_identical_to(b):
            return
        message = (
            f'The expressions a=`{repr(a)}` b=`{repr(b)}` are identical (they should not be).\n' +
            f'The expression tree for `a` is:\n{a.expression_tree_str()}\n' +
            f'The expression tree for `b` is:\n{b.expression_tree_str()}')
        raise self.failureException(message)
