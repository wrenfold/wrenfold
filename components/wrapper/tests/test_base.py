"""Shared utilities for unit testing."""

import sys
import typing as T
import unittest

from wrenfold import sym


class MathTestBase(unittest.TestCase):
    """Base class for unit tests."""

    def setUp(self):
        if sys.platform == "win32":
            sys.stdout.reconfigure(encoding="utf-8")
            sys.stderr.reconfigure(encoding="utf-8")

    def assertIdentical(self, a: T.Union[sym.AnyExpression, int, float], b: sym.AnyExpression):
        """Assert that two expressions are identical."""
        if isinstance(a, (int, float)):
            a = sym.Expr(a)

        if a.is_identical_to(b):
            return
        message = (
            f"The expressions a:\n{repr(a)}\nb:\n{repr(b)}\nare not identical.\n"
            + f"The expression tree for `a` is:\n{a.expression_tree_str()}\n"
            + f"The expression tree for `b` is:\n{b.expression_tree_str()}"
        )
        raise self.failureException(message)

    def assertNotIdentical(self, a: T.Union[sym.AnyExpression, int, float], b: sym.AnyExpression):
        """Assert that two expressions are not identical."""
        if isinstance(a, (int, float)):
            a = sym.Expr(a)

        if not a.is_identical_to(b):
            return
        message = (
            f"The expressions a:\n{repr(a)}\nb:\n{repr(b)}\n are identical (they should not be).\n"
            + f"The expression tree for `a` is:\n{a.expression_tree_str()}\n"
            + f"The expression tree for `b` is:\n{b.expression_tree_str()}"
        )
        raise self.failureException(message)

    def assertReprEqual(self, a: str, b: sym.AnyExpression):
        """Assert that the repr of `b` matches string `a`."""
        self.assertEqual(a, repr(b))
