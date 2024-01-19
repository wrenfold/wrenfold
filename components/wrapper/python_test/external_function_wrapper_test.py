"""
Test ability to create and insert user-specified external functions into the expression graph.
"""
import dataclasses
import unittest

from wrenfold import sym
from wrenfold import external_functions
from wrenfold.type_annotations import Vector2, Vector3
from test_base import MathTestBase


class ExternalFunctionWrapperTest(MathTestBase):

    def test_define_external_function_scalars(self):
        """Define function that accepts and returns scalars."""
        func = external_functions.declare_external_function(
            name="func", arguments=[("x", sym.Expr)], return_type=sym.Expr)
        self.assertEqual(1, func.num_arguments)

        x, y, z = sym.symbols("x, y, z")

        output = func(x + 2)
        self.assertIsInstance(output, sym.Expr)
        self.assertEqual("CompoundExpressionElement", output.type_name)

        # Should work with keyword arguments as well:
        self.assertIdentical(output, func(x=x + 2))

        # We can pass numeric values directly:
        self.assertIdentical(func(sym.integer(2)), func(2))
        self.assertIdentical(func(sym.float(1.132)), func(1.132))

        # Specifying too many or to few args should be an exception...
        self.assertRaises(RuntimeError, lambda: func())
        self.assertRaises(RuntimeError, lambda: func(x, y))
        self.assertRaises(RuntimeError, lambda: func(x, foo=y))
        self.assertRaises(KeyError, lambda: func(y=z))

        # Passing the wrong type should be an exception:
        self.assertRaises(sym.TypeError, lambda: func(x=sym.vector(y, z)))

    def test_define_external_function_matrices(self):
        """Define a function that accepts and returns matrices."""
        func = external_functions.declare_external_function(
            name="func", arguments=[("x", sym.Expr), ("v", Vector3)], return_type=Vector2)
        self.assertEqual(2, func.num_arguments)

        x, y, z = sym.symbols("x, y, z")
        v = sym.vector(y, 2, z - 1)

        output = func(x, v)
        self.assertIsInstance(output, sym.MatrixExpr)
        self.assertEqual((2, 1), output.shape)
        for element in output:
            self.assertEqual("CompoundExpressionElement", element.type_name)

        self.assertRaises(sym.TypeError, lambda: func(sym.vector(z), v=v))
        self.assertRaises(sym.TypeError, lambda: func(x, sym.vector(2, x)))
        self.assertRaises(RuntimeError, lambda: func(x, y, z))
        self.assertRaises(RuntimeError, lambda: func(2, x=v))

    def test_define_external_function_custom_types(self):
        """Define an external function that accepts and returns custom types."""

        @dataclasses.dataclass
        class TestType:
            """A dummy type we use for this test."""
            a: sym.Expr
            b: Vector2

        func = external_functions.declare_external_function(
            name="func", arguments=[("foo", TestType), ("bar", sym.Expr)], return_type=TestType)
        self.assertEqual(2, func.num_arguments)

        x, y = sym.symbols("x, y")
        v = sym.vector(x * y, x - y)

        output = func(TestType(a=2 + x, b=v), bar=sym.pi)

    def test_define_external_function_opaque_types(self):
        """Define an external funtion that accepts and returns opaque types."""


if __name__ == '__main__':
    unittest.main()
