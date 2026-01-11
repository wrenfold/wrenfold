"""
Test ability to create and insert user-specified external functions into the expression graph.
"""

import dataclasses
import unittest

import wrenfold as wf
from wrenfold import exceptions, external_functions, sym, type_info

from .test_base import MathTestBase


class ExternalFunctionWrapperTest(MathTestBase):
    def test_scalars(self):
        """Define function that accepts and returns scalars."""
        func = external_functions.declare_external_function(
            name="func", arguments=[("x", wf.FloatScalar)], return_type=wf.FloatScalar
        )

        self.assertEqual("func", func.name)
        self.assertEqual(1, func.num_arguments)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), func.return_type)

        x, y, z = sym.make_symbols("x", "y", "z")

        output = func(x + 2)
        self.assertIsInstance(output, sym.Expr)
        self.assertEqual("CompoundExpressionElement", output.type_name)
        self.assertEqual("func(2 + x)", repr(output))

        # Should work with keyword arguments as well:
        self.assertIdentical(output, func(x=x + 2))

        # We can pass numeric values directly:
        self.assertIdentical(func(sym.integer(2)), func(2))
        self.assertIdentical(func(sym.float_constant(1.132)), func(1.132))

        # Specifying too many or too few args should be an exception...
        self.assertRaises(RuntimeError, lambda: func())
        self.assertRaises(RuntimeError, lambda: func(x, y))
        self.assertRaises(RuntimeError, lambda: func(x, foo=y))
        self.assertRaises(KeyError, lambda: func(y=z))

        # Passing the wrong type should be an exception:
        self.assertRaises(exceptions.TypeError, lambda: func(x=sym.vector(y, z)))

    def test_matrices(self):
        """Define a function that accepts and returns matrices."""
        func = external_functions.declare_external_function(
            name="func",
            arguments=[("x", wf.FloatScalar), ("v", wf.Vector3)],
            return_type=wf.Vector2,
        )
        self.assertEqual(2, func.num_arguments)
        self.assertEqual(type_info.MatrixType(2, 1), func.return_type)

        x, y, z = sym.make_symbols("x", "y", "z")
        v = sym.vector(y, 2, z - 1)

        output = func(x, v)
        self.assertIsInstance(output, sym.MatrixExpr)
        self.assertEqual((2, 1), output.shape)
        for element in output:
            self.assertEqual("CompoundExpressionElement", element.type_name)

        self.assertRaises(exceptions.TypeError, lambda: func(sym.vector(z), v=v))
        self.assertRaises(exceptions.TypeError, lambda: func(x, sym.vector(2, x)))
        self.assertRaises(RuntimeError, lambda: func(x, y, z))
        self.assertRaises(RuntimeError, lambda: func(2, x=v))

    def test_custom_types(self):
        """Define an external function that accepts and returns custom types."""

        @dataclasses.dataclass
        class TestType:
            """A dummy type we use for this test."""

            a: wf.FloatScalar
            b: wf.Vector2

        func = external_functions.declare_external_function(
            name="func",
            arguments=[("foo", TestType), ("bar", wf.FloatScalar)],
            return_type=TestType,
        )
        self.assertEqual(2, func.num_arguments)
        self.assertIsInstance(func.return_type, type_info.CustomType)

        x, y = sym.make_symbols("x", "y")
        v = sym.vector(x * y, x - y)

        output = func(TestType(a=2 + x, b=v), bar=sym.pi)

        self.assertIsInstance(output, TestType)
        for element in [output.a, output.b[0], output.b[1]]:
            self.assertEqual("CompoundExpressionElement", element.type_name)

        self.assertRaises(exceptions.TypeError, lambda: func(y, x))
        self.assertRaises(TypeError, lambda: func(foo=2, bar=TestType(a=1, b=v)))
        self.assertRaises(RuntimeError, lambda: func(bar=v))

    def test_opaque_types(self):
        """Define an external function that accepts and returns opaque types."""

        class TestType(wf.Opaque):
            """An opaque type for use in this test."""

        # Define two functions - one that returns the type, and another accepts it.
        func_1 = external_functions.declare_external_function(
            name="func_1", arguments=[("x", wf.FloatScalar)], return_type=TestType
        )
        func_2 = external_functions.declare_external_function(
            name="func_2",
            arguments=[("q", TestType), ("w", wf.FloatScalar)],
            return_type=wf.FloatScalar,
        )
        self.assertEqual(1, func_1.num_arguments)
        self.assertIsInstance(func_1.return_type, type_info.CustomType)

        self.assertEqual(2, func_2.num_arguments)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), func_2.return_type)

        x, y = sym.make_symbols("x", "y")
        f = func_1(x=x + 2 / y)

        self.assertIsInstance(f, TestType)
        self.assertIsInstance(f._provenance, sym.CompoundExpr)
        self.assertRaises(exceptions.TypeError, lambda: bool(f._provenance))
        self.assertEqual("ExternalFunctionInvocation", f._provenance.type_name)

        # Pass it to another function:
        g = func_2(w=y * x, q=f)
        self.assertIsInstance(g, sym.Expr)
        self.assertEqual("CompoundExpressionElement", g.type_name)
        self.assertEqual("func_2(func_1(x + 2/y), x*y)", repr(g))

    def test_comparisons(self):
        """Check that we can test external functions for equality."""
        func_1 = external_functions.declare_external_function(
            name="func_1", arguments=[("x", wf.FloatScalar)], return_type=wf.Vector2
        )
        func_1_dup = external_functions.declare_external_function(
            name="func_1", arguments=[("x", wf.FloatScalar)], return_type=wf.Vector2
        )
        self.assertEqual(func_1, func_1_dup)

        func_2 = external_functions.declare_external_function(
            name="func_2", arguments=[("x", wf.FloatScalar)], return_type=wf.Vector2
        )
        self.assertNotEqual(func_1, func_2)

        func_3 = external_functions.declare_external_function(
            name="func_2", arguments=[("x", wf.Vector3)], return_type=wf.Vector2
        )
        self.assertNotEqual(func_1, func_2)
        self.assertNotEqual(func_2, func_3)


if __name__ == "__main__":
    unittest.main()
