"""
Python tests for the wrapper. Most of the algorithmic testing resides in C++/gtest.

These tests are here to make sure the wrapper works.
"""
import unittest

import mc

from test_base import MathTestBase


class ExpressionWrapperTest(MathTestBase):

    def test_create_symbols(self):
        """Test we can create symbolic variables."""
        x = mc.symbols('x')
        y = mc.symbols('y')
        xy = mc.symbols('x, y')
        self.assertIdentical(x, x)
        self.assertNotIdentical(x, y)
        self.assertIdentical(xy[0], x)
        self.assertIdentical(xy[1], y)
        self.assertEqual('Variable', x.type_name)

        # test multiple strings
        result = mc.symbols([('a', 'b'), 'c, d'])
        self.assertIsInstance(result, list)
        self.assertEqual(2, len(result))
        self.assertIdentical(mc.symbols('a'), result[0][0])
        self.assertIdentical(mc.symbols('b'), result[0][1])
        self.assertIdentical(mc.symbols('c'), result[1][0])
        self.assertIdentical(mc.symbols('d'), result[1][1])

    def test_repr(self):
        """Test __repr__ method."""
        x, y, z = mc.symbols('x, y, z')
        self.assertEqual('x', repr(x))
        self.assertEqual('x * y / 3', repr(x * y / 3))
        self.assertEqual('x * z ** 3', repr(z * z * z * x))
        self.assertEqual('5 * z / x ** 2', repr(5.0 * z / (x * x)))
        self.assertEqual('cos(x) * sin(z)', repr(mc.cos(x) * mc.sin(z)))
        self.assertEqual('x ** (1 / 2)', repr(mc.sqrt(x)))
        self.assertEqual('3 ** (1 / 2) * 7 ** (1 / 2) * x ** (1 / 2) / z ** (1 / 2)',
                         repr(mc.sqrt(21 * x / z)))

    def test_basic_scalar_operations(self):
        """Test wrappers for addition, multiplication, subtraction, negation."""
        p, q = mc.symbols('p, q')

        self.assertIsInstance(p + p, mc.Expr)
        self.assertIdentical(2 * p, p + p)
        self.assertIdentical(p, p + 0)
        self.assertEqual('Addition', (p + q).type_name)
        self.assertEqual('Multiplication', (p + p).type_name)
        self.assertIdentical(p + p, p + p + p - p)
        self.assertNotIdentical(p + p, p - q)
        self.assertNotIdentical(q - 5, q + 5)

        self.assertIdentical(p * q, q * p)
        self.assertIdentical(mc.pow(q, 2), q * q)
        self.assertEqual('Multiplication', (p * q).type_name)
        self.assertEqual('Power', (q * q).type_name)
        self.assertIdentical(q * q * q / (p * p), mc.pow(q, 3) / mc.pow(p, 2))
        self.assertIdentical(p / q, mc.pow(q / p, -1))
        self.assertNotIdentical(q + 5.0, q + 5)
        self.assertIdentical(2.0 * p, p * 6.0 / 3.0)
        self.assertIdentical(q, -(-q))
        self.assertIdentical(-q, -(-(-q)))

        self.assertIdentical(mc.pow(p, q), p ** q)

    def test_trig_functions(self):
        """Test that we can call trig functions."""
        self.assertIdentical(0, mc.sin(0))
        self.assertIdentical(0, mc.sin(mc.pi))
        self.assertIdentical(1, mc.sin(mc.pi / 2))
        self.assertIdentical(-1, mc.sin(-mc.pi / 2))

        self.assertIdentical(1, mc.cos(0))
        self.assertIdentical(-1, mc.cos(mc.pi))
        self.assertIdentical(0, mc.cos(mc.pi / 2))
        self.assertIdentical(0, mc.cos(-mc.pi / 2))

        self.assertIdentical(0, mc.tan(arg=0))
        self.assertIdentical(mc.inf, mc.tan(arg=mc.pi / 2))
        self.assertIdentical(mc.inf, mc.tan(arg=-mc.pi / 2))

    def test_distribute(self):
        """Test calling distribute on scalar expressions."""
        a, b, c, d, e, f = mc.symbols('a, b, c, d, e, f')
        expr = (a + b) * (c + d)
        self.assertIdentical(a * c + a * d + b * c + b * d, expr.distribute())

        expr = (a + b) * (c + d) * (e + f)
        self.assertIdentical(
            a * c * e + a * c * f + a * d * e + a * d * f + b * c * e + b * c * f + b * d * e +
            b * d * f, expr.distribute())

    def test_diff(self):
        """Test calling diff on scalar expressions."""
        x, y, z = mc.symbols('x, y, z')

        # Most of the tests reside in C++, just check that this works on a bunch of expression types:
        self.assertIdentical(1, x.diff(x))
        self.assertIdentical(0, x.diff(var=x, order=2))
        self.assertIdentical(3 * x ** 2, (x ** 3).diff(x))
        self.assertIdentical(x, (x * y).diff(y))
        self.assertIdentical(-y / x ** 2, (y / x).diff(x))
        self.assertIdentical(2 * y / (z * x ** 3), (y / (x * z)).diff(x, order=2))

        self.assertIdentical(z * mc.cos(y * z), mc.sin(y * z).diff(y))
        self.assertIdentical(y * z * mc.cos(y * z) + mc.sin(y * z), (z * mc.sin(y * z)).diff(z))
        self.assertIdentical(1 / mc.cos(x) ** 2, mc.tan(x).diff(x))

        self.assertIdentical(1 / x, mc.log(y * x).diff(x))


if __name__ == '__main__':
    unittest.main(verbosity=2)