"""
Python tests for the wrapper. Most of the algorithmic testing resides in C++/gtest.

These tests are here to make sure the wrapper works.
"""
import unittest

from wrenfold import sym
from test_base import MathTestBase


class ExpressionWrapperTest(MathTestBase):

    def test_create_symbols(self):
        """Test we can create symbolic variables."""
        x = sym.symbols('x')
        y = sym.symbols('y')
        xy = sym.symbols('x, y')
        self.assertIdentical(x, x)
        self.assertNotIdentical(x, y)
        self.assertIdentical(xy[0], x)
        self.assertIdentical(xy[1], y)
        self.assertEqual('Variable', x.type_name)

        # test multiple strings
        result = sym.symbols([('a', 'b'), 'c, d'])
        self.assertIsInstance(result, list)
        self.assertEqual(2, len(result))
        self.assertIdentical(sym.symbols('a'), result[0][0])
        self.assertIdentical(sym.symbols('b'), result[0][1])
        self.assertIdentical(sym.symbols('c'), result[1][0])
        self.assertIdentical(sym.symbols('d'), result[1][1])

    def test_repr(self):
        """Test __repr__ method."""
        x, y, z = sym.symbols('x, y, z')
        self.assertReprEqual('x', x)
        self.assertReprEqual('x * y / 3', x * y / 3)
        self.assertReprEqual('x * z ** 3', z * z * z * x)
        self.assertReprEqual('5 * z / x ** 2', 5.0 * z / (x * x))
        self.assertReprEqual('cos(x) * sin(z)', sym.cos(x) * sym.sin(z))
        self.assertReprEqual('x ** (1 / 2)', sym.sqrt(x))
        self.assertReprEqual('3 ** (1 / 2) * 7 ** (1 / 2) * x ** (1 / 2) * (z ** -1) ** (1 / 2)',
                             sym.sqrt(21 * x / z))
        self.assertReprEqual('atan2(y, x)', sym.atan2(y, x))
        self.assertReprEqual('abs(x)', sym.abs(x))
        self.assertReprEqual('where(x < 0, -x, cos(x))', sym.where(x < 0, -x, sym.cos(x)))
        self.assertReprEqual('Derivative(signum(x), x)', sym.signum(x).diff(x, use_abstract=True))

    def test_bool_conversion(self):
        """Test that only true and false can be converted to bool."""
        x, y, z = sym.symbols('x, y, z')
        self.assertRaises(sym.TypeError, lambda: bool(x + 2))
        self.assertRaises(sym.TypeError, lambda: bool(2.0 / z))
        self.assertRaises(sym.TypeError, lambda: 1 if sym.cos(y * z) + x else 0)
        self.assertTrue(bool(sym.true))
        self.assertFalse(bool(sym.false))
        self.assertEqual(2.0, 2.0 if sym.true else 0.0)

    def test_basic_scalar_operations(self):
        """Test wrappers for addition, multiplication, subtraction, negation."""
        p, q = sym.symbols('p, q')

        self.assertIsInstance(p + p, sym.Expr)
        self.assertIdentical(2 * p, p + p)
        self.assertIdentical(p, p + 0)
        self.assertEqual('Addition', (p + q).type_name)
        self.assertEqual('Multiplication', (p + p).type_name)
        self.assertIdentical(p + p, p + p + p - p)
        self.assertNotIdentical(p + p, p - q)
        self.assertNotIdentical(q - 5, q + 5)

        self.assertIdentical(p * q, q * p)
        self.assertIdentical(sym.pow(q, 2), q * q)
        self.assertEqual('Multiplication', (p * q).type_name)
        self.assertEqual('Power', (q * q).type_name)
        self.assertIdentical(q * q * q / (p * p), sym.pow(q, 3) / sym.pow(p, 2))
        self.assertIdentical(p / q, sym.pow(q / p, -1))
        self.assertNotIdentical(q + 5.0, q + 5)
        self.assertIdentical(2.0 * p, p * 6.0 / 3.0)
        self.assertIdentical(q, -(-q))
        self.assertIdentical(-q, -(-(-q)))

        self.assertIdentical(sym.pow(p, q), p ** q)

    def test_trig_functions(self):
        """Test that we can call trig functions."""
        self.assertIdentical(0, sym.sin(0))
        self.assertIdentical(0, sym.sin(sym.pi))
        self.assertIdentical(1, sym.sin(sym.pi / 2))
        self.assertIdentical(-1, sym.sin(-sym.pi / 2))

        self.assertIdentical(1, sym.cos(0))
        self.assertIdentical(-1, sym.cos(sym.pi))
        self.assertIdentical(0, sym.cos(sym.pi / 2))
        self.assertIdentical(0, sym.cos(-sym.pi / 2))

        self.assertIdentical(0, sym.tan(arg=0))
        self.assertIdentical(sym.zoo, sym.tan(arg=sym.pi / 2))
        self.assertIdentical(sym.zoo, sym.tan(arg=-sym.pi / 2))

        self.assertIdentical(sym.pi / 2, sym.atan2(1, 0))
        self.assertIdentical(sym.pi / 4, sym.atan2(1, 1))

    def test_abs(self):
        """Test calling abs."""
        x = sym.symbols("x")
        self.assertIdentical(3, sym.abs(-3))
        self.assertIdentical(sym.abs(x), sym.abs(sym.abs(x)))

    def test_signum(self):
        """Test calling signum."""
        x, y = sym.symbols("x, y")
        self.assertIdentical(1, sym.signum(3))
        self.assertIdentical(-1, sym.signum(-5.22))
        self.assertIdentical(1, sym.signum(sym.true))
        self.assertNotIdentical(sym.signum(x), sym.signum(y))

    def test_distribute(self):
        """Test calling distribute on scalar expressions."""
        a, b, c, d, e, f = sym.symbols('a, b, c, d, e, f')
        expr = (a + b) * (c + d)
        self.assertIdentical(a * c + a * d + b * c + b * d, expr.distribute())

        expr = (a + b) * (c + d) * (e + f)
        self.assertIdentical(
            a * c * e + a * c * f + a * d * e + a * d * f + b * c * e + b * c * f + b * d * e +
            b * d * f, expr.distribute())

    def test_diff(self):
        """Test calling diff on scalar expressions."""
        x, y, z = sym.symbols('x, y, z')

        # Most of the tests reside in C++, just check that this works on a bunch of expression types:
        self.assertIdentical(1, x.diff(x))
        self.assertIdentical(0, x.diff(var=x, order=2))
        self.assertIdentical(3 * x ** 2, (x ** 3).diff(x))
        self.assertIdentical(x, (x * y).diff(y))
        self.assertIdentical(-y / x ** 2, (y / x).diff(x))
        self.assertIdentical(2 * y / (z * x ** 3), (y / (x * z)).diff(x, order=2))

        self.assertIdentical(z * sym.cos(y * z), sym.sin(y * z).diff(y))
        self.assertIdentical(y * z * sym.cos(y * z) + sym.sin(y * z), (z * sym.sin(y * z)).diff(z))
        self.assertIdentical(1 / sym.cos(x) ** 2, sym.tan(x).diff(x))

        self.assertIdentical(1 / x, sym.log(y * x).diff(x))

        self.assertIdentical(-y / (x ** 2 + y ** 2), sym.atan2(y, x).diff(x))
        self.assertIdentical(2 * x / (x ** 2 + 4 * y ** 2), sym.atan2(2 * y, x).diff(y))

        # Differentiate conditionals:
        self.assertIdentical(
            sym.where(x > 0, 5 * sym.cos(5 * x), 0),
            sym.where(x > 0, sym.sin(5 * x), y + 5).diff(x))

    def test_relational(self):
        """Test creating relational expressions."""
        x, y, z = sym.symbols('x, y, z')
        self.assertIdentical(x < y, y > x)
        self.assertIdentical(z > 0, 0 < z)
        self.assertIdentical(x >= 0.0, 0.0 <= x)
        self.assertIdentical(x == y, y == x)
        self.assertNotIdentical(x == z, x == y)
        self.assertIdentical(sym.true, sym.Expr(1) > 0)
        self.assertIdentical(sym.true, sym.pi > sym.euler)

    def test_conditionals(self):
        """Test creating conditional logic."""
        x, y, z = sym.symbols('x, y, z')
        self.assertIdentical(sym.where(x < y, y, x), sym.max(x, y))
        self.assertIdentical(sym.where(y < x, y, x), sym.min(x, y))
        self.assertIdentical(x, sym.where(0 < sym.Expr(1), x, y))
        self.assertIdentical(y - 3, sym.where(sym.pi >= sym.euler, y - 3, x * 5))

    def test_subs(self):
        """Test calling subs() on expressions."""
        x, y, z = sym.symbols('x, y, z')
        self.assertIdentical(y, x.subs(x, y))
        self.assertIdentical(0, (x - y).subs(y, x))
        self.assertIdentical(1, (x / z).subs(z, x))
        self.assertIdentical(z ** 2 * x, (x ** 3 * y ** 2).subs(x * y, z))
        self.assertIdentical(2 * z, (x + 2 * z - (y * 3) / 2).subs(x - (y * 3) / 2, 0))

    def test_collect(self):
        """Test calling collect() on expressions."""
        x, y, z = sym.symbols('x, y, z')
        self.assertIdentical(x ** 2 * (y + 3), (x ** 2 * y + x ** 2 * 3).collect(x))
        self.assertIdentical(x ** 2 * (sym.log(y) - sym.pi) + x * (sym.cos(y) + sym.sin(y)),
                             (x ** 2 * sym.log(y) - x ** 2 * sym.pi + x * sym.cos(y) +
                              x * sym.sin(y)).collect(x))

        f = x ** 2 * y + x ** 2 * 2 + x * y * 5 + x * (y ** 2) * sym.pi - x * y * sym.log(z)
        self.assertIdentical(x ** 2 * (y + 2) + x * (y ** 2 * sym.pi + y * (5 - sym.log(z))),
                             f.collect([x, y]))


if __name__ == '__main__':
    unittest.main(verbosity=2)
