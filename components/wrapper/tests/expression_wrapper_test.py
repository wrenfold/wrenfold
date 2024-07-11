"""
Python tests for the wrapper itself. Most of the algorithmic testing resides in C++/gtest.

These tests are here to make sure that wrapped methods behave as expected, and that we
don't accidentally remove any.
"""
import unittest

from test_base import MathTestBase

from wrenfold import exceptions, sym


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
        self.assertEqual((), x.args)

        # test multiple strings
        result = sym.symbols([('a', 'b'), 'c, d'])
        self.assertIsInstance(result, list)
        self.assertEqual(2, len(result))
        self.assertIdentical(sym.symbols('a'), result[0][0])
        self.assertIdentical(sym.symbols('b'), result[0][1])
        self.assertIdentical(sym.symbols('c'), result[1][0])
        self.assertIdentical(sym.symbols('d'), result[1][1])

        # apply assumptions:
        self.assertNotIdentical(x, sym.symbols("x", real=True))
        self.assertNotIdentical(
            sym.symbols("x", real=True, nonnegative=True),
            sym.symbols("x", real=True, positive=True))

        self.assertRaises(exceptions.InvalidArgumentError,
                          lambda: sym.symbols("z", real=True, complex=True))

    def test_create_unique_symbols(self):
        """Test calling unique_symbols."""
        a = sym.unique_symbols(count=1)
        b, c = sym.unique_symbols(count=2)
        self.assertNotIdentical(a, b)
        self.assertNotIdentical(b, c)
        self.assertEqual('Variable', a.type_name)
        self.assertEqual((), a.args)

    def test_is_identical_to(self):
        """Test calling is_identical_to and __eq__."""
        x, y = sym.symbols("x, y")
        self.assertTrue(x.is_identical_to(x))
        self.assertFalse(x.is_identical_to(y))
        # __eq__ operator is just an alias of is_identical_to
        self.assertTrue(x == x)
        self.assertFalse(x == y)
        self.assertTrue(sym.integer(5) == 5)
        self.assertFalse(sym.integer(5) == y * x)
        self.assertTrue(sym.float(1.0) == 1.0)
        self.assertFalse(sym.float(0.1) == 3.14)

    def test_hash(self):
        """Test we can call __hash__ on scalar expressions."""
        x, y = sym.symbols("x, y")
        self.assertEqual(hash(x), hash(x))
        self.assertNotEqual(hash(x), hash(y))  # Not impossible, but very unlikely.
        # use expressions as keys in a dict:
        storage = {x: 10, y: -13}
        self.assertEqual(10, storage[x])
        self.assertEqual(-13, storage[y])

    def test_numeric_constructors(self):
        """Test explicit construction of Expr from numeric types."""
        self.assertIdentical(sym.zero, sym.integer(0))
        self.assertIdentical(sym.one, sym.integer(1))
        self.assertIdentical(-42, sym.integer(-42))
        self.assertIdentical(1.231, sym.float(1.231))
        self.assertIdentical(9.81, sym.float(9.81))
        self.assertIdentical(sym.nan, sym.float(float("nan")))
        self.assertIdentical(sym.zoo, sym.float(float("inf")))
        self.assertIdentical(sym.one / 2, sym.rational(1, 2))
        self.assertIdentical(sym.integer(-5) / 7, sym.rational(-5, 7))
        self.assertIdentical(sym.integer(1) / 3, sym.rational(2, 6))
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.rational(1, 0))
        self.assertEqual((), sym.integer(7).args)
        self.assertEqual((), sym.float(-0.862).args)
        self.assertEqual((), sym.rational(7, 9).args)

        # Cannot invoke with values that exceed range of 64-bit signed int.
        # Python stores these values internally, but we can't convert them for now:
        self.assertRaises(TypeError, lambda: sym.integer(9223372036854775807 + 1))
        self.assertRaises(TypeError, lambda: sym.integer(-9223372036854775808 - 1))

        # Disallow constructing BooleanExpr from numeric literals:
        self.assertRaises(TypeError, lambda: sym.BooleanExpr(1))
        self.assertRaises(TypeError, lambda: sym.BooleanExpr(-0.231))
        self.assertRaises(TypeError, lambda: sym.BooleanExpr(True))
        self.assertRaises(TypeError, lambda: sym.BooleanExpr(False))

    def test_repr(self):
        """Test __repr__ method."""
        x, y, z = sym.symbols('x, y, z')
        self.assertReprEqual('x', x)
        self.assertReprEqual('x*y/3', x * y / 3)
        self.assertReprEqual('3*x + 2*y/5', 3 * x + 2 * y / 5)
        self.assertReprEqual('x*z**3', z * z * z * x)
        self.assertReprEqual('5*z/x**2', 5.0 * z / (x * x))
        self.assertReprEqual('cos(x)*sin(z)', sym.cos(x) * sym.sin(z))
        self.assertReprEqual('cosh(x)*sinh(z)', sym.cosh(x) * sym.sinh(z))
        self.assertReprEqual('2 + tanh(z)', 2 + sym.tanh(z))
        self.assertReprEqual('x**(1/2)', sym.sqrt(x))
        self.assertReprEqual('21**(1/2)*(x/z)**(1/2)', sym.sqrt(21 * x / z))
        self.assertReprEqual('atan2(y, x)', sym.atan2(y, x))
        self.assertReprEqual('abs(x)', sym.abs(x))
        self.assertReprEqual('where(x < 0, -x, cos(x))', sym.where(x < 0, -x, sym.cos(x)))
        self.assertReprEqual('Derivative(sign(x), x)', sym.sign(x).diff(x, use_abstract=True))
        self.assertReprEqual('x == z', sym.eq(x, z))
        self.assertReprEqual('iverson(z < x)', sym.iverson(x > z))
        self.assertReprEqual('I', sym.I)
        self.assertReprEqual('E', sym.E)
        self.assertReprEqual('2 + 5*I', 2 + 5 * sym.I)

    def test_bool_conversion(self):
        """Test that only true and false can be converted to bool."""
        x, y, z = sym.symbols('x, y, z')
        self.assertRaises(exceptions.TypeError, lambda: bool(x + 2))
        self.assertRaises(exceptions.TypeError, lambda: bool(2.0 / z))
        self.assertRaises(exceptions.TypeError, lambda: 1 if sym.cos(y * z) + x else 0)
        self.assertRaises(exceptions.TypeError, lambda: bool(x < y))
        self.assertTrue(bool(sym.true))
        self.assertFalse(bool(sym.false))
        self.assertEqual(2.0, 2.0 if sym.true else 0.0)
        self.assertTrue(0 < sym.integer(5))
        self.assertFalse(22 == sym.pi)

    def test_basic_scalar_operations(self):
        """Test wrappers for addition, multiplication, subtraction, negation, and power."""
        p, q = sym.symbols('p, q')

        self.assertIsInstance(p + p, sym.Expr)
        self.assertIdentical(2 * p, p + p)
        self.assertIdentical(p, p + 0)
        self.assertEqual('Addition', (p + q).type_name)
        self.assertEqual((p, q), (p + q).args)
        self.assertIdentical(p + p, p + p + p - p)
        self.assertNotIdentical(q + 5.0, q + 5)
        self.assertNotIdentical(p + p, p - q)
        self.assertNotIdentical(q - 5, q + 5)

        self.assertIdentical(p * q, q * p)
        self.assertEqual('Multiplication', (p * q).type_name)
        self.assertEqual((p, q), (p * q).args)
        self.assertIdentical(q * q * q / (p * p), sym.pow(q, 3) / sym.pow(p, 2))
        self.assertIdentical(2.0 * p, (p * 6.0) / 3.0)
        self.assertIdentical(q, -(-q))
        self.assertIdentical(-q, -(-(-q)))

        self.assertEqual('Power', (q * q).type_name)
        self.assertIdentical(sym.pow(q, 2), q * q)
        self.assertIdentical(sym.pow(p, q), p ** q)
        self.assertIdentical(sym.pow(2, q), 2 ** q)
        self.assertIdentical(sym.pow(p, 5), p ** 5)
        self.assertIdentical(sym.pow(p, 0.231), p ** 0.231)
        self.assertIdentical(p / q, sym.pow(q / p, -1))
        self.assertEqual((p, q), (p ** q).args)

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

    def test_hyperbolic_trig_functions(self):
        """Test that we can call hyperbolic trig functions."""
        x = sym.symbols("x")
        self.assertIdentical(sym.cos(x), sym.cosh(x * sym.I))
        self.assertIdentical(sym.sin(x) * sym.I, sym.sinh(x * sym.I))
        self.assertIdentical(sym.tan(x) * sym.I, sym.tanh(x * sym.I))
        self.assertIdentical(x, sym.cosh(sym.acosh(x)))
        self.assertIdentical(x, sym.sinh(sym.asinh(x)))
        self.assertIdentical(x, sym.tanh(sym.atanh(x)))

    def test_abs(self):
        """Test calling abs."""
        x = sym.symbols("x")
        self.assertIdentical(3, sym.abs(-3))
        self.assertIdentical(sym.abs(x), sym.abs(sym.abs(x)))
        self.assertIdentical(sym.abs(x), abs(x))
        self.assertEqual((x,), sym.abs(x).args)

    def test_signum(self):
        """Test calling signum."""
        x, y = sym.symbols("x, y")
        self.assertIdentical(1, sym.sign(3))
        self.assertIdentical(-1, sym.sign(-5.22))
        self.assertNotIdentical(sym.sign(x), sym.sign(y))
        self.assertRaises(TypeError, lambda: sym.sign(sym.true))

    def test_floor(self):
        """Test calling floor."""
        x = sym.symbols("x")
        self.assertIdentical(0, sym.floor(0))
        self.assertIdentical(7, sym.floor(7.12312))
        self.assertIdentical(-3, sym.floor(-sym.integer(5) / 2))
        self.assertIdentical(sym.floor(x), sym.floor(sym.floor(x)))

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

        # Diffing a polynomial too many times can trigger arithmetic error (factorial overflow)
        self.assertRaises(exceptions.ArithmeticError, lambda: (x ** 100).diff(x, 20))

    def test_relational(self):
        """Test creating relational expressions."""
        x, y, z = sym.symbols('x, y, z')
        self.assertIdentical(x < y, y > x)
        self.assertIdentical(z > 0, 0 < z)
        self.assertIdentical(x >= 0.0, 0.0 <= x)
        self.assertIdentical(sym.true, sym.Expr(1) > 0)
        self.assertIdentical(sym.true, sym.pi > sym.E)
        self.assertEqual((x, y), (x < y).args)
        # We use sym.eq to make == relationals:
        self.assertIdentical(sym.eq(x, 1), sym.eq(1, x))
        self.assertNotIdentical(sym.eq(x, y), sym.eq(2, x * y))
        self.assertEqual((y, x * 2), sym.eq(x * 2, y).args)

    def test_conditionals(self):
        """Test creating conditional logic."""
        x, y = sym.symbols('x, y')
        self.assertIdentical(sym.where(x < y, y, x), sym.max(x, y))
        self.assertIdentical(sym.where(y < x, y, x), sym.min(x, y))
        self.assertIdentical(x, sym.where(0 < sym.Expr(1), x, y))
        self.assertIdentical(y - 3, sym.where(sym.pi >= sym.E, y - 3, x * 5))
        self.assertEqual((x < y, sym.cos(x), y), sym.where(x < y, sym.cos(x), y).args)

    def test_iverson(self):
        """Test converting boolean values to integer."""
        x, y = sym.symbols('x, y')
        self.assertIdentical(1, sym.iverson(sym.one < 10.2))
        self.assertIdentical(0, sym.iverson(sym.eq(sym.one, sym.zero)))
        self.assertEqual((x < y,), sym.iverson(x < y).args)

    def test_unevaluated(self):
        """Test calling `unevaluated`."""
        x, y = sym.symbols('x, y')
        self.assertIdentical(sym.unevaluated(x), sym.unevaluated(x))
        self.assertIdentical(sym.unevaluated(x), sym.unevaluated(sym.unevaluated(x)))

    def test_subs(self):
        """Test calling subs() on expressions."""
        x, y, z = sym.symbols('x, y, z')
        self.assertIdentical(x, x.subs(x, x))
        self.assertIdentical(y, x.subs(x, y))
        self.assertIdentical(2.5, x.subs(x, 2.5))
        self.assertIdentical(0, (x - y).subs(y, x))
        self.assertIdentical(1, (x / z).subs(z, x))
        self.assertIdentical(z ** 2 * x, (x ** 3 * y ** 2).subs(x * y, z))
        self.assertIdentical(2 * z, (x + 2 * z - (y * 3) / 2).subs(x - (y * 3) / 2, 0))
        self.assertIdentical(sym.cos(y + 1), sym.cos(x).subs(x, y + 1))
        self.assertIdentical(z * (sym.cos(x * 2) + 3) + sym.log(sym.cos(x * 2)),
                             (z * x + sym.log(x - 3)).subs(x,
                                                           sym.cos(x * 2) + 3))
        self.assertIdentical(
            sym.sin(z - 3) * sym.abs(z), (sym.sin(y - 3) * x).subs([(y, z), (x, sym.abs(z))]))
        # Boolean substitution:
        f = sym.where(x > y, sym.cos(x), sym.abs(y))
        self.assertIdentical(sym.cos(x), f.subs(x > y, sym.true))
        self.assertIdentical(sym.abs(y), f.subs(x > y, sym.false))

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

    def test_eval(self):
        """Test calling eval() on an expression."""
        x, y = sym.symbols('x, y')
        self.assertIdentical(x, x.eval())
        self.assertIdentical(x + 2.0, (x + y).subs(y, 2).eval())
        self.assertEqual(5.3, sym.float(5.3).eval())
        self.assertAlmostEqual(
            4.3 / 2.71582, (x / y).subs(x, 4.3).subs(y, 2.71582).eval(), places=15)
        self.assertEqual(3923, sym.integer(3923).eval())
        self.assertEqual(123, (sym.integer(100) + x).subs(x, 23).eval())
        self.assertEqual(complex(1, 2), (1 + sym.I * 2).eval())
        self.assertEqual(complex(0.23, 0.5), (x + sym.I * y).subs(x, 0.23).subs(y, 0.5).eval())

    def test_cse(self):
        """Test calling eliminate_subexpressions()."""
        x, y = sym.symbols('x, y')

        def var(idx: int, letter: str = 'v'):
            return sym.symbols(f'{letter}{idx}')

        f1 = sym.cos(x) * sym.cos(x) + y * sym.cos(x)
        f1_cse, replacements = sym.eliminate_subexpressions(f1, None, 2)

        v0 = var(0)
        self.assertIdentical(v0 ** 2 + y * v0, f1_cse)
        self.assertEqual(1, len(replacements))
        self.assertSequenceEqual([(v0, sym.cos(x))], replacements)

        f2 = sym.abs(y * 2) + sym.cos(y * 2) / x - 5 / x
        f2_cse, replacements = sym.eliminate_subexpressions(f2, lambda idx: var(idx, 'u'))

        u0, u1 = var(0, 'u'), var(1, 'u')
        self.assertIdentical(sym.abs(u0) + sym.cos(u0) * u1 - 5 * u1, f2_cse)
        self.assertEqual(2, len(replacements))
        self.assertSequenceEqual([(u0, y * 2), (u1, 1 / x)], replacements)

    def test_integer_exceptions(self):
        """
        Test that arithmetic exceptions propagate into python.

        Actual logic tested in checked_int_test.cc, here we just test that exceptions are passed into python correctly.
        """
        i64_max = 9223372036854775807
        i64_min = -9223372036854775808
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.integer(i64_max) * 2)
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.integer(i64_min) * 2)
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.integer(i64_min) * -1)
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.integer(i64_min) / -1)
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.integer(i64_max) + 1)
        self.assertRaises(exceptions.ArithmeticError, lambda: sym.integer(i64_min) - 1)
        self.assertRaises(exceptions.ArithmeticError, lambda: -sym.integer(i64_min))


if __name__ == '__main__':
    unittest.main(verbosity=2)
