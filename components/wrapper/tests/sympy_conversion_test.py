"""
Test conversion of wrenfold expressions to sympy, and visa-versa.
"""
import unittest
import sympy as sp
import sys
import typing as T

from wrenfold import sym
from wrenfold import sympy_conversion
from test_base import MathTestBase

# Some shorthand for the purpose of this test:
spy = sym.to_sympy
unspy = lambda x: sympy_conversion.from_sympy(expr=x, sp=sp)


class SympyConversionTest(MathTestBase):
    """Test conversion to and from sympy."""

    def assertEqualSp(self, first: T.Any, second: sym.AnyExpression, msg: T.Any = None):
        """Convert `second` to sympy form, and compare with __eq__ operator."""
        self.assertEqual(first=first, second=spy(second), msg=msg)

    def assertIdenticalFromSp(self, first: sym.AnyExpression, second: T.Any):
        """Convert `second` from sympy to wrenfold form, and compare with is_identical_to."""
        self.assertIdentical(a=first, b=unspy(second))

    def test_variables(self):
        """Test conversion of Variable <-> Symbol."""
        self.assertEqualSp(sp.symbols('x'), sym.symbols('x'))
        self.assertEqualSp(sp.symbols('x', real=True), sym.symbols('x', real=True))
        self.assertEqualSp(sp.symbols('x', positive=True), sym.symbols('x', positive=True))
        self.assertEqualSp(sp.symbols('x', nonnegative=True), sym.symbols('x', nonnegative=True))
        self.assertEqualSp(sp.symbols('x', complex=True), sym.symbols('x', complex=True))
        self.assertEqualSp(sp.symbols('$arg_2_3', real=True), sym.function_argument_variable(2, 3))

        # sympy --> wf
        self.assertIdenticalFromSp(sym.symbols('x'), sp.symbols('x'))
        self.assertIdenticalFromSp(sym.symbols('x', real=True), sp.symbols('x', real=True))
        self.assertIdenticalFromSp(sym.symbols('x', positive=True), sp.symbols('x', positive=True))
        self.assertIdenticalFromSp(
            sym.symbols('x', nonnegative=True), sp.symbols('x', nonnegative=True))
        self.assertIdenticalFromSp(sym.symbols('x', complex=True), sp.symbols('x', complex=True))
        self.assertIdenticalFromSp(
            sym.function_argument_variable(2, 3), sp.symbols('$arg_2_3', real=True))

    def test_numeric_constants(self):
        self.assertEqualSp(sp.Integer(0), sym.zero)
        self.assertEqualSp(sp.Integer(1), sym.one)
        self.assertEqualSp(sp.Integer(2), sym.integer(2))
        self.assertEqualSp(sp.Integer(-5), sym.integer(-5))

        self.assertEqualSp(sp.Rational(1, 2), sym.rational(1, 2))
        self.assertEqualSp(sp.Rational(6, 14), sym.rational(6, 14))
        self.assertEqualSp(sp.Rational(-3, 5), sym.rational(-3, 5))

        self.assertEqualSp(sp.Float(0.0), sym.float(0.0))
        self.assertEqualSp(sp.Float(1.0), sym.float(1.0))
        self.assertEqualSp(sp.Float(1.2), sym.float(1.2))
        self.assertEqualSp(sp.Float(-4.845926), sym.float(-4.845926))
        self.assertEqualSp(sp.Float(1.782e10), sym.float(1.782e10))
        self.assertEqualSp(sp.Float(sys.float_info.max), sys.float_info.max)
        self.assertEqualSp(sp.Float(sys.float_info.min), sys.float_info.min)

        # sympy --> wf
        self.assertIdenticalFromSp(sym.zero, sp.Integer(0))
        self.assertIdenticalFromSp(sym.one, sp.Integer(1))
        self.assertIdenticalFromSp(sym.integer(-1), sp.Integer(-1))
        self.assertIdenticalFromSp(sym.integer(-23), -sp.Integer(23))
        self.assertIdenticalFromSp(sym.integer(2 ** 63 - 1), sp.Integer(2 ** 63 - 1))
        self.assertIdenticalFromSp(sym.integer(-1 * 2 ** 63), -sp.Integer(2 ** 63))

        self.assertIdenticalFromSp(sym.float(0.0), sp.Float(0.0))
        self.assertIdenticalFromSp(sym.float(1.0), sp.Float(1.0))
        self.assertIdenticalFromSp(sym.float(sys.float_info.max), sp.Float(sys.float_info.max))
        self.assertIdenticalFromSp(sym.float(sys.float_info.min), sp.Float(sys.float_info.min))

        self.assertIdenticalFromSp(sym.rational(1, 2), sp.Rational(1, 2))
        self.assertIdenticalFromSp(sym.rational(-5, 6), sp.Rational(-5, 6))

    def test_symbolic_constants(self):
        """Test conversion of special mathematical constants."""
        self.assertEqualSp(sp.pi, sym.pi)
        self.assertEqualSp(sp.E, sym.E)
        self.assertEqualSp(sp.zoo, sym.zoo)
        self.assertEqualSp(sp.I, sym.I)
        self.assertEqualSp(sp.nan, sym.nan)

        # sympy --> wf
        self.assertIdenticalFromSp(sym.pi, sp.pi)
        self.assertIdenticalFromSp(sym.E, sp.E)
        self.assertIdenticalFromSp(sym.zoo, sp.zoo)
        self.assertIdenticalFromSp(sym.I, sp.I)
        self.assertIdenticalFromSp(sym.nan, sp.nan)

        # No equivalent of infinity:
        self.assertRaises(TypeError, lambda: unspy(sp.oo))
        self.assertRaises(TypeError, lambda: unspy(-sp.oo))

    def test_boolean_constants(self):
        self.assertEqualSp(sp.true, sym.true)
        self.assertEqualSp(sp.false, sym.false)

        # sympy --> wf
        self.assertIdenticalFromSp(sym.true, sp.true)
        self.assertIdenticalFromSp(sym.false, sp.false)

    def test_addition(self):
        x, y = sym.symbols('x, y')
        self.assertIsInstance(spy(x + y), sp.Add)
        self.assertEqualSp(spy(x) + spy(y), x + y)
        self.assertEqualSp(spy(x) + spy(3), x + 3)
        self.assertEqualSp(spy(x) + spy(sym.one / 3) - spy(y), x + sym.one / 3 - y)
        self.assertEqualSp(spy(x) + 3.14159, x + 3.14159)
        self.assertEqualSp(spy(-x) + spy(-y), -x - y)

        # sympy --> wf
        self.assertIdenticalFromSp(x + y, spy(x) + spy(y))
        self.assertIdenticalFromSp(x + 3, spy(x) + 3)
        self.assertIdenticalFromSp(y - 22 / x, spy(y) - 22 / spy(x))
        self.assertIdenticalFromSp(-x - y, -spy(x) - spy(y))

    def test_multiplication(self):
        x, y = sym.symbols('x, y')
        self.assertIsInstance(spy(x * y), sp.Mul)
        self.assertIsInstance(spy(x / y), sp.Mul)
        self.assertIsInstance(spy(x + x), sp.Mul)
        self.assertEqualSp(spy(x) * 2, x * 2)
        self.assertEqualSp(spy(x) * -0.7842, x * -0.7842)
        self.assertEqualSp(spy(x) * spy(y), x * y)
        self.assertEqualSp(spy(x) * spy(x) * spy(y) * -22, x * x * y * -22)
        self.assertEqualSp(spy(x) * spy(sym.pi) * spy(sym.pi), x * sym.pi * sym.pi)
        self.assertEqualSp(spy(x) / spy(y), x / y)
        self.assertEqualSp(spy(x) / spy(y) / spy(3), x / y / 3)

        # sympy --> wf
        self.assertIdenticalFromSp(x * y, spy(x) * spy(y))
        self.assertIdenticalFromSp(x / y, spy(x) / spy(y))
        self.assertIdenticalFromSp(x * 2, spy(x) * 2)
        self.assertIdenticalFromSp(y * y * 0.8294, spy(y) * spy(y) * 0.8294)
        self.assertIdenticalFromSp(x / y / 55, spy(x) / spy(y) / 55)

    def test_power(self):
        x, y = sym.symbols('x, y')
        w = sym.symbols('w', real=True, nonnegative=True)
        self.assertIsInstance(spy(x * x), sp.Pow)
        self.assertIsInstance(spy(1 / x), sp.Pow)
        self.assertEqualSp(sp.Pow(spy(x), 2), x * x)
        self.assertEqualSp(sp.Pow(spy(x), 3), x * x * x)
        self.assertEqualSp(sp.Pow(spy(x * y), sp.Integer(1) / 2), sym.sqrt(x * y))
        self.assertEqualSp(sp.Pow(3, sp.Integer(1) / 2), sym.sqrt(3))
        self.assertEqualSp(sp.Pow(22, sp.Integer(3) / 5), sym.pow(22, sym.integer(3) / 5))
        self.assertEqualSp(sp.Pow(spy(w), 2) * sp.Pow(spy(x), 2), sym.pow(w * x, 2))
        self.assertEqualSp(-sp.Pow(spy(x), 3), sym.pow(-x, 3))
        self.assertEqualSp(
            sp.Pow(-145212480 * spy(x),
                   sp.Integer(1) / 6), sym.pow(-145212480 * x, sym.one / 6))

        # sympy --> wf
        self.assertIdenticalFromSp(x ** 2, spy(x) ** 2)
        self.assertIdenticalFromSp(x ** 3, spy(x) ** 3)
        self.assertIdenticalFromSp(x ** y, spy(x) ** spy(y))
        self.assertIdenticalFromSp((x ** y) ** (sym.integer(1) / 3),
                                   sp.Pow(spy(x), spy(y)) ** (sp.Integer(1) / 3))
        self.assertIdenticalFromSp(y ** -0.8721, spy(y) ** -0.8721)
        self.assertIdenticalFromSp(x ** -1, 1 / spy(x))
        self.assertIdenticalFromSp(x ** -2, 1 / sp.Pow(spy(x), 2))
        self.assertIdenticalFromSp(w ** sym.rational(1, 3) * x ** sym.rational(1, 3),
                                   sp.Pow(spy(w) * spy(x), sp.Rational(1, 3)))

    def test_functions(self):
        x, y = sym.symbols('x, y')
        self.assertEqualSp(sp.cos(spy(x)), sym.cos(x))
        self.assertEqualSp(sp.sin(spy(x)), sym.sin(x))
        self.assertEqualSp(sp.tan(spy(x)), sym.tan(x))
        self.assertEqualSp(sp.acos(spy(x)), sym.acos(x))
        self.assertEqualSp(sp.asin(spy(x)), sym.asin(x))
        self.assertEqualSp(sp.atan(spy(x)), sym.atan(x))
        self.assertEqualSp(sp.cosh(spy(x)), sym.cosh(x))
        self.assertEqualSp(sp.sinh(spy(x)), sym.sinh(x))
        self.assertEqualSp(sp.tanh(spy(x)), sym.tanh(x))
        self.assertEqualSp(sp.acosh(spy(x)), sym.acosh(x))
        self.assertEqualSp(sp.asinh(spy(x)), sym.asinh(x))
        self.assertEqualSp(sp.atanh(spy(x)), sym.atanh(x))
        self.assertEqualSp(sp.log(spy(x)), sym.log(x))
        self.assertEqualSp(sp.Abs(spy(x)), sym.abs(x))
        self.assertEqualSp(sp.sign(spy(x)), sym.sign(x))
        self.assertEqualSp(sp.floor(spy(x)), sym.floor(x))
        self.assertEqualSp(sp.atan2(spy(y), spy(x)), sym.atan2(y, x))

        # sympy --> wf
        sx, sy = spy(x), spy(y)
        self.assertIdenticalFromSp(sym.cos(x), sp.cos(sx))
        self.assertIdenticalFromSp(sym.sin(x), sp.sin(sx))
        self.assertIdenticalFromSp(sym.tan(x), sp.tan(sx))
        self.assertIdenticalFromSp(sym.acos(x), sp.acos(sx))
        self.assertIdenticalFromSp(sym.asin(x), sp.asin(sx))
        self.assertIdenticalFromSp(sym.atan(x), sp.atan(sx))
        self.assertIdenticalFromSp(sym.cosh(x), sp.cosh(sx))
        self.assertIdenticalFromSp(sym.sinh(x), sp.sinh(sx))
        self.assertIdenticalFromSp(sym.tanh(x), sp.tanh(sx))
        self.assertIdenticalFromSp(sym.acosh(x), sp.acosh(sx))
        self.assertIdenticalFromSp(sym.asinh(x), sp.asinh(sx))
        self.assertIdenticalFromSp(sym.atanh(x), sp.atanh(sx))
        self.assertIdenticalFromSp(sym.log(x), sp.log(sx))
        self.assertIdenticalFromSp(sym.abs(x), sp.Abs(sx))
        self.assertIdenticalFromSp(sym.sign(x), sp.sign(sx))
        self.assertIdenticalFromSp(sym.floor(x), sp.floor(sx))
        self.assertIdenticalFromSp(sym.atan2(y, x), sp.atan2(sy, sx))

    def test_relational(self):
        x, y = sym.symbols('x, y')
        self.assertEqualSp(spy(x) < spy(y), x < y)
        self.assertEqualSp(spy(y) < spy(x), x > y)
        self.assertEqualSp(spy(x) <= spy(y), x <= y)
        self.assertEqualSp(spy(y) <= spy(x), x >= y)
        self.assertEqualSp(sp.Eq(spy(x), spy(y)), sym.eq(x, y))

        # sympy --> wf
        self.assertIdenticalFromSp(x < y, spy(x) < spy(y))
        self.assertIdenticalFromSp(y < x, spy(x) > spy(y))
        self.assertIdenticalFromSp(x <= y, spy(x) <= spy(y))
        self.assertIdenticalFromSp(y <= x, spy(x) >= spy(y))
        self.assertIdenticalFromSp(sym.eq(x, y), sp.Eq(spy(x), spy(y)))

    def test_conditional(self):
        x, y = sym.symbols('x, y')
        self.assertEqualSp(sp.Piecewise((2, spy(x < y)), (3, True)), sym.where(x < y, 2, 3))
        self.assertEqualSp(
            sp.Piecewise((sp.cos(spy(x)), spy(x <= y + 2)), (sp.sin(spy(y)) * 2, True)),
            sym.where(x <= y + 2, sym.cos(x),
                      sym.sin(y) * 2))

        # sympy --> wf
        self.assertIdenticalFromSp(sym.where(x < y, 2, 3), sp.Piecewise((2, spy(x < y)), (3, True)))
        self.assertIdenticalFromSp(
            sym.where(x <= y + 2, sym.cos(x),
                      sym.sin(y) * 2),
            sp.Piecewise((sp.cos(spy(x)), spy(x <= y + 2)), (sp.sin(spy(y)) * 2, True)))

    def test_iverson_bracket(self):
        x, y = sym.symbols('x, y')
        self.assertEqualSp(sp.Piecewise((1, spy(x < y)), (0, True)), sym.iverson(x < y))

        # sympy --> wf
        self.assertIdenticalFromSp(sym.iverson(x < y), sp.Piecewise((1, spy(x < y)), (0, True)))

    def test_matrix(self):
        x, y = sym.symbols('x, y')

        m1_sp = sp.Matrix([[spy(x) + 3, -2 * spy(y)], [spy(x * y), 22]])
        m1_wf = sym.matrix([[x + 3, -2 * y], [x * y, 22]])

        m2_sp = sp.Matrix([sp.cos(spy(x)), spy(x * y * 3), sp.pi])
        m2_wf = sym.vector(sym.cos(x), x * y * 3, sym.pi)

        self.assertEqualSp(m1_sp, m1_wf)
        self.assertEqualSp(m2_sp, m2_wf)

        # sympy --> wf
        self.assertIdenticalFromSp(m1_wf, m1_sp)
        self.assertIdenticalFromSp(m2_wf, m2_sp)
        self.assertIdenticalFromSp(sym.eye(2), sp.eye(2))
        self.assertIdenticalFromSp(sym.zeros(3, 5), sp.zeros(3, 5))


if __name__ == '__main__':
    unittest.main()