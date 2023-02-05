"""
Python tests for the wrapper. Most of the algorithmic testing resides in C++/gtest.

These tests are here to make sure the wrapper works.
"""
import mc


def assert_identical(a: mc.Expr, b: mc.Expr):
    """Assert that two expressions are identical."""
    if a.is_identical_to(b):
        return
    message = (f'The expressions a=`{repr(a)}` b=`{repr(b)}` are not identical.\n' +
               f'The expression tree for `a` is:\n{a.expression_tree_str()}\n' +
               f'The expression tree for `b` is:\n{b.expression_tree_str()}')
    raise AssertionError(message)


def assert_not_identical(a: mc.Expr, b: mc.Expr):
    """Assert that two expressions are not identical."""
    if not a.is_identical_to(b):
        return
    message = (
        f'The expressions a=`{repr(a)}` b=`{repr(b)}` are identical (they should not be).\n' +
        f'The expression tree for `a` is:\n{a.expression_tree_str()}\n' +
        f'The expression tree for `b` is:\n{b.expression_tree_str()}')
    raise AssertionError(message)


def test_create_symbols():
    x = mc.symbol('x')
    y = mc.symbol('y')
    assert_identical(x, x)
    assert_not_identical(x, y)


def test_repr():
    x = mc.symbol('x')
    y = mc.symbol('y')
    z = mc.symbol('z')
    assert repr(x) == 'x'
    assert repr(x * y / 3) == 'x * y / 3'
    assert repr(z * z * z * x) == 'x * z ** 3'
    assert repr(5.0 * z / (x * x)) == '5 * z / x ** 2'
    assert repr(mc.cos(x) * mc.sin(z)) == 'cos(x) * sin(z)'
    assert repr(mc.sqrt(x)) == 'x ** (1 / 2)'
    assert repr(mc.sqrt(21 * x / z)) == '3 ** (1 / 2) * 7 ** (1 / 2) * x ** (1 / 2) / z ** (1 / 2)'


def test_basic_scalar_operations():
    p = mc.symbol('p')
    q = mc.symbol('q')

    assert_identical(p + p, 2 * p)
    assert_identical(p + p, p + p + p - p)
    assert_not_identical(p + p, p - q)
    assert_not_identical(q - 5, q + 5)
    assert_identical(q * q * q / (p * p), mc.pow(q, 3) / mc.pow(p, 2))
    assert_identical(p / q, mc.pow(q / p, -1))
    assert_not_identical(q + 5.0, q + 5)
    assert_identical(2.0 * p, p * 6.0 / 3.0)
    assert_identical(q, -(-q))
    assert_identical(-q, -(-(-q)))
