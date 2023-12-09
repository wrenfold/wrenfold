"""
Test of matrix-specific functionality.
"""
import numpy as np
import typing as T
import unittest

from wrenfold import sym
from test_base import MathTestBase


class MatrixWrapperTest(MathTestBase):

    def test_matrix_constructors(self):
        """Test different methods of constructing matrices."""
        x, y, z, a, b, c = sym.symbols('x, y, z, a, b, c')

        data = [(x, a), (y, b), (z, c)]
        m = sym.matrix(data)
        self.assertEqual('Matrix', m.type_name)
        self.assertEqual((3, 2), m.shape)
        self.assertEqual(6, m.size)
        self.assertFalse(m.is_empty)

        for i, row in enumerate(data):
            for j, value in enumerate(row):
                self.assertIdentical(value, m[i, j])

        data_as_list = m.to_list()
        self.assertIdentical(sym.matrix(data_as_list), m)

        # Different ways of constructing vectors:
        v = sym.vector(x, y, z)
        self.assertEqual((3, 1), v.shape)
        self.assertEqual('Matrix', v.type_name)
        self.assertIdentical(v, sym.matrix([x, y, z]))
        self.assertIdentical(v, sym.matrix([[x], [y], [z]]))
        self.assertIdentical(sym.row_vector(x, y, z), sym.matrix([[x, y, z]]))

        # Stacking matrices:
        self.assertIdentical(
            sym.matrix([[a, x], [b, y]]), sym.matrix([sym.row_vector(a, x),
                                                      sym.row_vector(b, y)]))
        self.assertIdentical(m, sym.matrix([m[0:2, :], data[2]]))
        self.assertIdentical(
            sym.vector(a, b, c, x, y, z), sym.matrix([sym.vector(a, b, c),
                                                      sym.vector(x, y, z)]))

        def generator_func(*args):
            for element in args:
                yield element

        # construct from generators
        gen = generator_func(z, x, c)
        self.assertIdentical(sym.vector(z, x, c), sym.matrix(gen))
        gen = generator_func([a, x], [b, y])
        self.assertIdentical(sym.matrix([[a, x], [b, y]]), sym.matrix(gen))
        gen = generator_func(
            generator_func(x, y, z), generator_func(a, b, c), generator_func(1, 2, 3))
        self.assertIdentical(sym.matrix([[x, y, z], [a, b, c], [1, 2, 3]]), sym.matrix(gen))

        # combine generator with other inputs
        self.assertIdentical(
            sym.matrix([(a, c), (1.0, z)]), sym.matrix([(a, c), generator_func(1.0, z)]))
        self.assertIdentical(
            sym.matrix([(b, x, x), (z, 5, z)]),
            sym.matrix([sym.row_vector(b, x, x), generator_func(z, 5, z)]))
        self.assertIdentical(
            sym.matrix([(sym.pi, x, 2), (y, z, c)]),
            sym.matrix([generator_func(sym.pi, x, 2),
                        generator_func(y, z, c)]))

        # throw if empty
        self.assertRaises(sym.DimensionError, lambda: sym.matrix([]))
        self.assertRaises(sym.DimensionError, lambda: sym.vector())
        self.assertRaises(sym.DimensionError, lambda: sym.row_vector())

        # cannot nest matrices:
        self.assertRaises(RuntimeError, lambda: sym.vector(m, 1.5, 2))
        self.assertRaises(RuntimeError, lambda: sym.row_vector(x, y, sym.vector(z, 9.0)))
        self.assertRaises(RuntimeError,
                          lambda: sym.matrix([[sym.row_vector(1, x), 1], [0.0, sym.pi]]))

        # mixing iterable/non-iterable:
        self.assertRaises(TypeError, lambda: sym.matrix([sym.row_vector(x, y), 5]))
        self.assertRaises(TypeError, lambda: sym.matrix([sym.vector(x, y), z]))

        # dimensions do not agree:
        self.assertRaises(sym.DimensionError, lambda: sym.matrix([(y, 2), (x,)]))
        self.assertRaises(sym.DimensionError, lambda: sym.matrix([(a, sym.pi, sym.euler),
                                                                  (x, 5, z, b)]))

    def test_matrix_repr(self):
        """Test matrix __repr__."""
        x, y, z = sym.symbols('x, y, z')
        m = sym.matrix([[x * x, -y + 3, x * z], [-2, sym.pi, 5]])
        self.assertEqual('[[x ** 2, 3 - y, x * z],\n [    -2,    pi,     5]]', repr(m))

    def test_bool_conversion(self):
        """Test that we cannot cast to bool."""
        x, y, z = sym.symbols('x, y, z')
        m = sym.matrix([[x * y, -2], [z, sym.pi]])
        self.assertRaises(sym.TypeError, lambda: bool(m))
        self.assertRaises(sym.TypeError, lambda: 1 if m else 0)

    def test_special_matrices(self):
        """Test convenience constructors for specific matrices."""
        eye = sym.eye(3)
        self.assertEqual((3, 3), eye.shape)
        for i in range(eye.shape[0]):
            for j in range(eye.shape[1]):
                self.assertIdentical(sym.one if i == j else sym.zero, eye[i, j])

        self.assertRaises(sym.DimensionError, lambda: sym.eye(0))
        self.assertRaises(sym.DimensionError, lambda: sym.eye(-2))

        zeros = sym.zeros(5, 4)
        self.assertEqual((5, 4), zeros.shape)
        for i in range(zeros.shape[0]):
            for j in range(zeros.shape[1]):
                self.assertIdentical(sym.zero, zeros[i, j])

        self.assertRaises(sym.DimensionError, lambda: sym.zeros(-4, 3))
        self.assertRaises(sym.DimensionError, lambda: sym.zeros(0, 6))

    @staticmethod
    def generate_slices(dim: int, step_sizes: T.Iterable[int] = (1, 2, 3)):
        """
        Generate slices that are valid in a container of size `dim`.
        """
        for start in range(dim):
            for end in range(dim):
                for sz in step_sizes:
                    yield slice(start, end, sz if end >= start else -sz)

    @staticmethod
    def generate_row_and_col_slices(shape: T.Tuple[int, int],
                                    step_sizes: T.Iterable[int] = (1, 2, 3)):
        """
        Generate slices that extract all possible sub-blocks (for the given step sizes) for a 2D array
        of size `shape`.
        """
        rows, cols = np.mgrid[0:shape[0], 0:shape[1]]
        rows = rows.reshape(-1)
        cols = cols.reshape(-1)
        for (start_row, start_col) in zip(rows, cols):
            for (end_row, end_col) in zip(rows, cols):
                for sz in step_sizes:
                    row_slice = slice(start_row, end_row, sz if end_row >= start_row else -sz)
                    col_slice = slice(start_col, end_col, sz if end_col >= start_col else -sz)
                    yield row_slice, col_slice

    def test_matrix_slicing(self):
        """Test ability to slice specific elements from matrices."""
        # Create 4x5 symbol grid:
        symbols = []
        for i in range(4):
            symbols.append(sym.symbols(f'x_{i}{j}' for j in range(5)))

        symbol_array = np.array(symbols, dtype=object)

        m = sym.matrix(symbols)
        self.assertEqual((4, 5), m.shape)

        # access individual elements
        for i in range(m.shape[0]):
            for j in range(m.shape[1]):
                self.assertIdentical(symbols[i][j], m[i, j])

        # slice rows
        for i in range(m.shape[0]):
            self.assertEqual((1, 5), m[i].shape)
            self.assertIdentical(sym.row_vector(*symbols[i]), m[i])

        # slice cols
        for j in range(m.shape[1]):
            self.assertEqual((4, 1), m[:, j].shape)
            self.assertIdentical(sym.vector(*[symbols[i][j] for i in range(m.shape[0])]), m[:, j])

        # reverse using slice:
        self.assertIdentical(sym.matrix(np.fliplr(symbol_array)), m[:, ::-1])
        self.assertIdentical(sym.matrix(np.flipud(symbol_array)), m[::-1, :])
        self.assertIdentical(sym.matrix(np.flip(symbol_array, axis=(0, 1))), m[::-1, ::-1])

        # slice on cols:
        for i in range(m.shape[0]):
            for col_slice in self.generate_slices(m.shape[1]):
                sliced_symbols = symbol_array[i, col_slice].reshape((1, -1))
                if sliced_symbols.size != 0:
                    self.assertIdentical(sym.matrix(sliced_symbols), m[i, col_slice])
                else:
                    self.assertTrue(m[i, col_slice].is_empty)

        # slice on rows:
        for j in range(m.shape[1]):
            for row_slice in self.generate_slices(m.shape[0]):
                sliced_symbols = symbol_array[row_slice, j].reshape((-1, 1))
                if sliced_symbols.size != 0:
                    self.assertIdentical(sym.matrix(sliced_symbols), m[row_slice, j])
                else:
                    self.assertTrue(m[row_slice, j].is_empty)

        # slice on both dimensions:
        for row_slice, col_slice in self.generate_row_and_col_slices(m.shape):
            sliced_symbols = symbol_array[row_slice, col_slice]
            if sliced_symbols.size != 0:
                self.assertIdentical(sym.matrix(sliced_symbols), m[row_slice, col_slice])
            else:
                self.assertTrue(m[row_slice, col_slice].is_empty)

        # Invalid indices:
        self.assertRaises(sym.DimensionError, lambda: m[-5, 0])
        self.assertRaises(sym.DimensionError, lambda: m[0, -6])
        self.assertRaises(sym.DimensionError, lambda: m[0, 10])
        self.assertRaises(sym.DimensionError, lambda: m[11, 2])

        # Test iteration:
        self.assertEqual(4, len(m))
        for index, row in enumerate(m):
            self.assertIdentical(m[index], row)

    def test_unary_map(self):
        """Test unary map w/ a lambda."""
        q = sym.symbols('q')
        v = sym.vector(sym.pi, 7, q)
        v_mapped = v.unary_map(lambda x: sym.pow(x, 2) * 3)
        self.assertIdentical(sym.vector(3 * sym.pi ** 2, 147, 3 * q ** 2), v_mapped)

    def test_vec(self):
        """Test column-order vectorization."""
        symbols = sym.symbols(['x, y', 'a, b', 'p, q'])
        m = sym.matrix(symbols)
        m_t = m.transpose()
        self.assertEqual((3, 2), m.shape)
        self.assertIdentical(sym.matrix([m[:, 0], m[:, 1]]), sym.vec(m))
        self.assertIdentical(
            sym.matrix([m[0, :].transpose(), m[1, :].transpose(), m[2, :].transpose()]),
            sym.vec(m_t))

    def test_matrix_operations(self):
        """Check that add/mul/sub operations are wrapped."""
        m0 = sym.matrix_of_symbols('x', 4, 3)
        m1 = sym.matrix_of_symbols('y', 3, 6)

        # matrix mul should produce matrix:
        self.assertIsInstance(m0 * m1, sym.MatrixExpr)
        self.assertEqual((4, 6), (m0 * m1).shape)
        self.assertIdentical(sym.matrix(np.array(m0) @ np.array(m1)), m0 * m1)
        self.assertIdentical(sym.matrix(np.array(m1).T @ np.array(m0).T), m1.T * m0.T)

        v = sym.vector(2, sym.pi, sym.integer(5) / 2)
        self.assertIdentical(sym.matrix(np.array(m0) @ np.array(v)), m0 * v)
        self.assertIdentical(sym.matrix(np.array(v).T @ np.array(m0).T), v.T * m0.T)

        # inner product still produces a matrix of size 1x1
        x, z = sym.symbols('x, z')
        u = sym.row_vector(x, 0, z)
        self.assertIsInstance(u * v, sym.MatrixExpr)
        self.assertEqual((1, 1), (u * v).shape)
        self.assertIdentical(sym.vector(2 * x + sym.integer(5) / 2 * z), u * v)

        # negation:
        self.assertIsInstance(-m0, sym.MatrixExpr)
        self.assertIdentical(m0 * -1, -m0)

        # addition/subtraction:
        m0 = sym.matrix_of_symbols('p', 2, 3)
        m1 = sym.matrix_of_symbols('q', 2, 3)
        self.assertIsInstance(m0 + m1, sym.MatrixExpr)
        self.assertIsInstance(m0 - m1, sym.MatrixExpr)
        self.assertEqual((2, 3), (m0 + m1).shape)
        self.assertIdentical(sym.matrix(np.array(m0) + np.array(m1)), m0 + m1)
        self.assertIdentical(sym.matrix(np.array(m0) - np.array(m1)), m0 - m1)
        self.assertIdentical(sym.zeros(2, 3), m0 - m0)
        self.assertIdentical(sym.zeros(2, 3), m1 - m1)
        self.assertIdentical(m0 * 2, m0 + m0)
        self.assertIdentical(m1 * 3, m1 + m1 + m1)

        # cannot add incompatible dimensions
        self.assertRaises(sym.DimensionError, lambda: m0 + sym.identity(4))
        self.assertRaises(sym.DimensionError, lambda: m0 - sym.zeros(6, 7))

        # division by matrix is prohibited:
        self.assertRaises(TypeError, lambda: 31 / m0)
        self.assertRaises(TypeError, lambda: u / m1)

    def test_distribute(self):
        """Test calling distribute on a matrix."""
        x, y, z = sym.symbols('x, y, z')
        m = sym.vector((x + y) * (2 + z), sym.sin(x) * (y + 2))
        self.assertIdentical(
            sym.vector(2 * x + z * x + y * 2 + y * z,
                       sym.sin(x) * y + sym.sin(x) * 2), m.distribute())

    def test_collect(self):
        """Test calling collect on a matrix."""
        x, y, z = sym.symbols('x, y, z')
        m = sym.vector(x * x * (y - 3) + x * (5 - y) + x * x * x * (y * y * (z - 3) + y * (2 - z)),
                       x * x * (z + 6) + y * y * (sym.sin(z) - 2))
        self.assertIdentical(m, m.distribute().collect([x, y]))

    def test_diff(self):
        """Test calling diff on a matrix (element-wise differentiation)."""
        x, y, z, w = sym.symbols('x, y, z, w')
        m = sym.matrix([(x * sym.cos(y * z), y + sym.tan(z)), (z * x * y / w, w * sym.pow(x, y))])
        for var in (x, y, z, w):
            m_diff = m.diff(var=var, order=1)
            self.assertIdentical(m_diff, m.unary_map(lambda el: el.diff(var)))

    def test_jacobian(self):
        """Test calling jacobian on a matrix."""
        x, y, z, w = sym.symbols('x, y, z, w')
        m = sym.matrix([x * x + y * z * w, w * y * sym.cos(x - z) + z])
        jac_expected = sym.matrix([[m[0].diff(x), m[0].diff(y), m[0].diff(z)],
                                   [m[1].diff(x), m[1].diff(y), m[1].diff(z)]])
        self.assertIdentical(jac_expected, m.jacobian([x, y, z]))
        self.assertIdentical(jac_expected, m.jacobian(sym.vector(x, y, z)))

    def test_subs(self):
        """Test calling subs on a matrix."""
        a, b, c, d, x, y, z = sym.symbols('a, b, c, d, x, y, z')
        m = sym.matrix([(a + x * 2, b - c), (c - sym.sin(y), d + sym.log(d))])
        self.assertIdentical(
            sym.matrix([(0, b - c), (c - sym.sin(y), z)]),
            m.subs(a, -x * 2).subs(d + sym.log(d), z))

    def test_matrix_conditional(self):
        """Test creating a matrix conditional."""
        a, b, c, d, x, y, z = sym.symbols('a, b, c, d, x, y, z')

        u = sym.vector(2 * a, b - a, c * d)
        v = sym.vector(x * y, 2, d * z)
        self.assertIdentical(u, sym.where(sym.Expr(1) > 0, u, v))
        self.assertRaises(TypeError, lambda: sym.where(x < 0, u, d + c))
        self.assertRaises(sym.DimensionError, lambda: sym.where(x > 0, u, u.transpose()))

    def test_determinant(self):
        """Test calling `det()` on a matrix."""
        a, b, c, d = sym.symbols('a, b, c, d')
        M = sym.matrix([[a, b], [c, d]])
        self.assertIdentical(a * d - b * c, M.det())
        self.assertIdentical(M.det(), sym.det(M))


if __name__ == '__main__':
    unittest.main(verbosity=2)
