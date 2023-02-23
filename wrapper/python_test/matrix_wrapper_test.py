"""
Test of matrix-specific functionality.
"""
import numpy as np
import typing as T
import unittest

import mc

from test_base import MathTestBase


class MatrixWrapperTest(MathTestBase):

    def test_matrix_constructors(self):
        """Test different methods of constructing matrices."""
        x, y, z, a, b, c = mc.symbols('x, y, z, a, b, c')

        data = [(x, a), (y, b), (z, c)]
        m = mc.matrix(data)
        self.assertEqual('Matrix', m.type_name)
        self.assertEqual((3, 2), m.shape)
        self.assertEqual(6, m.size)
        self.assertFalse(m.is_empty)

        for i, row in enumerate(data):
            for j, value in enumerate(row):
                self.assertIdentical(value, m[i, j])

        data_as_list = m.to_list()
        self.assertIdentical(mc.matrix(data_as_list), m)

        # Different ways of constructing vectors:
        v = mc.vector(x, y, z)
        self.assertEqual((3, 1), v.shape)
        self.assertEqual('Matrix', v.type_name)
        self.assertIdentical(v, mc.matrix([x, y, z]))
        self.assertIdentical(v, mc.matrix([[x], [y], [z]]))
        self.assertIdentical(mc.row_vector(x, y, z), mc.matrix([[x, y, z]]))

        # Stacking matrices:
        self.assertIdentical(
            mc.matrix([[a, x], [b, y]]), mc.matrix([mc.row_vector(a, x),
                                                    mc.row_vector(b, y)]))
        self.assertIdentical(m, mc.matrix([m[0:2, :], data[2]]))
        self.assertIdentical(
            mc.vector(a, b, c, x, y, z), mc.matrix([mc.vector(a, b, c),
                                                    mc.vector(x, y, z)]))

        def generator_func(*args):
            for element in args:
                yield element

        # construct from generators
        gen = generator_func(z, x, c)
        self.assertIdentical(mc.vector(z, x, c), mc.matrix(gen))
        gen = generator_func([a, x], [b, y])
        self.assertIdentical(mc.matrix([[a, x], [b, y]]), mc.matrix(gen))
        gen = generator_func(
            generator_func(x, y, z), generator_func(a, b, c), generator_func(1, 2, 3))
        self.assertIdentical(mc.matrix([[x, y, z], [a, b, c], [1, 2, 3]]), mc.matrix(gen))

        # combine generator with other inputs
        self.assertIdentical(
            mc.matrix([(a, c), (1.0, z)]), mc.matrix([(a, c), generator_func(1.0, z)]))
        self.assertIdentical(
            mc.matrix([(b, x, x), (z, 5, z)]),
            mc.matrix([mc.row_vector(b, x, x), generator_func(z, 5, z)]))
        self.assertIdentical(
            mc.matrix([(mc.pi, x, 2), (y, z, c)]),
            mc.matrix([generator_func(mc.pi, x, 2),
                       generator_func(y, z, c)]))

        # throw if empty
        self.assertRaises(mc.DimensionError, lambda: mc.matrix([]))
        self.assertRaises(mc.DimensionError, lambda: mc.vector())
        self.assertRaises(mc.DimensionError, lambda: mc.row_vector())

        # cannot nest matrices:
        self.assertRaises(mc.TypeError, lambda: mc.vector(m, 1.5, 2))
        self.assertRaises(mc.TypeError, lambda: mc.row_vector(x, y, mc.vector(z, 9.0)))
        self.assertRaises(mc.TypeError, lambda: mc.matrix([[mc.row_vector(1, x), 1], [0.0, mc.pi]]))

        # mixing iterable/non-iterable:
        self.assertRaises(TypeError, lambda: mc.matrix([mc.row_vector(x, y), 5]))
        self.assertRaises(TypeError, lambda: mc.matrix([mc.vector(x, y), z]))

        # dimensions do not agree:
        self.assertRaises(mc.DimensionError, lambda: mc.matrix([(y, 2), (x,)]))
        self.assertRaises(mc.DimensionError, lambda: mc.matrix([(a, mc.pi, mc.euler),
                                                                (x, 5, z, b)]))

    def test_special_matrices(self):
        """Test convenience constructors for specific matrices."""
        eye = mc.eye(3)
        self.assertEqual((3, 3), eye.shape)
        for i in range(eye.shape[0]):
            for j in range(eye.shape[1]):
                self.assertIdentical(mc.one if i == j else mc.zero, eye[i, j])

        self.assertRaises(mc.DimensionError, lambda: mc.eye(0))
        self.assertRaises(mc.DimensionError, lambda: mc.eye(-2))

        zeros = mc.zeros(5, 4)
        self.assertEqual((5, 4), zeros.shape)
        for i in range(zeros.shape[0]):
            for j in range(zeros.shape[1]):
                self.assertIdentical(mc.zero, zeros[i, j])

        self.assertRaises(mc.DimensionError, lambda: mc.zeros(-4, 3))
        self.assertRaises(mc.DimensionError, lambda: mc.zeros(0, 6))

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
            symbols.append(mc.symbols(f'x_{i}{j}' for j in range(5)))

        symbol_array = np.array(symbols, dtype=object)

        m = mc.matrix(symbols)
        self.assertEqual((4, 5), m.shape)

        # access individual elements
        for i in range(m.shape[0]):
            for j in range(m.shape[1]):
                self.assertIdentical(symbols[i][j], m[i, j])

        # slice rows
        for i in range(m.shape[0]):
            self.assertEqual((1, 5), m[i].shape)
            self.assertIdentical(mc.row_vector(*symbols[i]), m[i])

        # slice cols
        for j in range(m.shape[1]):
            self.assertEqual((4, 1), m[:, j].shape)
            self.assertIdentical(mc.vector(*[symbols[i][j] for i in range(m.shape[0])]), m[:, j])

        # reverse using slice:
        self.assertIdentical(mc.matrix(np.fliplr(symbol_array)), m[:, ::-1])
        self.assertIdentical(mc.matrix(np.flipud(symbol_array)), m[::-1, :])
        self.assertIdentical(mc.matrix(np.flip(symbol_array, axis=(0, 1))), m[::-1, ::-1])

        # slice on cols:
        for i in range(m.shape[0]):
            for col_slice in self.generate_slices(m.shape[1]):
                sliced_symbols = symbol_array[i, col_slice].reshape((1, -1))
                if sliced_symbols.size != 0:
                    self.assertIdentical(mc.matrix(sliced_symbols), m[i, col_slice])
                else:
                    self.assertTrue(m[i, col_slice].is_empty)

        # slice on rows:
        for j in range(m.shape[1]):
            for row_slice in self.generate_slices(m.shape[0]):
                sliced_symbols = symbol_array[row_slice, j].reshape((-1, 1))
                if sliced_symbols.size != 0:
                    self.assertIdentical(mc.matrix(sliced_symbols), m[row_slice, j])
                else:
                    self.assertTrue(m[row_slice, j].is_empty)

        # slice on both dimensions:
        for row_slice, col_slice in self.generate_row_and_col_slices(m.shape):
            sliced_symbols = symbol_array[row_slice, col_slice]
            if sliced_symbols.size != 0:
                self.assertIdentical(mc.matrix(sliced_symbols), m[row_slice, col_slice])
            else:
                self.assertTrue(m[row_slice, col_slice].is_empty)

        # Invalid indices:
        self.assertRaises(mc.DimensionError, lambda: m[-5, 0])
        self.assertRaises(mc.DimensionError, lambda: m[0, -6])
        self.assertRaises(mc.DimensionError, lambda: m[0, 10])
        self.assertRaises(mc.DimensionError, lambda: m[11, 2])

        # Test iteration:
        self.assertEqual(4, len(m))
        for index, row in enumerate(m):
            self.assertIdentical(m[index], row)

    def test_unary_map(self):
        """Test unary map w/ a lambda."""
        q = mc.symbols('q')
        v = mc.vector(mc.pi, 7, q)
        v_mapped = v.unary_map(lambda x: mc.pow(x, 2) * 3)
        self.assertIdentical(mc.vector(3 * mc.pi ** 2, 147, 3 * q ** 2), v_mapped)

    def test_vec(self):
        """Test column-order vectorization."""
        symbols = mc.symbols(['x, y', 'a, b', 'p, q'])
        m = mc.matrix(symbols)
        m_t = m.transpose()
        self.assertEqual((3, 2), m.shape)
        self.assertIdentical(mc.matrix([m[:, 0], m[:, 1]]), mc.vec(m))
        self.assertIdentical(
            mc.matrix([m[0, :].transpose(), m[1, :].transpose(), m[2, :].transpose()]), mc.vec(m_t))

    def test_matrix_operations(self):
        """Check that add/mul/sub operations are wrapped."""
        m0 = mc.matrix_of_symbols('x', 4, 3)
        m1 = mc.matrix_of_symbols('y', 3, 6)

        # matrix mul should produce matrix:
        self.assertIsInstance(m0 * m1, mc.MatrixExpr)
        self.assertEqual((4, 6), (m0 * m1).shape)
        self.assertIdentical(mc.matrix(np.array(m0) @ np.array(m1)), m0 * m1)
        self.assertIdentical(mc.matrix(np.array(m1).T @ np.array(m0).T), m1.T * m0.T)

        v = mc.vector(2, mc.pi, mc.integer(5) / 2)
        self.assertIdentical(mc.matrix(np.array(m0) @ np.array(v)), m0 * v)
        self.assertIdentical(mc.matrix(np.array(v).T @ np.array(m0).T), v.T * m0.T)

        # inner product should produce scalar, not matrix:
        x, z = mc.symbols('x, z')
        u = mc.row_vector(x, 0, z)
        self.assertIsInstance(u * v, mc.Expr)
        self.assertIdentical(2 * x + mc.integer(5) / 2 * z, u * v)

        # negation:
        self.assertIsInstance(-m0, mc.MatrixExpr)
        self.assertIdentical(m0 * -1, -m0)

        # addition/subtraction:
        m0 = mc.matrix_of_symbols('p', 2, 3)
        m1 = mc.matrix_of_symbols('q', 2, 3)
        self.assertIsInstance(m0 + m1, mc.MatrixExpr)
        self.assertIsInstance(m0 - m1, mc.MatrixExpr)
        self.assertEqual((2, 3), (m0 + m1).shape)
        self.assertIdentical(mc.matrix(np.array(m0) + np.array(m1)), m0 + m1)
        self.assertIdentical(mc.matrix(np.array(m0) - np.array(m1)), m0 - m1)
        self.assertIdentical(mc.zeros(2, 3), m0 - m0)
        self.assertIdentical(mc.zeros(2, 3), m1 - m1)
        self.assertIdentical(m0 * 2, m0 + m0)
        self.assertIdentical(m1 * 3, m1 + m1 + m1)

        # cannot add incompatible dimensions
        self.assertRaises(mc.DimensionError, lambda: m0 + mc.identity(4))
        self.assertRaises(mc.DimensionError, lambda: m0 - mc.zeros(6, 7))

        # division by matrix is prohibited:
        self.assertRaises(mc.TypeError, lambda: 31 / m0)
        self.assertRaises(mc.TypeError, lambda: u / m1)

    def test_distribute(self):
        """Test calling distribute on a matrix."""
        x, y, z = mc.symbols('x, y, z')
        m = mc.vector((x + y) * (2 + z), mc.sin(x) * (y + 2))
        self.assertIdentical(
            mc.vector(2 * x + z * x + y * 2 + y * z,
                      mc.sin(x) * y + mc.sin(x) * 2), m.distribute())

    def test_diff(self):
        """Test calling diff on a matrix (element-wise differentiation)."""
        x, y, z, w = mc.symbols('x, y, z, w')
        m = mc.matrix([(x * mc.cos(y * z), y + mc.tan(z)), (z * x * y / w, w * mc.pow(x, y))])
        for var in (x, y, z, w):
            m_diff = m.diff(var=var, order=1)
            self.assertIdentical(m_diff, m.unary_map(lambda el: el.diff(var)))


if __name__ == '__main__':
    unittest.main(verbosity=2)
