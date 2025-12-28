"""
Test of geometry module wrapper.

NB: Most of this is tested in `quaternion_test.cc`. This is just a test of the wrapper itself.
"""

import unittest

import numpy as np
from wrenfold import exceptions, sym
from wrenfold.geometry import Quaternion, inverse_left_jacobian_of_so3, left_jacobian_of_so3

from .test_base import MathTestBase


class GeometryWrapperTest(MathTestBase):
    def assertQuatIdentical(self, a: Quaternion, b: Quaternion):
        self.assertIdentical(a.w, b.w)
        self.assertIdentical(a.x, b.x)
        self.assertIdentical(a.y, b.y)
        self.assertIdentical(a.z, b.z)

    def test_construct_quaternion(self):
        """Test construction of quaternions."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        self.assertIdentical(w, q.w)
        self.assertIdentical(x, q.x)
        self.assertIdentical(y, q.y)
        self.assertIdentical(z, q.z)
        self.assertTrue(q.is_identical_to(q))

        q = Quaternion()
        self.assertIdentical(1, q.w)
        self.assertIdentical(0, q.x)
        self.assertIdentical(0, q.y)
        self.assertIdentical(0, q.z)

    def test_repr(self):
        """Test repr()"""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        self.assertEqual("Quaternion(w, x, y, z)", repr(q))
        self.assertEqual("Quaternion(1, 0, 0, 0)", repr(Quaternion()))

    def test_convert_quaternion_formats(self):
        """Test conversion between formats."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)

        as_list = q.to_list()
        self.assertIsInstance(as_list, list)
        self.assertQuatIdentical(q, Quaternion.from_wxyz([w, x, y, z]))
        self.assertQuatIdentical(q, Quaternion.from_wxyz(sym.vector(w, x, y, z)))

        self.assertQuatIdentical(q, Quaternion.from_xyzw([x, y, z, w]))
        self.assertQuatIdentical(q, Quaternion.from_xyzw(sym.vector(x, y, z, w)))

        self.assertIdentical(sym.vector(w, x, y, z), q.to_vector_wxyz())

        self.assertRaises(exceptions.DimensionError, lambda: Quaternion.from_xyzw([x, y, z]))
        self.assertRaises(
            exceptions.DimensionError,
            lambda: Quaternion.from_xyzw([1.0, -0.23, w, y, 3]),
        )

    def test_normalize(self):
        """Test normalize."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)

        squared_norm = q.squared_norm()
        self.assertIdentical(w * w + x * x + y * y + z * z, squared_norm)
        self.assertIdentical(1, Quaternion().squared_norm())
        self.assertIdentical(sym.sqrt(squared_norm), q.norm())
        self.assertIdentical(1, q.normalized().squared_norm().collect(squared_norm))

    def test_conjugate_and_inverse(self):
        """Test conjugation and inverse."""
        q = Quaternion.with_name("q")
        squared_norm = q.squared_norm()

        q_ident = q * q.conjugate()
        self.assertIdentical(1, q_ident.w.subs(squared_norm, 1))
        self.assertIdentical(0, q_ident.x)
        self.assertIdentical(0, q_ident.y)
        self.assertIdentical(0, q_ident.z)

        q = Quaternion(4, -3, 1, -2)
        q_ident = q * q.inverse()
        self.assertIdentical(1, q_ident.w)
        self.assertIdentical(0, q_ident.x)
        self.assertIdentical(0, q_ident.y)
        self.assertIdentical(0, q_ident.z)

    def test_to_rotation_matrix(self):
        """Test calling to_rotation_matrix."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        R = q.to_rotation_matrix()
        self.assertEqual((3, 3), R.shape)
        self.assertIdentical(R.T, q.conjugate().to_rotation_matrix())

    def test_to_rotation_vector(self):
        """Test calling to_rotation_vector."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        v = q.to_rotation_vector()
        self.assertIsInstance(v, sym.MatrixExpr)
        self.assertEqual((3, 1), v.shape)
        for epsilon in (0, 0.5, None):
            for use_atan2 in (True, False):
                v = q.to_rotation_vector(epsilon=epsilon, use_atan2=use_atan2)
                self.assertIsInstance(v, sym.MatrixExpr)
                self.assertEqual((3, 1), v.shape)

    def test_to_angle_axis(self):
        """Test calling to_angle_axis."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        angle, axis = q.to_angle_axis()
        self.assertIsInstance(angle, sym.Expr)
        self.assertIsInstance(axis, sym.MatrixExpr)
        self.assertEqual((3, 1), axis.shape)
        for epsilon in (0, 0.5, None):
            angle, axis = q.to_angle_axis(epsilon=epsilon)
            self.assertIsInstance(angle, sym.Expr)
            self.assertIsInstance(axis, sym.MatrixExpr)
            self.assertEqual((3, 1), axis.shape)

    def test_rotate_vector(self):
        """Test calling rotate()"""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        R = q.to_rotation_matrix()

        v = sym.vector(*sym.symbols("v_x, v_y, v_z"))
        self.assertIdentical(
            R, (q.rotate(v)).distribute().jacobian(v).subs(w**2, 1 - x**2 - y**2 - z**2)
        )

    def test_rotation_constructors(self):
        """Test construction via rotation helpers."""
        self.assertQuatIdentical(
            Quaternion.from_angle_axis(sym.pi / 2, 1, 0, 0),
            Quaternion.from_x_angle(sym.pi / 2),
        )
        self.assertQuatIdentical(
            Quaternion.from_angle_axis(-sym.pi / 4, 0, 1, 0),
            Quaternion.from_y_angle(-sym.pi / 4),
        )
        self.assertQuatIdentical(
            Quaternion.from_angle_axis(sym.pi / 6, 0, 0, 1),
            Quaternion.from_z_angle(sym.pi / 6),
        )
        self.assertQuatIdentical(
            Quaternion.from_x_angle(0.125),
            Quaternion.from_rotation_vector(sym.vector(0.125, 0, 0), None),
        )
        self.assertQuatIdentical(
            Quaternion.from_y_angle(-0.32),
            Quaternion.from_rotation_vector(0.0, -0.32, 0.0, None),
        )

        # Check we can call with different values of epsilon:
        for epsilon in (0, 0.1, None):
            Quaternion.from_rotation_vector(*sym.symbols("x, y, z"), epsilon=epsilon)
            Quaternion.from_rotation_vector(sym.vector(*sym.symbols("x, y, z")), epsilon=epsilon)

    def test_from_rotation_matrix(self):
        """Test calling from_rotation_matrix."""
        theta = sym.symbols("theta")
        R = sym.matrix(
            [
                [1, 0, 0],
                [0, sym.cos(theta), -sym.sin(theta)],
                [0, sym.sin(theta), sym.cos(theta)],
            ]
        )
        self.assertIdentical(
            Quaternion(0, 1, 0, 0),
            Quaternion.from_rotation_matrix(R).subs(theta, sym.pi),
        )

    def test_derivative_matrices(self):
        """Test calling right_retract_derivative and right_local_coordinates_derivative."""
        w, x, y, z = sym.symbols("w, x, y, z")
        q = Quaternion(w, x, y, z)
        J0 = q.right_retract_derivative()
        J1 = q.right_local_coordinates_derivative()
        self.assertIsInstance(J0, sym.MatrixExpr)
        self.assertIsInstance(J1, sym.MatrixExpr)
        self.assertIdentical(sym.eye(3), (J1 * J0).subs(w**2 + x**2 + y**2 + z**2, 1))

    def test_jacobians_of_so3(self):
        """Test calling left_jacobian_of_so3 and inverse_left_jacobian_of_so3."""
        x, y, z = sym.symbols("x, y, z")
        for epsilon in (0, 0.01, None):
            left_jacobian_of_so3(sym.vector(x, y, z), epsilon=epsilon)
            inverse_left_jacobian_of_so3(sym.vector(x, y, z), epsilon=epsilon)

        J = left_jacobian_of_so3(sym.vector(x, y, z), None)
        J_inv = inverse_left_jacobian_of_so3(sym.vector(x, y, z), None)
        np.testing.assert_allclose(
            (J * J_inv).subs([(x, 0.12), (y, -0.51), (z, 0.8)]).eval(), np.eye(3), atol=1.0e-15
        )


if __name__ == "__main__":
    unittest.main(verbosity=2)
