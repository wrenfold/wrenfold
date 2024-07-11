"""
Test of geometry module wrapper.

NB: Most of this is tested in `quaternion_test.cc`. This is just a test of the wrapper itself.
"""
import unittest

from test_base import MathTestBase

from wrenfold import sym
from wrenfold.geometry import Quaternion


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

    def test_rotation_constructors(self):
        """Test construction via rotation helpers."""
        self.assertQuatIdentical(
            Quaternion.from_angle_axis(sym.pi / 2, 1, 0, 0), Quaternion.from_x_angle(sym.pi / 2))
        self.assertQuatIdentical(
            Quaternion.from_angle_axis(-sym.pi / 4, 0, 1, 0), Quaternion.from_y_angle(-sym.pi / 4))
        self.assertQuatIdentical(
            Quaternion.from_angle_axis(sym.pi / 6, 0, 0, 1), Quaternion.from_z_angle(sym.pi / 6))
        self.assertQuatIdentical(
            Quaternion.from_x_angle(0.125),
            Quaternion.from_rotation_vector(sym.vector(0.125, 0, 0), None))
        self.assertQuatIdentical(
            Quaternion.from_y_angle(-0.32), Quaternion.from_rotation_vector(0.0, -0.32, 0.0, None))

    def test_from_rotation_matrix(self):
        """Test conversion from rotation matrix."""
        theta = sym.symbols("theta")
        R = sym.matrix([[1, 0, 0], [0, sym.cos(theta), -sym.sin(theta)],
                        [0, sym.sin(theta), sym.cos(theta)]])
        self.assertIdentical(
            Quaternion(0, 1, 0, 0),
            Quaternion.from_rotation_matrix(R).subs(theta, sym.pi))


if __name__ == '__main__':
    unittest.main(verbosity=2)
