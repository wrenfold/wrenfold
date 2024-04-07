// Copyright 2024 Gareth Cross
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view quaternion_constructor = R"doc(
Construct a quaternion from scalar expressions in ``[w,x,y,z]`` order.

Args:
  w: The scalar component of the quaternion.
  x: Vector element multiplied by basis vector :math:`\mathbf{i}`.
  y: Vector element multiplied by basis vector :math:`\mathbf{j}`.
  z: Vector element multiplied by basis vector :math:`\mathbf{k}`.

Caution:
  ``Quaternion`` will not normalize after construction. It is expected that the user will pass
  normalized components, or invoke :func:`wrenfold.sym.Quaternion.normalized`.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> q = geometry.Quaternion(w, x, y, z)
  >>> q
  Quaternion(w, x, y, z)
  >>> q.x
  x
)doc";

inline constexpr std::string_view quaternion_identity_constructor = R"doc(
Construct the identity quaternion.

Examples:
  >>> geometry.Quaternion()
  Quaternion(1, 0, 0, 0)
)doc";

inline constexpr std::string_view quaternion_with_name = R"doc(
Create a quaternion filled with ``variable`` expressions, prefixed with the provided name.

Args:
  name: Name prefix to prepend to the quaternion elements.

Returns:
  Quaternion instance.

Examples:
  >>> geometry.Quaternion.with_name('foo')
  Quaternion(foo_w, foo_x, foo_y, foo_z)
)doc";

inline constexpr std::string_view quaternion_eval = R"doc(
Invoke :func:`wrenfold.sym.Expr.eval` on every element of the quaternion, and return a numpy array.

Returns:
  4-element numpy array in ``[w, x, y, z]`` (scalar-first) order.

Examples:
  >>> geometry.Quaternion().eval()
  array([1., 0., 0., 0.])
)doc";

inline constexpr std::string_view quaternion_from_xyzw = R"doc(
Create a quaternion from a 4-element column vector in order ``[x, y, z, w]`` (scalar last).

Args:
  xyzw: 4x1 ``sym.MatrixExpr``

Returns:
  Quaternion instance.
)doc";

inline constexpr std::string_view quaternion_from_wxyz = R"doc(
Create a quaternion from a 4-element column vector in order ``[w, x, y, z]`` (scalar first).

Args:
  wxyz: 4x1 ``sym.MatrixExpr``

Returns:
  Quaternion instance.
)doc";

inline constexpr std::string_view quaternion_squared_norm = R"doc(
The sum of squared elements of ``self``, or the squared L2 norm.

Returns:
  Scalar-valued expression ``w**2 + x**2 + y**2 + z**2``.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> geometry.Quaternion(w, x, y, z).squared_norm()
  w**2 + x**2 + y**2 + z**2
)doc";

inline constexpr std::string_view quaternion_norm = R"doc(
The L2 norm of ``self``.

Returns:
  Scalar-valued expression ``sqrt(w**2 + x**2 + y**2 + z**2)``.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> geometry.Quaternion(w, x, y, z).norm()
  (w**2 + x**2 + y**2 + z**2)**(1/2)
)doc";

inline constexpr std::string_view quaternion_normalized = R"doc(
Create a new quaternion by dividing every element of ``self`` by ``self.norm()``.

Returns:
  Quaternion with unit norm.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> q = geometry.Quaternion(w, x, y, z)
  >>> norm = q.norm()
  >>> q.normalized().subs(norm, sym.symbols('q_n'))
  Quaternion(w/q_n, x/q_n, y/q_n, z/q_n)
  >>> geometry.Quaternion(0, 1, 1, 0).normalized()
  Quaternion(0, 2**(1/2)/2, 2**(1/2)/2, 0)
)doc";

inline constexpr std::string_view quaternion_conjugate = R"doc(
Compute the quaternion conjugate. Given the quaternion:

.. math::
  \mathbf{q} = q_w + q_x\cdot\mathbf{i} + q_y\cdot\mathbf{j} + q_z\cdot\mathbf{k}

The conjugate is defined as:

.. math::
  \bar{\mathbf{q}} = q_w - q_x\cdot\mathbf{i} - q_y\cdot\mathbf{j} - q_z\cdot\mathbf{k}

If :math:`\mathbf{q}` is normalized, then :math:`\mathbf{q}\cdot\bar{\mathbf{q}}` is the identity
element.

Returns:
  Conjugated quaternion, where the signs of the ``[x, y, z]`` components have been flipped.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> q = geometry.Quaternion(w, x, y, z)
  >>> q * q.conjugate()
  Quaternion(w**2 + x**2 + y**2 + z**2, 0, 0, 0)
  >>> q = geometry.Quaternion(0, 1, 1, 0).normalized()
  >>> q.conjugate() * q
  Quaternion(1, 0, 0, 0)
)doc";

inline constexpr std::string_view quaternion_inverse = R"doc(
Compute the quaternion inverse. Given the quaternion:

.. math::
  \mathbf{q} = q_w + q_x\cdot\mathbf{i} + q_y\cdot\mathbf{j} + q_z\cdot\mathbf{k}

The inverse is defined as:

.. math::
  \mathbf{q}^{-1} = \frac{q_w}{\lvert q \rvert^2} -
                    \frac{q_x}{\lvert q \rvert^2}\cdot\mathbf{i} -
                    \frac{q_y}{\lvert q \rvert^2}\cdot\mathbf{j} -
                    \frac{q_z}{\lvert q \rvert^2}\cdot\mathbf{k}

And :math:`\mathbf{q}\cdot\mathbf{q}^{-1}` is the identity element.

Returns:
  Inverted quaternion, where the signs of the ``[x, y, z]`` components have been flipped and the
  all components are divided by the squared norm.

Examples:
  >>> q = geometry.Quaternion(4, 2, 1, -3)
  >>> q * q.inverse()
  Quaternion(1, 0, 0, 0)
)doc";

inline constexpr std::string_view quaternion_to_rotation_matrix = R"doc(
Convert quaternion to an element of SO(3), the group of rotation matrices.

Caution:
  The result of this operation is only well defined if ``self`` is a **normalized** quaternion.

Returns:
  wrenfold.sym.MatrixExpr: A 3x3 rotation matrix.

Examples:
  >>> geometry.Quaternion().to_rotation_matrix()
  [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
  >>> geometry.Quaternion(0, 1, 1, 0).normalized().to_rotation_matrix()
  [[0, 1, 0], [1, 0, 0], [0, 0, -1]]

References:
  * `Quaternions and rotations <https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation>`_.
)doc";

inline constexpr std::string_view quaternion_from_angle_axis = R"doc(
Construct quaternion from an angle and normalized axis. This function constructs the quaternion:

.. math::
  \mathbf{q} = \cos{\frac{\theta}{2}} +
               v_x \cdot \sin{\frac{\theta}{2}} \cdot \mathbf{i} +
               v_y \cdot \sin{\frac{\theta}{2}} \cdot \mathbf{j} +
               v_z \cdot \sin{\frac{\theta}{2}} \cdot \mathbf{k}

Caution:
  The resulting quaternion only represents a rotation if the axis ``[vx, vy, vz]`` is normalized.

Args:
  angle: Rotation angle in radians.
  vx: X component of the rotation axis.
  vy: Y component of the rotation axis.
  vz: Z component of the rotation axis.

Returns:
  A unit-norm quaternion representing the rotation about vector ``[vx, vy, vz]`` by ``angle``
  radians.

Examples:
  >>> angle, x, y, z = sym.symbols('angle, x, y, z')
  >>> geometry.Quaternion.from_angle_axis(angle, x, y, z)
  Quaternion(cos(angle/2), x*sin(angle/2), y*sin(angle/2), z*sin(angle/2))
)doc";

inline constexpr std::string_view quaternion_from_rotation_vector = R"doc(
Construct quaternion from a rotation vector. A rotation vector (sometimes referred to as "Rodrigues
parameters") is parallel to the axis of rotation, and has a norm equal to the angle of rotation in
radians.

Caution:
  This method encounters a singularity when the norm of ``[x, y, z]`` is zero. Refer to the note for
  argument ``epsilon`` on how this is handled.

Args:
  x: X component of the vector, in radians.
  y: Y component of the vector, in radians.
  z: Z component of the vector, in radians.
  epsilon: If provided, ``epsilon`` specifies the threshold below which a small-angle approximation
    is used. A conditional will be inserted using :func:`wrenfold.sym.where` to switch between the
    normal and small-angle code paths. This value should be positive.

Returns:
  A unit-norm quaternion representing the rotation vector ``[x, y, z]``.

Examples:
  >>> geometry.Quaternion.from_rotation_vector(sym.pi, 0, 0, epsilon=None)
  Quaternion(0, 1, 0, 0)
)doc";

inline constexpr std::string_view quaternion_from_x_angle = R"doc(
Construct quaternion that rotates about the x-axis by the specified angle.

Args:
  angle: Angle in radians.

Returns:
  A unit-norm quaternion representing a rotation about the x-axis.
)doc";

inline constexpr std::string_view quaternion_from_y_angle = R"doc(
Construct quaternion that rotates about the y-axis by the specified angle.

Args:
  angle: Angle in radians.

Returns:
  A unit-norm quaternion representing a rotation about the y-axis.
)doc";

inline constexpr std::string_view quaternion_from_z_angle = R"doc(
Construct quaternion that rotates about the z-axis by the specified angle.

Args:
  angle: Angle in radians.

Returns:
  A unit-norm quaternion representing a rotation about the z-axis.
)doc";

inline constexpr std::string_view quaternion_to_angle_axis = R"doc(
Recover a rotation angle and axis from a unit-norm quaternion. The method used is documented in
`Quaternion Computation, Neil Dantum <http://www.neil.dantam.name/note/dantam-quaternion.pdf>`_,
equation 19.

If the quaternion is near identity, recovering the normalized axis of rotation entails a division by
zero. If the norm of the vector component of the quaternion falls below ``epsilon``, the division
is skipped and a constant value of ``[0, 0, 0]`` is used instead.

Caution:
  This operation is only valid if ``self`` is normalized.

Args:
  epsilon: If specified, a conditional branch is inserted to handle the singularity for quaternions
    near identity. This value should be positive.

Returns:
  wrenfold.sym.Expr: Angle in radians, converted into range ``[0, pi]``.
  wrenfold.sym.MatrixExpr: Normalized axis of rotation.
)doc";

inline constexpr std::string_view quaternion_to_rotation_vector = R"doc(
Recover a rotation vector from a unit-norm quaternion. The following formula is used:

.. math::
  \mathbf{v} = \frac{2}{\lvert \mathbf{q}_v \rvert}
  \cdot \text{atan2}\left(\lvert \mathbf{q}_v \rvert, q_w\right)
  \cdot \mathbf{q}_v

Where :math:`\mathbf{q}_v` is the vector component of the quaternion.

Caution:
  This operation is only valid if ``self`` is normalized.

Args:
  epsilon: If provided, ``epsilon`` specifies the threshold below which a small-angle approximation
    is used. A conditional will be inserted using :func:`wrenfold.sym.where` to switch between the
    normal and small-angle code paths. This value should be positive.

Returns:
  3x1 matrix expression for the rotation vector.
)doc";

inline constexpr std::string_view quaternion_from_rotation_matrix = R"doc(
Construct a quaternion from a rotation matrix. This function uses Sheppard's method, as documented
in section 3.3 of `A Survey on the Computation of Quaternions from Rotation Matrices, Sarabandi
and Thomas <https://www.iri.upc.edu/files/scidoc/2083-A-Survey-on-the-Computation-of-Quaternions-from-Rotation-Matrices.pdf>`_.

Caution:
  This operation is only valid if ``R`` is a valid member of SO(3).

Args:
  R: 3x3 rotation matrix.

Returns:
  A unit-norm quaternion corresponding to the rotation matrix.
)doc";

inline constexpr std::string_view quaternion_right_retract_derivative = R"doc(
Compute the 4x3 derivative of this quaternion with respect to a right-multiplied tangent-space
perturbation. This is the derivative:

.. math::
  \frac{\partial \left[\mathbf{q} \cdot \text{exp}\left(\mathbf{v}\right)\right]}
    {\partial \mathbf{v}}
    \biggr\rvert_{\mathbf{v} = 0}

Where ``exp(...)`` maps from a rotation vector to a quaternion. The derivatives of the four
quaternion elements (ordered ``[w, x, y, z]``) are computed with respect to the three elements of
``v``, linearized about ``v = 0``.

Returns:
  4x3 jacobian ``d(self * exp(v)) / dv`` evaluated at ``v = 0``.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> geometry.Quaternion(w, x, y, z).right_retract_derivative()
  [[-x/2, -y/2, -z/2], [w/2, -z/2, y/2], [z/2, w/2, -x/2], [-y/2, x/2, w/2]]
)doc";

inline constexpr std::string_view quaternion_right_local_coordintes_derivative = R"doc(
Compute the 3x4 derivative:

.. math::
  \frac{\partial \text{log}\left(\bar{\mathbf{q}} \cdot \left(\mathbf{q} + \delta\mathbf{q}\right)
  \right)}{\partial \delta\mathbf{q}}\biggr\rvert_{\delta\mathbf{q} = 0}

Where ``log(...)`` converts a quaternion to a rotation vector, and ``dq`` is an additive
perturbation to ``self``. The derivative of the rotation vector is taken with respect to the four
quaternion elements (ordered ``[w, x, y, z]``).

Returns:
  3x4 jacobian of ``d(log(self^T * (self + dq)) / dq`` evaluated at ``dq = 0``.

Examples:
  >>> w, x, y, z = sym.symbols('w, x, y, z')
  >>> geometry.Quaternion(w, x, y, z).right_retract_derivative()
  [[-2*x, 2*w, 2*z, -2*y], [-2*y, -2*z, 2*w, 2*x], [-2*z, 2*y, -2*x, 2*w]]
)doc";

inline constexpr std::string_view left_jacobian_of_so3 = R"doc(
Compute the *left* jacobian of SO(3). Given a rotation vector ``w``, this method computes the
3x3 derivative:

.. math::
  \mathbf{J}_l = \frac{
    \partial \text{log}\left(
    \text{exp}\left(\mathbf{w} + \delta\mathbf{w}\right) \cdot
    \text{exp}\left(\mathbf{w}\right)^T\right)
  }
  {\partial \delta\mathbf{w}} \biggr\rvert_{\delta\mathbf{w} = 0}

Where ``exp(...)`` maps from a rotation vector to a quaternion, and ``log(...)`` maps from a
quaternion to a rotation vector.

This matrix has a secondary interpretation, as the integral:

.. math::
  \mathbf{J}_l = \int_{0}^{1} \text{exp}\left(\alpha \cdot \mathbf{w}\right) \,d\alpha

Tip:
  The right jacobian can be obtained by transposing the left jacobian.

Args:
  w: 3x1 rotation vector, in radians.
  epsilon: If provided, ``epsilon`` specifies the threshold below which a small-angle approximation
    is used. A conditional will be inserted using :func:`wrenfold.sym.where` to switch between the
    normal and small-angle code paths. This value should be positive.

Returns:
  The 3x3 left jacobian of SO(3).

References:
  * `A micro Lie theory for state estimation in robotics <https://arxiv.org/abs/1812.01537>`_
  * `Associating Uncertainty With Three-Dimensional Poses for Use in Estimation Problems <https://ieeexplore.ieee.org/document/6727494>`_
)doc";

}  // namespace  wf::docstrings
