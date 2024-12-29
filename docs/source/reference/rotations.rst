3D Rotations
============

`Quaternions <https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation>`__ provide a
convention for storing and composing rotations in three dimensions. wrenfold provides the symbolic
:class:`~wrenfold.geometry.Quaternion` type to represent 3D rotations.

Quaternions can be constructed **from**:

  - Rotation vectors using :func:`~wrenfold.geometry.Quaternion.from_rotation_vector`.
  - Angle-axis representation using :func:`~wrenfold.geometry.Quaternion.from_angle_axis`.
  - Rotation matrices (members of :math:`SO(3)`) using
    :func:`~wrenfold.geometry.Quaternion.from_rotation_matrix`.
  - Angles about the principal axes using :func:`~wrenfold.geometry.Quaternion.from_x_angle`,
    :func:`~wrenfold.geometry.Quaternion.from_y_angle`, and
    :func:`~wrenfold.geometry.Quaternion.from_z_angle`.

Quaternions can be converted **to**:

  - Rotation matrices using :func:`~wrenfold.geometry.Quaternion.to_rotation_matrix`.
  - Angle-axis representation using :func:`~wrenfold.geometry.Quaternion.to_angle_axis`.
  - Rotation vectors using :func:`~wrenfold.geometry.Quaternion.to_rotation_vector`.

Internally, wrenfold quaternions are stored in ``[w, x, y, z]`` (scalar-first) order. Conversion to
scalar-last representation can be effectuated using :func:`~wrenfold.geometry.Quaternion.to_vector_xyzw`
and :func:`~wrenfold.geometry.Quaternion.from_xyzw`.

.. warning::

  When converting rotation representations, some care is required to handle singularities. Many of
  the methods listed above accept an ``epsilon`` parameter that governs the behavior near
  ``angle=0``. When ``epsilon`` is specified, a conditional statement and small-angle approximation
  are inserted to safely handle cases where :math:`\lvert\theta\rvert < \epsilon`.

Tangent-space Jacobians
-----------------------

Jacobians for rotation groups are typically computed with respect to a tangent-space perturbation.
One of the conceptually simplest ways to calculate the derivative is to first compute the Jacobian
with respect to the quaternion elements :math:`\mathbf{q} = \left[q_w, q_x, q_y, q_z\right]`, and
then chain rule this with the Jacobian of the tangent-space perturbation itself:

.. math::
  \mathbf{J} = \frac{\partial \mathbf{f}\left(\mathbf{q} \oplus \mathbf{\delta v}\right)}{\partial \mathbf{\delta v}} =
    \frac{\partial \mathbf{f}\left(\mathbf{q}\right)}{\partial \mathbf{q}}
    \frac{\partial \left[\mathbf{q} \cdot \text{exp}\left(\mathbf{\delta v}\right)\right]}{\partial \mathbf{\delta v}}
    \biggr\rvert_{\mathbf{\delta v} = 0}

Where :math:`\text{exp}\left(\mathbf{v}\right)` maps from a rotation vector into a unit quaternion.
The term :math:`\mathbf{q} \oplus \mathbf{\delta v} = \mathbf{q}\cdot\text{exp}\left(\mathbf{\delta v}\right)`
is sometimes referred to as the  *retraction* operation [#]_ [#]_ - it maps the perturbation
:math:`\mathbf{\delta v}` onto the group of quaternions about :math:`\mathbf{q}`.

The Jacobian :math:`\mathbf{J}_r = \frac{\partial\mathbf{q}\cdot\text{exp}\left(\mathbf{\delta v}\right)}{\partial\mathbf{\delta v}}`
(evaluated about :math:`\mathbf{\delta v} = 0`) is available in wrenfold using
:func:`~wrenfold.geometry.Quaternion.right_retract_derivative`.

Alternatively, one can *also* replace the retraction operation with an additive first-order Taylor
series approximation. The series is substituted into the original function
:math:`\mathbf{f}\left(\mathbf{q}\right)` and then evaluated about zero: [#]_

.. math::

  \mathbf{J} = \frac{\partial \mathbf{f}\left(\mathbf{q} + \mathbf{J}_r\left(\mathbf{q}\right)\mathbf{\delta v}\right)}
               {\partial \mathbf{\delta v}}\biggr\rvert_{\mathbf{\delta v} = 0}

Where :math:`\mathbf{J}_r` is the right retraction Jacobian. Because we are evaluating about
:math:`\mathbf{\delta v} = 0`, the two methods yield equivalent results. However, the second method
can sometimes produce lower operation counts when evaluated symbolically.

The following snippet illustrates both methods:

.. literalinclude:: rotations.py
    :language: python
    :start-after: jacobian_computation_start
    :end-before: jacobian_computation_end

Local-coordinates Jacobian
^^^^^^^^^^^^^^^^^^^^^^^^^^

In the previous example it is assumed that the function :math:`\mathbf{f}` returned a vector-space
that we could directly differentiate. What if it returns a quaternion?

.. math::
  \mathbf{q}^\prime = \mathbf{f}\left(\mathbf{q}\right)

In this instance, we may want the 3x3 Jacobian mapping perturbations **from** the tangent-space of
:math:`\mathbf{q}` **to** the tangent-space of :math:`\mathbf{q}^\prime`. This Jacobian can be
expressed as:

.. math::
  \mathbf{J} = \frac{\partial \text{log}\left(\bar{\mathbf{q}^\prime} \cdot \mathbf{f}\left(\mathbf{q} \oplus \mathbf{\delta v}\right)
  \right)}{\partial \mathbf{\delta v}}\biggr\rvert_{\mathbf{\delta v} = 0}

Where :math:`\text{log}\left(\mathbf{x}\right)` converts the quaternion :math:`\mathbf{x}` to a
rotation vector (the inverse of :math:`\text{exp}`, as we defined it above), and
:math:`\bar{\mathbf{q}^\prime}` is the conjugate of :math:`\mathbf{q}^\prime`.

By applying the chain rule, we can rewrite this Jacobian as:

.. math::

  \mathbf{J} = \frac{\partial \text{log}\left(\bar{\mathbf{q}^\prime} \cdot \left(\mathbf{q}^\prime + \mathbf{\delta q}^\prime\right)
  \right)}{\partial \mathbf{\delta q}^\prime}\biggr\rvert_{\mathbf{\delta q}^\prime = 0}
  \frac{\partial \mathbf{f}\left(\mathbf{q} \oplus \mathbf{\delta v}\right)}{\partial \mathbf{\delta v}}\biggr\rvert_{\mathbf{\delta v} = 0}

wrenfold refers to the first term in :math:`\mathbf{J}` as the *local-coordinates* Jacobian. It
has dimensions ``(3, 4)`` and can be obtained by invoking
:func:`~wrenfold.geometry.Quaternion.right_local_coordinates_derivative`.

.. tip::

  Refer to the `quaternion_interpolation <https://github.com/wrenfold/wrenfold/tree/main/examples/quaternion_interpolation>`__
  example to see the above ideas implemented in practice.

.. [#] `GTSAM Concepts <https://gtsam.org/notes/GTSAM-Concepts.html>`__
.. [#] `A micro Lie theory for state estimation in robotics <https://arxiv.org/abs/1812.01537>`__
.. [#] `SymForce: Symbolic Computation and Code Generation for Robotics
 <https://arxiv.org/abs/2204.07889>`__
