Unit vectors
============

``wrenfold.geometry.UnitN`` represents a vector on the sphere in an ambient
space of dimension ``N``. Its tangent space has dimension ``N - 1``. ``Unit3``
is a convenience alias for the same type. Construction leaves the components
unchanged; call ``normalized()`` if the input is not already a unit vector.

The tangent frame follows ``ceres::SphereManifold``: it uses a Householder
reflection with the **last** ambient coordinate as its pivot. Thus
``retract(delta)`` and ``local_coordinates(other)`` use the same coordinates
as Ceres ``Plus`` and ``Minus``. The frame has an unavoidable discontinuity on
the sphere. At an antipodal point, local coordinates use Ceres' last tangent
axis convention. Ceres also preserves the norm of a non-unit input; ``UnitN``
matches that behavior, although the intended input is a unit vector.

For a generated Ceres cost function, compute the Jacobian with respect to the
ambient vector and return its ``N`` columns to Ceres. Ceres then multiplies
it by ``SphereManifold::PlusJacobian``. To obtain tangent derivatives directly,
multiply the ambient Jacobian by ``UnitN.retract_derivative()`` instead. These
two paths produce the same tangent Jacobian; do not apply both mappings.

.. code:: python

   from wrenfold import geometry, sym

   v = geometry.Unit3.with_name("v", 3)
   residual = v.to_vector()[0:2]
   ambient_D_v = residual.jacobian(v.to_vector())  # 2x3 for a Ceres cost function
   tangent_D_v = ambient_D_v * v.retract_derivative()  # 2x2 for direct use

The `unit3_error example <https://github.com/wrenfold/wrenfold/blob/main/examples/unit3_error/unit3_error.py>`__
constructs two ``Unit3`` values from unit-length inputs and generates a weighted
two-dimensional error with optional tangent derivatives for both inputs.

The sphere construction follows Section B.2 of
`Hertzberg et al. <https://arxiv.org/abs/1107.1119>`_. Ceres uses a different
pivot order from the paper's displayed example.
