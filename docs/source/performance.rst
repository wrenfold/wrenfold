Runtime Performance
===================

In the following section, we assess the performance of wrenfold generated functions.

Caveats
-------

As with any benchmarking results, there are some important caveats to consider:

  * Choice of compiler, compiler version, and CPU architecture may meaningfully alter results.
  * Compiler flags (for instance, enabling ``-march=native``) can optimize *or* pessimize a given
    implementation, sometimes in unexpected ways.
  * Performance is a moving target, and results will evolve over time.

For the most accurate outcome, you should test your function in-context with a compiler and CPU
architecture that reflect your production environment.

Overview
--------

The results reported here were computed using the code in the
`wrenfold-benchmarks <https://github.com/wrenfold/wrenfold-benchmarks>`__ repository. We consider
the performance of four functions of increasing complexity:

  * ``QuatLocalCoords``: Computing the `tangent-space <https://en.wikipedia.org/wiki/Tangent_space>`__
    difference between two orientations parameterized as quaternions. This is sometimes referred to
    as the *local coordinates* operation:

    .. math::
        f(\mathbf{q}_0, \mathbf{q}_1) = \text{log}\left(\bar{\mathbf{q}}_0 \cdot \mathbf{q}_1\right)
  * ``ImuIntegration``: Performing a single step of
    `IMU preintegration <https://docs.openvins.com/propagation.html>`__.
  * ``QuatInterpolation``: Given an interpolation fraction :math:`\alpha`, compute the tangent-space
    interpolation between two quaternions:

    .. math::
        f(\mathbf{q}_0, \mathbf{q}_1, \alpha) = \mathbf{q}_0 \cdot \text{exp}\left(
            \text{log}\left(\bar{\mathbf{q}}_0 \cdot \mathbf{q}_1\right) \cdot \alpha\right)
  * ``RollingShutterCamera``: This function projects a Euclidean point into a moving rolling-shutter
    camera. The camera uses a first-order (constant velocity) motion model, and the
    `OpenCV <https://docs.opencv.org/4.x/d9/d0c/group__calib3d.html>`__ intrinsic model with radial
    and tangential distortion coefficients.

These functions are plausible sub-expressions of a `visual-inertial odometry (VIO) or
visual-inertial navigation system (VINS) <https://docs.openvins.com>`__ system, which is why they
were selected for study. For each function we also compute the tangent-space Jacobians with respect
to the input values.

Jacobian Computation
--------------------

When computing Jacobians on manifolds (for example the rotation group :math:`SO\left(3\right)`),
there are two plausible approaches that are well suited to symbolic code-generation:

  #. We can compute the Jacobians with respect to the group variables (for instance, the four
     variables :math:`\left[w, x, y, z\right]` that make up a quaternion) and then chain
     them with the Jacobian of the *retraction* operation (see section ``II.D`` of [#]_) evaluated
     around zero. We refer to this as the *chain-rule* method, which is detailed in section ``B.1``
     of the `SymForce paper <https://arxiv.org/abs/2204.07889>`__.
  #. Alternatively, we can first replace the retract operation with a first-order taylor series in
     the vector variable :math:`\delta \mathbf{x}`, substitute the series into the function, and
     then evaluate the result around :math:`\delta \mathbf{x} = 0` after computing Jacobians with
     respect to :math:`\delta \mathbf{x}`. This method is detailed in section  ``B.2`` of the
     SymForce paper. We refer to this as the *first-order* method. This approach can produce fewer
     operations in certain instances.

SymForce provides an `explicit interface <https://github.com/symforce-org/symforce/blob/main/symforce/ops/interfaces/storage.py#L11>`__
for manifolds, and defaults to taking derivatives via the first-order method. In order to perform an
apples-to-apples comparison, we apply the *chain-rule* method to both frameworks. There is nothing
to prevent the user from applying the first-order approximation to wrenfold, if they so desire. The
two code-generated implementations we evaluate are:

  * ``XXX_Wrenfold``: wrenfold symbolic expressions with chain-rule method.
  * ``XXX_SymforceChain``: SymForce symbolic expressions with chain-rule method. These are directly
    comparable to the ``_Wrenfold`` implementations.

Additionally, we compare to two additional implementations:

  * ``XXX_Handwritten``: A handwritten implementation. `Eigen <https://eigen.tuxfamily.org>`__ is
    used to provide rotation and linear-algebra operations. Jacobians are computed "GTSAM style" by
    manually chaining together derivatives for each step of the function:

    .. math::
        \frac{\partial \mathbf{f}\left(\mathbf{g}\left(\mathbf{x}\right)\right)}
          {\partial \mathbf{x}} =
        \frac{\partial \mathbf{f}\left(\mathbf{u}\right)}
          {\partial \mathbf{u}} \biggr\rvert_{\mathbf{u} = \mathbf{g}\left(\mathbf{x}\right)}
        \frac{\partial \mathbf{g}\left(\mathbf{x}\right)}
          {\partial \mathbf{x}}
  * ``XXX_Ceres``: A handwritten implementation that employs
    `Ceres auto-diff <http://ceres-solver.org/automatic_derivatives.html>`__ to compute Jacobians.

In all likelihood, a reasonably diligent author can produce a handwritten implementation that
surpasses the code-generated equivalent. We argue that the strength of code generation is the
ability to produce a *competitive* implementation in short order, allowing for quicker development
and evaluation in the context of a production system.

Results
-------

The following results were last updated with wrenfold ``v0.0.3``. We collect results for
``gcc 12.3.0-3`` and ``clang 16.0.6`` - both using optimization level ``-O3`` and ``x86_64``
architecture. The benchmark code can be found in the
`wrenfold-benchmarks <https://github.com/wrenfold/wrenfold-benchmarks>`_ repository.

We report resulting times as multiples of the wrenfold generated implementation (for the same
compiler). A multiple ``> 1.0`` indicates that the implementation under comparison is slower than
wrenfold, while a multiple ``< 1.0`` is faster. Plots are available below - a few observations
follow:

  * code-generated functions are *roughly* comparable to the handwritten implementations. For example,
    ``QuatInterpolation`` and ``ImuIntegration`` are ~5-15% faster in handwritten form under gcc [#]_,
    and ~5-10% slower than wrenfold under clang.
  * When comparing wrenfold to ``SymforceChain``, we find that:

      * For the three most complicated functions (``ImuIntegration``, ``QuatInterpolation``,
        and ``RollingShutterCamera``), wrenfold implementations are faster than SymForce.
      * For the ``QuatInterpolation`` test, the SymForce implementations require at least twice the
        time under both gcc and clang.
  * Auto-differentiated Ceres implementations are always slower than their code-generated
    equivalents, sometimes by multiples as high as 7x or 8x.

From our (evidently biased) perspective, **the primary takeaway** (with regards to performance) is
that code-generated methods are a comparable substitute for hand-rolled implementations. They can be
used to rapidly prototype mathematical functions while incurring a relatively small performance
trade-off.

.. raw:: html
    :file: _static/benchmark_plots/QuatLocalCoords-gcc.html
.. raw:: html
    :file: _static/benchmark_plots/QuatLocalCoords-clang.html

|

.. raw:: html
    :file: _static/benchmark_plots/ImuIntegration-gcc.html
.. raw:: html
    :file: _static/benchmark_plots/ImuIntegration-clang.html

|

.. raw:: html
    :file: _static/benchmark_plots/QuatInterpolation-gcc.html
.. raw:: html
    :file: _static/benchmark_plots/QuatInterpolation-clang.html

|

.. raw:: html
    :file: _static/benchmark_plots/RollingShutterCamera-gcc.html
.. raw:: html
    :file: _static/benchmark_plots/RollingShutterCamera-clang.html

.. rubric:: Footnotes

.. [#] `A micro Lie theory for state estimation in robotics <https://arxiv.org/abs/1812.01537>`__

.. [#] A comparison of the generated assembly reveals that Eigen is sometimes able to more
       effectively leverage SIMD operations, which may explain this gap. This suggests an avenue for
       possible future improvements to the code-generation framework. See this
       `related issue <https://github.com/wrenfold/wrenfold/issues/207>`__.
