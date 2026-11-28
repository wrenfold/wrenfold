Advanced derivatives
====================

Derivatives with respect to symbolic functions
----------------------------------------------

Derivatives can be taken with respect to any of the following:

  - Variables: ``x = sym.symbol('x')``
  - Symbolic function invocations: ``f_x = sym.Function('f')(x)``
  - Derivatives of symbolic function invocations: ``df_x = f_x.diff(x)``

For instance, consider the computation of the Lagrangian of a simple pendulum. We assume the
generalized coordinate of the system is the pendulum angle :math:`\theta`. We take
:math:`\theta = 0` to mean that the pendulum extends rightwards along the x-axis. The potential
energy of the system is given by:

.. math::
    U = m \cdot g \cdot r \cdot \sin{\theta}

.. literalinclude:: pendulum.py
    :language: python
    :start-after: potential_energy_start
    :end-before: potential_energy_end

Where :math:`r` is the length of the pendulum, :math:`m` the mass of the system, and :math:`g` the
acceleration due to gravity. The kinetic energy of the system is given by:

.. math::
    \begin{equation*}
    \begin{split}
    V &= \frac{1}{2} \cdot m \cdot \lVert \mathbf{v}\rVert^2 \\
    \mathbf{v} &= r\cdot\frac{\partial}{\partial t}\left(\cos\theta\cdot\hat{\mathbf{i}} + \sin\theta\cdot\hat{\mathbf{j}}\right)
    \end{split}
    \end{equation*}

.. literalinclude:: pendulum.py
    :language: python
    :start-after: kinetic_energy_start
    :end-before: kinetic_energy_end

We can then compute the Lagrangian :math:`L`, and the Euler-Lagrange equation of the system:

.. math::
    \begin{equation*}
    \begin{split}
    L &= T - V \\[10pt]
    0 &= \frac{\partial}{\partial t}\left(\frac{\partial}{\partial\dot{\theta}}L\right) - \frac{\partial}{\partial\theta}L \\
    0 &= g\cdot\sin\theta - r\cdot\ddot{\theta}
    \end{split}
    \end{equation*}

.. literalinclude:: pendulum.py
    :language: python
    :start-after: lagrangian_start
    :end-before: lagrangian_end

Note that while computing the equation of motion, we took the derivative with respect to both
:math:`\theta\left(t\right)` and :math:`\dot{\theta}\left(t\right)`.

Truncated derivatives
---------------------

It is sometimes useful to truncate a derivative to prevent it from propagating through some terms.
For instance, you might wish to compute a weighting function for a particular residual *without* the
weight itself contributing to the Jacobian. To that end, wrenfold includes the
:func:`~wrenfold.sym.stop_derivative` expression:

.. code:: python

    from wrenfold import sym

    c, x = sym.make_symbols('c', 'x')
    f_x = sym.Function('f')(x)

    # Geman-McClure weight function:
    w = (c**2) / (c**2 + f_x**2)
    print(sym.stop_derivative(w).diff(x)) # prints: 0

    # Weighted residual:
    residual = sym.stop_derivative(w) * f_x
    print(residual.diff(x)) # prints: Derivative(f(x), x)*StopDerivative(c**2/(c**2 + f(x)**2))

Checking a generated Jacobian
-----------------------------

Use :func:`wrenfold.testing.check_jacobian` in a Python test to compare an analytic Jacobian
with independent finite differences at a sample point. For example, to check a scalar residual
with two inputs:

.. code-block:: python

    from wrenfold.testing import check_jacobian

    check_jacobian(
        lambda x: x[0] * x[1],
        lambda x: [[x[1], x[0]]],
        at=[2.0, 3.0],
    )

For a pose, supply the update used by your solver. The pinhole camera below transforms a world
point into camera coordinates and returns a two-dimensional pixel. Its Jacobian has six columns:
three for rotation and three for translation. The check perturbs a rotated, translated pose
on the right. Reversing the multiplication order in the retraction changes the coordinate frame
and causes the check to fail.

.. literalinclude:: ../../../examples/check_camera_jacobian.py
    :language: python
    :start-at: import numpy
