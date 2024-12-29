Advanced derivatives
====================

Derivatives with respect to symbolic functions
----------------------------------------------

Derivatives can be taken with respect to any of the following:

  - Variables: ``x = sym.symbols('x')``
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

    c, x = sym.symbols('c, x')
    f_x = sym.Function('f')(x)

    # Geman-McClure weight function:
    w = (c**2) / (c**2 + f_x**2)
    print(sym.stop_derivative(w).diff(x)) # prints: 0

    # Weighted residual:
    residual = sym.stop_derivative(w) * f_x
    print(residual.diff(x)) # prints: Derivative(f(x), x)*StopDerivative(c**2/(c**2 + f(x)**2))
