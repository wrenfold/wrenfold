SymPy Interoperability
======================

wrenfold is not a full computer algebra system. Instead, we aim to support a limited set of common
operations required for applications like robotics, computer vision, numerical optimization, and
machine learning. In order to help supplement missing functionality, wrenfold expressions can be
converted to-and-from `SymPy <https://www.sympy.org>`_.

There are a some limitations:

    * SymPy manipulations may come with a significant performance penalty, since they all occur
      in python.
    * Not all expressions have an equivalent (in either direction of conversion). For example,
      expressions involving :doc:`custom types <custom_types>` have no equivalent in SymPy.

Example: Computing eigenvalues
------------------------------

As a motivating example, suppose we need an expression for the eigenvalues of a 3x3 matrix. There is
no function for this in wrenfold, but SymPy features an ``eigenvals()`` method that can return a
closed form expression.

.. code:: python

    >>> import sympy as sp
    >>> from wrenfold import sym
    >>> from wrenfold import sympy_conversion

    >>> m = sym.matrix_of_symbols('m', 3, 3)
    >>> m
    [[m_0_0, m_0_1, m_0_2], [m_1_0, m_1_1, m_1_2], [m_2_0, m_2_1, m_2_2]]

    >>> m_sp = sympy_conversion.to_sympy(m, sp=sp) # Convert matrix to SymPy.
    >>> m_sp
    Matrix([
    [m_0_0, m_0_1, m_0_2],
    [m_1_0, m_1_1, m_1_2],
    [m_2_0, m_2_1, m_2_2]])

    >>> ev_sp = m_sp.eigenvals(multiple=True)
    >>> sympy_conversion.from_sympy(ev_sp[0], sp=sp) # Convert eigenvalues back to wrenfold.
    m_0_0/3 + m_1_1/3 + m_2_2/3 - (3*m_0_1*m_1_0 - 3*m_0_0*m_1_1 + 3*m_0_2*m_2_0 + ...

Given the eigenvalue expressions, we can now substitute the variables in ``m`` with our choice of
expressions.

Further examples
----------------

The `cart-pole <https://github.com/wrenfold/wrenfold/tree/main/examples/cart-pole>`_ example uses
SymPy to help find the Euler-Lagrange equations of a cart-pole system featuring a double-pendulum.
