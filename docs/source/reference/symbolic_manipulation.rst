Symbolic manipulation
=====================

The symbolic math framework ought to feel familiar to users experienced with other symbolic tools,
such as `sympy <https://sympy.org>`_. To begin with, we will declare some input variables and
combine them into a larger expression:

.. code:: python

    from wrenfold import sym

    # Create two symbolic variables and construct an expression.
    x, y = sym.make_symbols('x', 'y')
    g = sym.cos(x * y)
    print(g)

.. code::

    cos(x*y)

.. code:: python

    # Visualize the expression tree:
    print(g.expression_tree_str())

.. code::

    Function (cos):
    └─ Multiplication:
       ├─ Variable (x, unknown)
       └─ Variable (y, unknown)

wrenfold represents mathematical operations as an expression tree. As operations are composed, the
tree grows in depth:

.. code:: python

    # Use `g` as part of a larger expression:
    f = g + x**3 * y
    f

.. code::

    x**3*y + cos(x*y)

.. code:: python

    # Visualize the expression tree:
    print(f.expression_tree_str())

.. code::

    Addition:
    ├─ Multiplication:
    │  ├─ Variable (y, unknown)
    │  └─ Power:
    │     ├─ Variable (x, unknown)
    │     └─ Integer (3)
    └─ Function (cos):
       └─ Multiplication:
          ├─ Variable (x, unknown)
          └─ Variable (y, unknown)

Symbolic expressions are **immutable**. They can be combined to form new expressions, or we can
subject them to analytical operations that create new expressions. For instance, we can easily
obtain the derivatives of ``f``:

.. code:: python

    # Compute the derivative of `f` wrt `x`:
    df = f.diff(x)
    df

.. code::

    3*x**2*y - y*sin(x*y)

.. code:: python

    # Compute the second derivative of `f` wrt `y`:
    f.diff(y, 2)

.. code::

    -x**2*cos(x*y)

Or collect powers:

.. code:: python

    # Collect powers of `y` in our derivative expression, `df`:
    df.collect(y)

.. code::

    y*(3*x**2 - sin(x*y))

Or substitute numerical constants and evaluate into a floating point value:

.. code:: python

    val = df.subs(x, sym.E).subs(y, sym.integer(1) / 3)
    print(val)

.. code::

    E**2 - sin(E/3)/3

.. code:: python

    val.eval()

.. code::

    7.126689299943595

As expressions are composed, they are automatically converted to canonical form:

.. code:: python

    # Constants are folded and coefficients are combined in additions:
    -1 + x + x + 5  # result: 4 + 2*x

    # Constants are distributed into additions:
    (7 * x + y ** 2) * sym.rational(3, 7)  # result: 3*x + 3*y**2/7

    # Common terms in multiplications are converted to powers:
    (x * y * x * x) / y  # result: x**3

    # Some power expressions simplify automatically:
    (1 / x) ** 2  # result: x**(-2)
    sym.sqrt(x) ** 2  # result: x

.. code:: python

    from wrenfold.enumerations import NumberSet

    z = sym.symbol('z', set=NumberSet.RealNonNegative)
    ((3 * z) ** 4) ** (sym.one / 4)

.. code::

    3*z

While wrenfold is not intended to be a full computer algebra system, it does support a variety of
common :doc:`functions and operations <../python_api/index>`. Symbolic expressions can be converted
:doc:`to and from SymPy <sympy_interop>` in order to perform more advanced manipulations.
