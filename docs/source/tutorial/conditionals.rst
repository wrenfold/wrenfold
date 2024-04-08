Boolean expressions and conditionals
====================================

Relationals
-----------

In addition to scalar-valued expressions, we can also construct boolean-valued expressions using
relationals:

.. code:: python

    >>> x, y = sym.symbols('x, y')
    >>> condition = x < y
    >>> type(condition)
    pywrenfold.sym.BooleanExpr
    >>> print(condition.expression_tree_str())

.. code::

    Relational (<)
    ├─ Variable (x, unknown)
    └─ Variable (y, unknown)

.. warning::
    The ``__eq__`` method in python corresponds to strict equality, *not* mathematical equivalence.
    Hence the ``==`` operator will directly return ``True`` if the two expression trees are
    completely identical, and ``False`` otherwise. To construct an equivalence relational, use
    :func:`wrenfold.sym.eq` instead.

    .. code:: python

        >>> x == y
        False
        >>> sym.eq(x, y)
        x == y

Conditional logic
-----------------

Boolean-valued expressions are relevant when creating conditional expressions. A ternary conditional
can be constructed using :func:`wrenfold.sym.where`:

.. code:: python

    >>> f = sym.where(x > 0, x * y, -x / 5)
    >>> f
    where(0 < x, x*y, -x/5)  # Note that `x > 0` was canonicalized as `0 < x`.

In the example above, :math:`f\left(x\right)` is a piecewise function with a discontinuity in the
first derivative at :math:`x = 0`. At code-generation time, ``sym.where`` will be converted to an
if-else branch. This can serve a number of purposes:

* Skipping irrelevant computations.
* Avoiding singularities or calculations that would introduce ``NaN`` values.
* Implementing piecewise continuous functions.

The result of the conditional is a scalar-valued expression, so it can be differentiated:

.. code:: python

    # Take the derivative of both branches.
    >>> f.diff(x)
    where(0 < x, y, -1/5)
    # Take the derivative of both branches, twice.
    # Since both sides evaluate to zero, the conditional is immediately simplified:
    >>> f.diff(x, 2)
    0

.. note::

    When taking the derivative of ``sym.where`` expressions, the contribution of the condition
    itself is ignored and the two branches are differentiated to create a new piecewise function.
    In the example above, the true derivative of the function :math:`f\left(x\right)` is infinite
    at :math:`x = 0`. We could insert the Dirac delta function to capture this, but this result
    would not be very useful in a numerical evaluation context.

Casting booleans
----------------

Boolean expressions cannot be multiplied or added like scalars, but they can be "casted" to scalars
using the `Iverson bracket <https://en.wikipedia.org/wiki/Iverson_bracket>`_. The iverson bracket
evaluates to ``1`` if the boolean condition is true, and ``0`` if the condition is false.

.. code:: python

    >>> condition = x < y
    >>> g = sym.iverson(condition)
    >>> g
    iverson(x < y)
    # Substitute values that make the statement true:
    >>> g.subs(x, 0.23).subs(y, 4)
    1

This can be useful if you want to mask out a value without inserting a conditional:

.. code:: python

    # We can't multiply (x < 0) by sin(x) directly, but we can do this:
    >>> h = sym.iverson(x < 0) * sym.sin(x)
    >>> h
    sin(x)*iverson(x < 0)
    # Will be zero for all positive `x`:
    >>> h.subs(x, 0.7)
    0
