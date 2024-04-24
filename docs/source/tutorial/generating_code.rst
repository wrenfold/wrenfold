Generating code
===============

Generating a function
---------------------

Ultimately, the goal of the wrenfold framework is to provide a quicker path from expressive symbolic
math to usable code. To generate a function, the first step is to create a python function that
performs the necessary math. We will start by implementing something simple - the
`smoothstep <https://en.wikipedia.org/wiki/Smoothstep>`_ function [#f1]_:

.. math::
  f\left(x\right) = \begin{cases}
  0 & x \lt 0 \\
  3 \cdot x^2 - 2 \cdot x^3 & 0 \le x \le 1 \\
  1 & x \gt 1
  \end{cases}

At first, we will assume the input variable :math:`x` is already in the domain ``[0, 1]``:

.. code:: python

    from wrenfold import sym
    from wrenfold import type_annotations

    def step(x: type_annotations.RealScalar):
        """The smoothstep polynomial."""
        return 3 * sym.pow(x, 2) - 2 * sym.pow(x, 3)

Note the presence of the type annotation on argument ``x``. This is important, as it communicates
type information required to create a function signature. In actuality, ``RealScalar`` is just
a scalar-valued expression of type :class:`wrenfold.sym.Expr`.

.. note::

    The :doc:`type annotations <../python_api/type_annotations>` module contains a number of
    pre-declared types that can be used to annotate functions.

Next, we create a **FunctionDescription**:

.. code:: python

    from wrenfold import code_generation

    desc = code_generation.create_function_description(func=step)
    print(desc)  # prints: FunctionDescription('step', 1 args)

The :class:`wrenfold.code_generation.FunctionDescription` is built as follows:

    #. The signature of ``step(x)`` is inspected to deduce the argument types.
    #. Appropriately-typed symbolic arguments are constructed, and the function is invoked.
    #. The returned expression trees are captured.

The description still stores the function in symbolic format. We can then convert it to syntax by
calling :func:`wrenfold.code_generation.transpile`:

.. code:: python

    definition = code_generation.transpile(desc)
    print(definition)  # prints: FunctionDefinition('step', <1 arguments>, <8 elements>)

:class:`wrenfold.ast.FunctionDefinition` is the root of the abstract syntax tree. If desired, we can
directly inspect its contents:

.. code:: python

    >>> definition.signature.name
    'step'
    >>> definition.signature.arguments
    [Argument(x: floating_point)]
    >>> definition.body[1]
    Declaration(v001: floating_point = GetArgument(x))
    >>> definition.body[2]
    Declaration(v002: floating_point = Multiply(VariableRef(v001), VariableRef(v001)))

In most cases this will not be necessary. Instead, We can pass the AST directly to a generator to
obtain code:

.. code:: python

    generator = code_generation.CppGenerator()
    cpp = generator.generate(definition)
    print(cpp)

.. code:: cpp

    template <typename Scalar>
    Scalar step(const Scalar x)
    {
        // Operation counts:
        // add: 1
        // multiply: 4
        // total: 5

        const Scalar v001 = x;
        const Scalar v002 = v001 * v001;
        const Scalar v003 = v001 * v002;
        const Scalar v009 = v002 * static_cast<Scalar>(3);
        const Scalar v005 = v003 * static_cast<Scalar>(-2);
        const Scalar v010 = v005 + v009;
        return v010;
    }

We have done three steps so far:

  #. Capture our symbolic function in a function description.
  #. Transpile into an abstract syntax tree.
  #. Emit that syntax tree in a particular language (C++).

In some cases, you might wish to repeat the final step for many different target languages. If not,
all three steps can be done with a single call to
:func:`wrenfold.code_generation.generate_function`:

.. code:: python

    cpp = code_generation.generate_function(func=step)

Output arguments
----------------

Let's improve our generated function by adding the first and second derivatives as an optional
output argument:

.. code:: python

    def step_deriv(x: type_annotations.RealScalar):
        """The smoothstep polynomial."""
        f = 3 * sym.pow(x, 2) - 2 * sym.pow(x, 3)
        # Place the first and second derivative into a 2x1 vector:
        df = sym.vector(f.diff(x), f.diff(x, 2))
        # Because we are now producing multiple outputs, we need to indicate which one is the
        # return value, and which should be an output argument:
        return [
            code_generation.ReturnValue(f),
            code_generation.OutputArg(df, name="df", is_optional=True)
        ]

    desc = code_generation.create_function_description(step_deriv)
    definition = code_generation.transpile(desc)
    cpp = generator.generate(definition)
    print(cpp)

The key distinction here is that our symbolic function now returns a sequence of
:class:`wrenfold.code_generation.ReturnValue` and :class:`wrenfold.code_generation.OutputArg`
objects. Presently, wrenfold only supports one return value (but many output arguments).

We make the ``df`` argument optional by passing ``is_optional=True`` to ``OutputArg`` on
construction. The resulting function will then only compute values required for ``df`` when the
argument is present. The C++ code looks like:

.. code:: cpp

    template <typename Scalar, typename T1>
    Scalar step_deriv(const Scalar x, T1&& df)
    {
        auto _df = wf::make_optional_output_span<2, 1>(df);

        // Operation counts:
        // add: 3
        // branch: 1
        // multiply: 7
        // total: 11

        const Scalar v001 = x;
        const Scalar v002 = v001 * v001;
        if (static_cast<bool>(_df)) {
            const Scalar v021 = v001 * static_cast<Scalar>(-12);
            const Scalar v017 = v001 * static_cast<Scalar>(6);
            const Scalar v014 = v002 * static_cast<Scalar>(-6);
            const Scalar v022 = static_cast<Scalar>(6) + v021;
            const Scalar v018 = v014 + v017;
            _df(0, 0) = v018;
            _df(1, 0) = v022;
        }
        const Scalar v003 = v001 * v002;
        const Scalar v009 = v002 * static_cast<Scalar>(3);
        const Scalar v005 = v003 * static_cast<Scalar>(-2);
        const Scalar v010 = v005 + v009;
        return v010;
    }


Conditional logic
-----------------

Lastly, let's extend our function to automatically clamp the value of ``x`` to the valid interval.
We do this by adding calls to :func:`wrenfold.sym.min` and :func:`wrenfold.sym.max` - both of which
are shorthand for ``sym.where``:

.. code:: python

    def step_clamped(x: type_annotations.RealScalar):
        """The clamped smoothstep polynomial."""
        # First express the polynomials in terms of `xv`.
        xv = sym.symbols('xv', real=True)
        f = 3 * sym.pow(xv, 2) - 2 * sym.pow(xv, 3)
        df = sym.vector(f.diff(xv), f.diff(xv, 2))
        # Replace `xv` with the clamped argument. By doing things in this order we get a neater
        # result (since we don't need to differentiate the clamping).
        x_clamped = sym.min(sym.max(x, 0), 1)
        f = f.subs(xv, x_clamped)
        df = df.subs(xv, x_clamped)
        return [
            code_generation.ReturnValue(f),
            code_generation.OutputArg(df, name="df", is_optional=True)
        ]

The output code (truncated here) now includes the clamping logic as well:

.. code:: cpp

    template <typename Scalar, typename T1>
    Scalar step_clamped(const Scalar x, T1&& df)
    {
        auto _df = wf::make_optional_output_span<2, 1>(df);

        const Scalar v002 = x;
        const bool v005 = v002 < static_cast<Scalar>(0);
        Scalar v006;
        if (v005) {
            v006 = static_cast<Scalar>(0);
        } else {
            v006 = v002;
        }
        const bool v008 = static_cast<Scalar>(1) < v006;
        Scalar v009;
        if (v008) {
            v009 = static_cast<Scalar>(1);
        } else {
            v009 = v006;
        }
        const Scalar v010 = v009 * v009;

        // ... the remainder of the function is effectively unchanged.
    }

.. rubric:: Footnotes

.. [#f1] In practice, this function is simple enough that using code generation to write it is
  overkill.
