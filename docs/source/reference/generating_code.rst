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

Initially we will assume the input variable :math:`x` is already in the domain ``[0, 1]``:

.. code:: python

    from wrenfold import sym
    from wrenfold import type_annotations

    def step(x: type_annotations.FloatScalar):
        """The smoothstep polynomial."""
        return 3 * sym.pow(x, 2) - 2 * sym.pow(x, 3)

Note the presence of the type annotation on argument ``x``. This is important, as it communicates
type information required to create a function signature. In actuality, ``FloatScalar`` is just
a scalar-valued expression of type :class:`wrenfold.sym.Expr`.

.. note::

    The :doc:`type annotations <../python_api/type_annotations>` module contains a number of
    pre-declared types that can be used to annotate functions. For vector/matrix types, the user can
    always declare their own by following the pattern in
    `type_annotations.py <https://github.com/wrenfold/wrenfold/blob/main/components/python/wrenfold/type_annotations.py>`__.

Next, we can code-generate the equivalent C++:

.. code:: python

    from wrenfold import code_generation

    cpp = code_generation.generate_function(func=step, generator=code_generation.CppGenerator())
    print(cpp)

.. code:: cpp

    template <typename Scalar>
    Scalar step(const Scalar x)
    {
        // ...
        const Scalar v001 = x;
        return v001 * v001 * (static_cast<Scalar>(3) + -(v001 * static_cast<Scalar>(2)));
    }

Or Rust:

.. code:: python

    rust = code_generation.generate_function(func=step, generator=code_generation.RustGenerator())
    print(rust)

.. code:: rust

    #[inline]
    #[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if,
            clippy::needless_late_init, unused_variables)]
    pub fn step<>(x: f64) -> f64
    {
        // ...
        let v001: f64 = x;
        v001 * v001 * ((3i64) as f64 + -(v001 * (2i64) as f64))
    }

Output arguments
----------------

Let's improve our generated function by adding the first and second derivatives as an optional
output argument:

.. code:: python

    def step_deriv(x: type_annotations.FloatScalar):
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

    cpp = code_generation.generate_function(func=step_deriv, generator=code_generation.CppGenerator())
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

        // ...

        const Scalar v001 = x;
        const Scalar v034 = -v001;
        if (static_cast<bool>(_df)) {
            _df(0, 0) = v001 * static_cast<Scalar>(6) * (static_cast<Scalar>(1) + v034);
            _df(1, 0) = static_cast<Scalar>(6) + static_cast<Scalar>(12) * v034;
        }
        return v001 * v001 * (static_cast<Scalar>(3) + static_cast<Scalar>(2) * v034);
    }


Conditional logic
-----------------

Lastly, let's extend our function to automatically clamp the value of ``x`` to the valid interval.
We do this by adding calls to :func:`wrenfold.sym.min` and :func:`wrenfold.sym.max` - both of which
are shorthand for ``sym.where``:

.. code:: python

    def step_clamped(x: type_annotations.FloatScalar):
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
        Scalar v006;
        if (v002 < static_cast<Scalar>(0)) {
            v006 = static_cast<Scalar>(0);
        } else {
            v006 = v002;
        }
        Scalar v009;
        if (static_cast<Scalar>(1) < v006) {
            v009 = static_cast<Scalar>(1);
        } else {
            v009 = v006;
        }

        // ... the remainder of the function is effectively unchanged.
    }

Inspecting intermediate parts
-----------------------------

When :func:`wrenfold.code_generation.generate_function` is called, three separate steps occur:

  #. The signature of the symbolic function is inspected to determine the input types. Variable
     inputs are constructed, and the function is invoked. The symbolic expression tree is captured in
     a :class:`wrenfold.code_generation.FunctionDescription` object.
  #. The symbolic expressions are flattened into a simple intermediate representation (IR), and
     common sub-expressions are eliminated. This representation is then converted into an abstract
     syntax tree (AST).
  #. The syntax tree is emitted in a particular language (C++ for example).

If desired (perhaps while debugging), we can perform these steps separately First, we create
the ``FunctionDescription``:

.. code:: python

    from wrenfold import code_generation

    desc = code_generation.create_function_description(func=step)
    print(desc)  # prints: FunctionDescription('step', 1 args)

We can then convert it to syntax by calling :func:`wrenfold.code_generation.transpile`:

.. code:: python

    definition = code_generation.transpile(desc)
    print(definition)  # prints: FunctionDefinition('step', <1 arguments>, <8 elements>)

:class:`wrenfold.ast.FunctionDefinition` is the root of the abstract syntax tree. We can directly
inspect its contents:

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
        // ...
        const Scalar v001 = x;
        return v001 * v001 * (static_cast<Scalar>(3) + -(v001 * static_cast<Scalar>(2)));
    }

.. rubric:: Footnotes

.. [#f1] In practice, this function is simple enough that using code generation to create it might
  be overkill. We use it for this example so that the generated code is compact and legible.
