Calling external functions
==========================

It is sometimes desirable to call an external handwritten function from a code-generated one, and
use the result as part of further symbolic expressions. This can enable a few useful behaviors:

    * Evaluate complex logic that cannot easily be expressed in a functional expression tree. For
      example, solving a small numerical optimization iteratively within a larger expression.
    * Insert custom error-checking or logging logic into generated functions.
    * Interface with user-provided types that are not adequately expressed as a
      :doc:`dataclass <custom_types>`. For example, you might wish to pass a dynamically sized
      buffer of values, and perform bilinear interpolation from within the generated code.

These use cases can be achieved by declaring an external function. There are a few caveats to keep
in mind:

    * By necessity, wrenfold must assume all calls to external functions are **pure** (without any
      side effects). Any two identical calls are assumed to be interchangeable and will be
      de-duplicated during code-generation.
    * Because external functions are effectively a black box, we cannot propagate the derivative
      through them.

Declaring an external function
------------------------------

In this example, we will pass a lookup table to our generated function. To begin with, we need a
type to represent the table. We do this by inheriting from
:class:`wrenfold.type_annotations.Opaque`:

.. literalinclude:: external_functions_script.py
    :language: python
    :start-after: lookup_table_start
    :end-before: lookup_table_end

The type itself requires no further additions, it is merely a placeholder that we will later map to
a real type in the target language.

Next, we declare a function to represent our lookup operation:

.. literalinclude:: external_functions_script.py
    :language: python
    :start-after: interpolate_table_start
    :end-at: interpolate_table_end

``interpolate_table`` is an instance of :class:`wrenfold.external_functions.ExternalFunction`. We
can call it with symbolic expressions, provided they match the expected types we specified in the
``arguments`` list.

Now we can define a symbolic function that uses ``interpolate_table``. We will write a function that
computes the bearing vector between two points :math:`\mathbf{v} = \mathbf{p}_1 - \mathbf{p}_0`, and
uses the direction angle of vector
:math:`\theta = \text{atan2}\left(\mathbf{v}_y, \mathbf{v}_x\right)` as an argument to the lookup
table:

.. literalinclude:: external_functions_script.py
    :language: python
    :start-after: function_definition_start
    :end-before: function_definition_end

To emit actual code for our ``LookupTable`` type and ``interpolate_table`` function, we customize
the code generator:

.. literalinclude:: external_functions_script.py
    :language: python
    :start-after: code_generator_start
    :end-before: code_generator_end

Which produces the following C++:

.. code:: cpp

    // Our generated method correctly accepts a `std::vector<double>`, and invokes
    // the appropriately-namespaced `interpolate_table`.
    Scalar lookup_angle(const std::vector<double>& table, const T1& p_0, const T2& p_1)
    {
        auto _p_0 = wf::make_input_span<2, 1>(p_0);
        auto _p_1 = wf::make_input_span<2, 1>(p_1);

        // ...

        const Scalar v001 = _p_0(0, 0);
        const Scalar v006 = _p_0(1, 0);
        const Scalar v002 = -v001;
        const Scalar v000 = _p_1(0, 0);
        const Scalar v007 = -v006;
        const Scalar v005 = _p_1(1, 0);
        const Scalar v003 = v000 + v002;
        const Scalar v008 = v005 + v007;
        const Scalar v014 = std::atan2(v008, v003);
        const Scalar v015 = static_cast<Scalar>(M_PI) + v014;
        const Scalar v012 = static_cast<Scalar>(0.5);
        const Scalar v019 = v012 * v015;
        const Scalar v018 = static_cast<Scalar>(1) / static_cast<Scalar>(M_PI);
        const Scalar v020 = v018 * v019;
        const Scalar v009 = v008 * v008;
        const Scalar v004 = v003 * v003;
        const Scalar v021 = utilities::interpolate_table(table, v020);
        const Scalar v010 = v004 + v009;
        const Scalar v022 = v010 * v021;
        return v022;
    }

Note that nowhere did we explicitly tell wrenfold anything about the nature of
``std::vector<double>``. For the most part we would prefer not to, since it would induce a great
deal more complexity in the code-generator. Instead we treat it as an opaque type that can be
passed through the generated function to a handwritten one.
