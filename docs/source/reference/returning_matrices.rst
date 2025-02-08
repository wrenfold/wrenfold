Returning matrices
==================

The :doc:`Calling generated code <integrating_code>` section covered how to integrate a simple toy
function (smoothstep with a Jacobian) into a C++ projected. Our ``step_clamped`` function generates
a C++ function with the signature:

.. code:: cpp

  template <typename Scalar, typename T1>
  Scalar step_clamped(const Scalar x, T1&& df)
  {
      auto _df = wf::make_optional_output_span<2, 1>(df);
      // ...
  }

So far, we have yielded the output ``df`` as an output argument. We do not need to know
anything about the linear algebra framework employed at runtime. The user provides the necessary
specialization of :doc:`wf::convert_to_span <../cpp_api/span>` for type ``T1``, and wrenfold fills out
the span ``_df`` with the results (default implementations are provided for :ref:`Eigen <Using Eigen>`
and :ref:`nalgebra <Rust Integration>`).

What if we want to **return** ``df`` directly, rather than passing it out as an argument? To achieve
this, we need to define a custom code formatter and override two methods. For C++, this could look
like:

.. code:: python

  from wrenfold import ast, code_generation, type_info

  class CustomCppGenerator(code_generation.CppGenerator):
    """Custom C++ generator to allow returning Eigen matrices."""

    def format_matrix_type(self, mat: type_info.MatrixType):
        """This methods specifies how we declare the return value type."""
        return f"Eigen::Matrix<double, {mat.rows}, {mat.cols}>"

    def format_construct_matrix(self, element: ast.ConstructMatrix) -> str:
        """This method specifies how we construct the return value type."""
        formatted_args = ", ".join(self.format(x) for x in element.args)
        matrix_name = f"Eigen::Matrix<double, {element.type.rows}, {element.type.cols}>"
        # In this case, we use the Eigen streaming operator:
        return f"({matrix_name}() << {formatted_args}).finished()"

Next, we alter the definition of ``step_clamped`` to return a vector with our outputs:

.. code:: python

  def step_clamped(x: type_annotations.FloatScalar):
    """The clamped smoothstep polynomial."""
    # First express the polynomials in terms of `xv`.
    xv = sym.symbols('xv', real=True)
    f = 3 * sym.pow(xv, 2) - 2 * sym.pow(xv, 3)
    df = sym.vector(f.diff(xv), f.diff(xv, 2))

    # Replace `xv` with the clamped argument.
    x_clamped = sym.min(sym.max(x, 0), 1)
    f = f.subs(xv, x_clamped)
    df = df.subs(xv, x_clamped)

    return sym.vstack([sym.vector(f), df])

Then we generate the ``step_clamped`` function again using the custom generator:

.. code:: python

  # Note: We pass CustomCppGenerator here instead:
  cpp = code_generation.generate_function(func=step_clamped, generator=CustomCppGenerator())
  print(cpp)

.. code:: cpp

  // This snippet has been formatted with clang-format for clarity.
  template <typename Scalar>
  Eigen::Matrix<double, 3, 1> step_clamped(const Scalar x) {
    // ...
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
    const Scalar v042 = -v009;
    return (Eigen::Matrix<double, 3, 1>()
                << v009 * v009 *
                      (static_cast<Scalar>(3) + static_cast<Scalar>(2) * v042),
            v009 * static_cast<Scalar>(6) * (static_cast<Scalar>(1) + v042),
            static_cast<Scalar>(6) + static_cast<Scalar>(12) * v042)
        .finished();
  }

Note that we had to define two methods on ``CustomCppGenerator``:

  1. ``format_matrix_type``, which governs how the type will appear in the return signature.
  2. ``format_construct_matrix``, which generates code that will invoke the constructor.
