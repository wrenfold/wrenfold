Calling generated code
======================

In the :doc:`previous section <generating_code>` we created a simple symbolic smoothstep function
and then generated the equivalent code. In this section we will walk through the process of using
our new function.

.. note::

    For examples of integrating generated functions with GTSAM, Ceres, and Sophus see the
    `wrenfold-extra-examples <https://github.com/wrenfold/wrenfold-extra-examples>`_ repository.

C++
---

The ``step_clamped`` function we generated begins with:

.. code:: cpp

    template <typename Scalar, typename T1>
    Scalar step_clamped(const Scalar x, T1&& df)
    {
        auto _df = wf::make_optional_output_span<2, 1>(df);
        // ...
    }

Where ``Scalar`` is expected to be a floating point type. But what of ``T1``? This is the type of
our optional vector-valued output argument ``df``. By default, wrenfold emits a generic forwarding
reference for all output matrices and vectors. The rationale is twofold:

  * We can maximize the number of linear algebra libraries that work with generated code. The user
    need only specialize :doc:`wf::convert_to_span <../cpp_api/span>` (more on this below) for their
    preferred types.
  * With a forwarding reference we can accept non-const r-value arguments (such as temporary views
    into larger buffers).

.. tip::

    wrenfold can *also* emit functions that **directly employ Eigen types** in the function
    signature. See :ref:`Using Eigen <Using Eigen>`.

The generated function invokes ``wf::make_optional_output_span<2, 1>`` on ``df`` in order to create
a 2D span with dimensions ``(2, 1)``. Because this argument is optional, we can pass one of two
things:

  #. Any type that implements the :doc:`wf::convert_to_span <../cpp_api/span>` trait.
  #. Or ``std::nullptr_t``, which indicates we do not care about filling this output.

The span type itself resides in the wrenfold
`runtime <https://github.com/wrenfold/wrenfold/tree/main/components/runtime/wrenfold>`_, a small
header only C++17 library that provides an n-dimensional span type used to pass arguments to and
from generated functions. You should copy these headers into your project (like all of wrenfold,
they are MIT licensed) - or otherwise ensure they are on your include path.

Implementing ``convert_to_span`` for a custom matrix type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the interest of providing a complete example, we will assume you have a custom matrix type that
looks something like:

.. code:: cpp

    class simple_matrix {
    public:
     // ...
     constexpr int rows() const { return rows_; }
     constexpr int cols() const { return cols_; }
     const double* data() const { return values_.data(); }
     double* data() { return values_.data(); }
     // ...
    private:
      int rows_;
      int cols_;
      std::vector<double> values_;
    };

Next we will specialize ``wf::convert_to_span`` for ``simple_matrix``:

.. code:: cpp

    // `Dimensions` is a wf::value_pack of compile-time constants.
    template <typename Dimensions>
    struct wf::convert_to_span<Dimensions, simple_matrix> {
        // We will accept our matrix by forwarding reference, which allows us to easily handle
        // const (input) and non-const (output) matrices.
        template <typename U>
        auto convert(U&& mat) const {
            // Double check our dynamically-sized matrix matches the expected dimensions.
            assert(wf::constant_value_pack_axis_v<0, Dimensions> == mat.rows());
            assert(wf::constant_value_pack_axis_v<1, Dimensions> == mat.cols());
            // For column major our strides will be (1, rows). For row major they would be
            // (cols, 1).
            auto strides = wf::make_value_pack(wf::constant<1>{}, mat.rows());
            return wf::make_span(mat.data(), Dimensions{}, strides);
        }
    };

The example above is simplified. In practice you may wish to have different specializations for
dynamic vs. static matrices, or support a matrix type with non-contiguous data. See the
``wrenfold/span.h`` header for an example implementation for Eigen.

With our custom specialization in hand, we can call ``step_clamped`` and pass it our
``simple_matrix`` type:

.. code:: cpp

    // Fill `diff` with the optional output argument.
    simple_matrix diff(2, 1);
    const double step_1 = step_clamped(0.237, diff);

    // In cases where we do not care about the optional output, pass nullptr.
    const double step_2 = step_clamped(0.781, nullptr);

.. _Using Eigen:

Using Eigen
^^^^^^^^^^^

There are two options available when using wrenfold `Eigen <https:://https://eigen.tuxfamily.org>`_
types:

  1. A default implementation of ``wf::convert_to_span`` is provided for use with Eigen. To activate
     it, ``#define WF_SPAN_EIGEN_SUPPORT`` prior to including ``wrenfold/span.h``. This will enable
     conversion of all types that inherit from ``Eigen::MatrixBase`` or ``Eigen::QuaternionBase``.
  2. **OR**, you can you request that Eigen types be directly employed in function signatures by
     passing :py:attr:`CppMatrixTypeBehavior.Eigen` to
     :class:`~wrenfold.code_generation.CppGenerator`.

An example of the first method:

.. code:: cpp

    // We incorporate the wrenfold runtime headers in our project, and define WF_SPAN_EIGEN_SUPPORT:
    #define WF_SPAN_EIGEN_SUPPORT
    #include <wrenfold/span.h>

    // ... later at the call-site:
    Eigen::Vector2d diff{};
    const double step_1 = step_clamped(0.237, diff);

    // We can also pass views or blocks from larger matrices.
    // Place the two derivative values into the top (1, 2) corner:
    Eigen::Matrix4d buffer{};
    const double step_2 = step_clamped(0.448, buffer.topLeftCorner<1, 2>().transpose());

Alternatively, we can request Eigen types be used in the function signature:

.. code:: python

    from wrenfold import code_generation

    generator = code_generation.CppGenerator(code_generation.CppMatrixTypeBehavior.Eigen)
    cpp = code_generation.generate_function(func=step, generator=code_generation.CppGenerator())
    print(cpp)

Which produces:

.. code:: cpp

    // The output argument is an instance of `Eigen::Matrix<Scalar, 2, 1>`.
    template <typename Scalar>
    Scalar step_clamped(const Scalar x, Eigen::Matrix<Scalar, 2, 1>* const df) { /* ... */ }

The first method (generic arguments + spans) cannot yield a matrix as a return value, since there
is no way to instantiate an owning type to contain the data. The latter method will return a dense
Eigen matrix.

.. tip::

    For additional examples of using ``CppMatrixTypeBehavior.Eigen`` to request function signatures
    with Eigen types, see `rosenbrock <https://github.com/wrenfold/wrenfold/blob/main/examples/rosenbrock/rosenbrock.py>`__
    and `rotation_error <https://github.com/wrenfold/wrenfold/blob/main/examples/rotation_error/rotation_error.py>`__.

Including requisite headers
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Generated C++ functions depend directly on:

  * The C++ STL headers ``<cmath>`` and ``<cstdint>``.
  * The wrenfold runtime, a header-only C++17 library that provides the ``span`` type. The runtime
    depends on ``<tuple>`` and ``<type_traits>``.

You can add these includes to your output code manually, or use the provided convenience function:
:func:`wrenfold.code_generation.CppGenerator.apply_preamble`.

.. _Rust Integration:

Rust
----

In rust, our sample function ``step_clamped`` begins with:

.. code:: rust

    #[inline]
    #[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if,
            clippy::needless_late_init, unused_variables)]
    pub fn step_clamped<T1, >(x: f64, df: Option<&mut T1>) -> f64
    where
        T1: wrenfold_traits::OutputSpan2D<2, 1, ValueType = f64>, {
        // ...
    }

In rust, the span trait is an explicit constraint on the generic type ``T1``. The traits are defined
in the `wrenfold-traits <https://crates.io/crates/wrenfold-traits>`__ crate. In this example, we can
pass any type that implements ``OutputSpan2D`` for ``(D0 = 2, D1 = 1)``:

.. code:: rust

    /// A two-dimensional mutable output span with shape `(D0, D1)`.
    pub trait OutputSpan2D<const D0: usize, const D1: usize> {
        /// The spanned scalar type.
        type ValueType;

        /// Set element `(i, j)` to `val`.
        fn set(&mut self, i: usize, j: usize, val: Self::ValueType);
    }

A default implementation is provided for `nalgebra <https://docs.rs/nalgebra/latest/nalgebra/>`_
matrices and vectors. The ``nalgebra`` feature must enabled to use this feature.

.. warning::

    The rust code generator is currently limited to emitting functions for a single scalar type
    at once.
