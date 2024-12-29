Interfacing with existing types
===============================

The wrenfold code-generator can integrate with existing types from your codebase, both as function
arguments and return values. This can unlock some useful functionality:

  * Leverage existing group or manifold types in your codebase. For example, if your optimization is
    implemented in `GTSAM <https://gtsam.org>`_ you might wish to pass ``gtsam::Pose3`` directly to
    and from generated functions.
  * Pass parameter structs directly to a generated function without breaking them into scalar/vector
    pieces.

In general, this can improve type-safety and legibility by eliminating type conversion clutter where
generated methods are invoked. Our overall philosophy is **BYOT** (Bring Your Own Types): Rather
than code-generate new structs, wrenfold aims to facilitate relatively easy integration of structs
that exist a priori in the user codebase.

There are two kinds of user-provided types:

    #. `dataclass <https://docs.python.org/3/library/dataclasses.html>`_ types, which we will
       outline in this section.
    #. ``Opaque`` types, which are primarily useful in combination with
       :doc:`external functions <external_functions>`.

Defining a custom type
----------------------

In order to use an externally defined struct, wrenfold needs your assistance in two places:

    #. We need a python definition of the object that lays out the struct members and their types.
    #. The code-generator *may* need to be customized to emit valid calls to member accessors and
       constructors.

In this example we will implement support for a simple ``vec2`` type. We assume the existence of a
C++ struct of the form:

.. code:: cpp

    namespace geo {
      struct vec2 {
        constexpr vec2(double x, double y) noexcept : x_(x), y_(y) {}
        // ...
        constexpr double x() const noexcept { return x_; }
        constexpr double y() const noexcept { return y_; }
        // ...
      private:
        double x_;
        double y_;
      };
    } // namespace geo

In python, custom types are declared as dataclasses with type-annotated members. We declare a
symbolic equivalent of our ``vec2`` type:

.. literalinclude:: custom_types_script.py
    :language: python
    :start-after: dataclass_declaration_start
    :end-before: dataclass_declaration_end

When defining a dataclass for use with wrenfold, all members must be type annotated with:

  * One of the types from the :doc:`type annotations <../python_api/type_annotations>` module, or
    a similarly declared type that inherits from ``sym.Expr`` or ``sym.MatrixExpr``.
  * Another custom dataclass type - thus nested structs are supported.

Next, we will create a simple example function that uses our vector type. We can accept ``Vec2`` as
an argument, and return it as well:

.. literalinclude:: custom_types_script.py
    :language: python
    :start-after: function_definition_start
    :end-before: function_definition_end

Customizing code generation
---------------------------

With our symbolic ``rotate_vector`` method in hand, we are nearly ready to generate code. First we
need to make some minor customizations to the code formatter:

.. literalinclude:: custom_types_script.py
    :language: python
    :start-after: code_generator_start
    :end-before: code_generator_end

The default C++ code-generation logic for constructors assumes initializer-list syntax, which is
already valid for our ``geo::vec2`` type - we do not need to customize that. Now we leverage our new
custom generator:

.. literalinclude:: custom_types_script.py
    :language: python
    :start-after: transpilation_start

.. code:: cpp

    template <typename Scalar>
    geo::vec2 rotate_vector(const Scalar angle, const geo::vec2& v, geo::vec2& v_rot_D_angle)
    {
      const Scalar v001 = angle;
      const Scalar v004 = v.y();
      const Scalar v002 = std::sin(v001);
      const Scalar v008 = v.x();
      const Scalar v007 = std::cos(v001);
      const Scalar v013 = v004 * v007 + v002 * v008;
      const Scalar v010 = v007 * v008 + -(v002 * v004);
      v_rot_D_angle = geo::vec2{
        -v013,
        v010
      };
      return geo::vec2{
        v010,
        v013
      };
    }

Voila - our generated methods uses ``geo::vec2`` for input arguments, output arguments, and the
return value.

Emitting a custom constructor call
----------------------------------

Suppose we want to generate this method in Rust as well, and invoke a *custom constructor*
``geo::Vec2::new(...)``. To override the construction logic, we implement
``format_construct_custom_type``:

.. literalinclude:: custom_types_script.py
    :language: python
    :start-after: rust_code_generator_start
    :end-before: rust_code_generator_end

The generated code now looks like:

.. code:: rust

  #[inline]
  #[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if,
          clippy::needless_late_init, unused_variables)]
  pub fn rotate_vector<>(angle: f64, v: &geo::Vec2, v_rot_D_angle: &mut geo::Vec2) -> geo::Vec2
  {
    let v001: f64 = angle;
    let v004: f64 = v.y();
    let v002: f64 = (v001).sin();
    let v008: f64 = v.x();
    let v007: f64 = (v001).cos();
    let v013: f64 = v004 * v007 + v002 * v008;
    let v010: f64 = v007 * v008 + -(v002 * v004);
    *v_rot_D_angle = geo::Vec2::new(-v013, v010);
    geo::Vec2::new(v010, v013)
  }

.. note::

    For a more complicated demonstration, refer to the `custom_types example
    <http://github.com/wrenfold/wrenfold/blob/main/examples/custom_types/custom_types_gen.py>`_.
