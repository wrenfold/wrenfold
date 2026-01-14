Type annotations
================

wrenfold determines the runtime type/shape of input arguments using type annotations. Consider the
function:

.. code:: python

  import wrenfold as wf
  from wrenfold import sym

  def rotate_point(angle: wf.FloatScalar, p: wf.Vector2):
      """Rotate a point in 2D counter-clockwise by an angle in radians."""
      R = sym.matrix([
        [sym.cos(angle), -sym.sin(angle)],
        [sym.sin(angle), sym.cos(angle)]
      ])
      p_rotated = R * p
      return [
          wf.OutputArg(p_rotated, name="p_rotated"),
          wf.OutputArg(p_rotated.jacobian([angle]), name="D_angle", is_optional=True)
      ]

The annotation ``wf.FloatScalar`` indicates that the runtime type for ``angle`` is a floating-point
number. The annotation ``wf.Vector2`` indicates that the runtime type for ``p`` is a 2x1 column
vector. In C++, the generated code looks like:

.. code:: cpp

  template <typename Scalar, typename T1, typename T2, typename T3>
  void rotate_point(const Scalar angle, const T1& p, T2&& p_rotated, T3&& D_angle)
  {
    auto _p = wf::make_input_span<2, 1>(p);
    auto _p_rotated = wf::make_output_span<2, 1>(p_rotated);
    auto _D_angle = wf::make_optional_output_span<2, 1>(D_angle);

    // ...
  }

The use of ``wf.Vector2`` has resulted in the runtime code
``auto _p = wf::make_input_span<2, 1>(p);``, which permits any type ``T1`` (of appropriate size)
that implements ``wf::convert_to_span`` to be passed to ``rotate_point``.

In fact, ``wf.Vector2`` is an instance of
`typing.Annotated <https://docs.python.org/3/library/typing.html#typing.Annotated>`_:

.. code:: python

  # Snippet from type_annotations.py
  Vector2 = Annotated[sym.MatrixExpr, Shape(rows=2, cols=1), "A 2x1 column vector."]

You can define custom matrix sizes in the same manner:

.. code:: python

  import typing
  import wrenfold as wf
  import wrenfold.sym as sym

  Matrix3x9 = typing.Annotated[sym.MatrixExpr, wf.Shape(rows=3, cols=9)]

  def my_func(some_input: Matrix3x9):
      # ...
