//! Machine generated code.
#![cfg_attr(rustfmt, rustfmt_skip)]

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn rosenbrock<T0, T3, >(xy: &T0, a: f64, b: f64, f_D_xy: &mut T3) -> f64
where
  T0: wrenfold_traits::Span2D<2, 1, ValueType = f64>,
  T3: wrenfold_traits::OutputSpan2D<1, 2, ValueType = f64>,
{
  // Operation counts:
  // add: 5
  // multiply: 7
  // negate: 2
  // total: 14

  let v003: f64 = xy.get(0, 0);
  let v041: f64 = -v003;
  let v001: f64 = xy.get(1, 0);
  let v006: f64 = v001 + v003 * v041;
  let v000: f64 = b;
  let v036: f64 = v000 * v006;
  let v040: f64 = (2i64) as f64 * v036;
  let v008: f64 = a;
  let v010: f64 = v008 + v041;
  f_D_xy.set(0, 0, (2i64) as f64 * (v003 + -(v008 + v003 * v040)));
  f_D_xy.set(0, 1, v040);
  v006 * v036 + v010 * v010
}
