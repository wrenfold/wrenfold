//! Machine generated code.
#![cfg_attr(rustfmt, rustfmt_skip)]

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, clippy::neg_multiply, unused_variables, unused_parens)]
pub fn lookup_and_compute_inner_product<>(vec: &std::vec::Vec<crate::types::StructType>, a: f64, b: f64) -> f64
{
  // Operation counts:
  // add: 1
  // call: 2
  // multiply: 2
  // total: 5
  
  let v004: f64 = b;
  let v001: f64 = a;
  let v005: crate::types::StructType = crate::external::interpolate_access(vec, v004);
  let v002: crate::types::StructType = crate::external::interpolate_access(vec, v001);
  v002.x * v005.x + v002.y * v005.y
}
