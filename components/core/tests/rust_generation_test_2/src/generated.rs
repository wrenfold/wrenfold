//! Machine generated code.
#![cfg_attr(rustfmt, rustfmt_skip)]

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
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
  let v009: f64 = v005.y;
  let v008: f64 = v002.y;
  let v006: f64 = v005.x;
  let v003: f64 = v002.x;
  let v010: f64 = v008 * v009;
  let v007: f64 = v003 * v006;
  let v011: f64 = v007 + v010;
  v011
}