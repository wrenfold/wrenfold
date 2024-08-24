//! Machine generated code.
#![cfg_attr(rustfmt, rustfmt_skip)]

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn simple_multiply_add<>(x: f64, y: f64, z: f64) -> f64
{
  // Operation counts:
  // add: 1
  // multiply: 1
  // total: 2
  
  let v02: f64 = y;
  let v01: f64 = x;
  let v00: f64 = z;
  v00 + v01 * v02
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn vector_rotation_2d<T1, T2, T3, >(theta: f64, v: &T1, v_rot: &mut T2, D_theta: Option<&mut T3>) -> ()
where
  T1: wrenfold_traits::Span2D<2, 1, ValueType = f64>,
  T2: wrenfold_traits::OutputSpan2D<2, 1, ValueType = f64>,
  T3: wrenfold_traits::OutputSpan2D<2, 1, ValueType = f64>,
{
  // Operation counts:
  // add: 2
  // branch: 1
  // call: 2
  // multiply: 4
  // negate: 2
  // total: 11
  
  let v002: f64 = theta;
  let v003: f64 = (v002).sin();
  let v001: f64 = v.get(1, 0);
  let v007: f64 = (v002).cos();
  let v006: f64 = v.get(0, 0);
  let v012: f64 = v003 * v006 + v001 * v007;
  let v009: f64 = v006 * v007 + -(v001 * v003);
  if let Some(D_theta) = D_theta {
    D_theta.set(0, 0, -v012);
    D_theta.set(1, 0, v009);
  }
  v_rot.set(0, 0, v009);
  v_rot.set(1, 0, v012);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn vector_norm_3d<T0, T1, >(v: &T0, D_v: &mut T1) -> f64
where
  T0: wrenfold_traits::Span2D<3, 1, ValueType = f64>,
  T1: wrenfold_traits::OutputSpan2D<1, 3, ValueType = f64>,
{
  // Operation counts:
  // add: 2
  // call: 1
  // divide: 1
  // multiply: 6
  // total: 10
  
  let v004: f64 = v.get(2, 0);
  let v002: f64 = v.get(1, 0);
  let v000: f64 = v.get(0, 0);
  let v007: f64 = (v000 * v000 + v002 * v002 + v004 * v004).sqrt();
  let v010: f64 = (1i64) as f64 / v007;
  D_v.set(0, 0, v000 * v010);
  D_v.set(0, 1, v002 * v010);
  D_v.set(0, 2, v004 * v010);
  v007
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn heaviside<>(x: f64) -> f64
{
  // Operation counts:
  // branch: 1
  // compare: 1
  // total: 2
  
  let v001: f64 = x;
  let v005: i64;
  if ((0i64) as f64) <= (v001) {
    v005 = 1i64;
  } else {
    v005 = 0i64;
  }
  (v005) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn exclusive_or<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // branch: 2
  // compare: 2
  // total: 4
  
  let v004: f64 = y;
  let v001: f64 = x;
  let v005: bool = ((0i64) as f64) < (v004);
  let v009: i64;
  if ((0i64) as f64) < (v001) {
    if v005 {
      v009 = 0i64;
    } else {
      v009 = 1i64;
    }
  } else {
    if v005 {
      v009 = 1i64;
    } else {
      v009 = 0i64;
    }
  }
  (v009) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn signum_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  ((0.0f64 < v00) as i64 - (v00 < 0.0f64) as i64) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn abs_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).abs()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn floor_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  ((v00).floor() as i64) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn cosh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).cosh()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn sinh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).sinh()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn tanh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).tanh()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn acosh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).acosh()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn asinh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).asinh()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn atanh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  (v00).atanh()
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn atan2_with_derivatives<>(y: f64, x: f64, D_y: &mut f64, D_x: &mut f64) -> f64
{
  // Operation counts:
  // add: 1
  // call: 1
  // divide: 1
  // multiply: 4
  // negate: 1
  // total: 8
  
  let v000: f64 = x;
  let v001: f64 = y;
  let v007: f64 = (1i64) as f64 / (v001 * v001 + v000 * v000);
  *D_y = v000 * v007;
  *D_x = -(v001 * v007);
  (v001).atan2(v000)
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn nested_conditionals_1<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // add: 2
  // branch: 2
  // call: 5
  // compare: 2
  // multiply: 2
  // negate: 1
  // total: 14
  
  let v002: f64 = x;
  let v000: f64 = y;
  let v001: f64 = (v000).abs();
  let v034: f64;
  if (v001) < ((v002).abs()) {
    let v015: f64;
    if ((0i64) as f64) < (v000) {
      v015 = (v000 * v002).cos();
    } else {
      v015 = (v002).cos() + (2i64) as f64;
    }
    v034 = v015 * (3i64) as f64 + -((v015).abs()).sqrt();
  } else {
    let v029: f64;
    if ((0i64) as f64) < (v002) {
      v029 = (v001).ln();
    } else {
      v029 = (3i64) as f64 * (v000).atan2(v002);
    }
    v034 = 0.2f64 * ((v029).abs()).powf(0.3333333333333333f64);
  }
  v034
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn nested_conditionals_2<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // add: 1
  // branch: 3
  // call: 4
  // compare: 3
  // divide: 1
  // multiply: 3
  // negate: 1
  // total: 16
  
  let v002: f64 = x;
  let v000: f64 = y;
  let v044: f64;
  if ((v000).abs()) < ((v002).abs()) {
    if ((0i64) as f64) < (v002) {
      if ((0i64) as f64) < (v000) {
        v044 = (v000 * v002 * std::f64::consts::PI).cos();
      } else {
        v044 = (((1i64) as f64 / v000) * (22i64) as f64).sin() + -(v002 * (3i64) as f64);
      }
    } else {
      v044 = (v000 * 0.4f64).atan2(v002 * 0.1f64) * (19i64) as f64 + -v000;
    }
  } else {
    v044 = ((v002 * (v000 + (2i64) as f64)).abs()).sqrt();
  }
  v044
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn create_rotation_matrix<T0, T1, T2, >(w: &T0, R: &mut T1, R_D_w: Option<&mut T2>) -> ()
where
  T0: wrenfold_traits::Span2D<3, 1, ValueType = f64>,
  T1: wrenfold_traits::OutputSpan2D<3, 3, ValueType = f64>,
  T2: wrenfold_traits::OutputSpan2D<9, 3, ValueType = f64>,
{
  // Operation counts:
  // add: 65
  // branch: 3
  // call: 4
  // compare: 1
  // divide: 4
  // multiply: 110
  // negate: 13
  // total: 200
  
  let v0007: f64 = w.get(2, 0);
  let v0003: f64 = w.get(0, 0);
  let v0005: f64 = w.get(1, 0);
  let v0487: f64 = v0007 * v0007;
  let v0479: f64 = v0003 * v0003;
  let v0475: f64 = v0005 * v0005;
  let v0009: f64 = v0475 + v0479 + v0487;
  let v0014: f64 = 0.5f64;
  let v0010: f64 = (v0009).sqrt();
  let v0024: f64 = ((1i64) as f64 + v0009 * 0.25f64).sqrt();
  let v0016: f64 = v0010 * v0014;
  let v0025: f64 = (1i64) as f64 / v0024;
  let v0017: f64 = (v0016).sin();
  let v0013: f64 = (1i64) as f64 / v0010;
  let v0478: f64 = v0014 * v0025;
  let v0472: f64 = v0013 * v0017;
  let v0037: f64 = (v0016).cos();
  let v0011: bool = (1e-16f64) < (v0010);
  let v0027: f64;
  let v0034: f64;
  let v0038: f64;
  let v0042: f64;
  if v0011 {
    v0027 = v0005 * v0472;
    v0034 = v0007 * v0472;
    v0038 = v0037;
    v0042 = v0003 * v0472;
  } else {
    v0027 = v0005 * v0478;
    v0034 = v0007 * v0478;
    v0038 = v0025;
    v0042 = v0003 * v0478;
  }
  if let Some(R_D_w) = R_D_w {
    let v0077: f64;
    let v0084: f64;
    let v0094: f64;
    let v0100: f64;
    let v0109: f64;
    let v0116: f64;
    let v0124: f64;
    let v0130: f64;
    let v0138: f64;
    if v0011 {
      let v0551: f64 = -((1i64) as f64 / (v0010 * v0010 * v0010));
      let v0528: f64 = ((1i64) as f64 / v0009) * (v0014 * v0037);
      let v0250: f64 = v0017 * v0551 + v0528;
      let v0481: f64 = v0003 * v0250;
      let v0529: f64 = (-0.5f64) * v0472;
      v0077 = v0005 * v0481;
      v0084 = v0007 * v0481;
      v0094 = v0475 * v0528 + v0017 * (v0013 + v0475 * v0551);
      v0100 = v0005 * (v0007 * v0250);
      v0109 = v0250 * v0487 + v0472;
      v0116 = v0003 * v0529;
      v0124 = v0250 * v0479 + v0472;
      v0130 = v0005 * v0529;
      v0138 = v0007 * v0529;
    } else {
      let v0075: f64 = (1i64) as f64 / (v0024 * v0024 * v0024);
      let v0473: f64 = (-0.125f64) * v0075;
      let v0505: f64 = v0003 * v0473;
      let v0114: f64 = -0.25f64;
      v0077 = v0005 * v0505;
      v0084 = v0007 * v0505;
      v0094 = v0473 * v0475 + v0478;
      v0100 = v0007 * (v0005 * v0473);
      v0109 = v0473 * v0487 + v0478;
      v0116 = v0114 * (v0003 * v0075);
      v0124 = v0473 * v0479 + v0478;
      v0130 = v0114 * (v0005 * v0075);
      v0138 = v0114 * (v0007 * v0075);
    }
    let v0289: f64 = v0038 * v0084;
    let v0361: f64 = v0038 * v0077;
    let v0295: f64 = v0038 * v0100;
    let v0536: f64 = v0289 + v0042 * v0138;
    let v0269: f64 = v0034 * v0100;
    let v0545: f64 = v0042 * v0130 + v0361;
    let v0277: f64 = v0027 * v0100;
    let v0544: f64 = v0042 * v0116 + v0038 * v0124;
    let v0313: f64 = v0034 * v0077;
    let v0300: f64 = v0027 * v0084;
    let v0257: f64 = v0034 * v0084;
    let v0299: f64 = v0042 * v0100;
    let v0306: f64 = v0042 * v0084;
    let v0539: f64 = v0034 * v0138 + v0038 * v0109;
    let v0537: f64 = v0034 * v0130 + v0295;
    let v0256: f64 = v0027 * v0077;
    let v0534: f64 = v0034 * v0116 + v0289;
    let v0287: f64 = v0042 * v0077;
    let v0538: f64 = v0295 + v0027 * v0138;
    let v0547: f64 = v0027 * v0130 + v0038 * v0094;
    let v0546: f64 = v0361 + v0027 * v0116;
    let v0268: f64 = v0027 * v0094;
    let v0341: f64 = v0042 * v0124;
    let v0532: f64 = v0269 + v0027 * v0109;
    let v0533: f64 = v0277 + v0034 * v0094;
    let v0542: f64 = v0300 + v0313;
    let v0531: f64 = v0257 + v0042 * v0109;
    let v0541: f64 = v0299 + v0313;
    let v0543: f64 = v0306 + v0034 * v0124;
    let v0278: f64 = v0034 * v0109;
    let v0540: f64 = v0299 + v0300;
    let v0530: f64 = v0256 + v0042 * v0094;
    let v0535: f64 = v0287 + v0027 * v0124;
    let v0548: f64 = -(4i64) as f64;
    R_D_w.set(0, 0, (v0256 + v0257) * v0548);
    R_D_w.set(0, 1, (v0268 + v0269) * v0548);
    R_D_w.set(0, 2, (v0277 + v0278) * v0548);
    R_D_w.set(1, 0, (2i64) as f64 * (v0534 + v0535));
    R_D_w.set(1, 1, (2i64) as f64 * (v0530 + v0537));
    R_D_w.set(1, 2, (2i64) as f64 * (v0539 + v0540));
    R_D_w.set(2, 0, (2i64) as f64 * (v0543 + -v0546));
    R_D_w.set(2, 1, (2i64) as f64 * (v0541 + -v0547));
    R_D_w.set(2, 2, (2i64) as f64 * (v0531 + -v0538));
    R_D_w.set(3, 0, (2i64) as f64 * (v0535 + -v0534));
    R_D_w.set(3, 1, (2i64) as f64 * (v0530 + -v0537));
    R_D_w.set(3, 2, (2i64) as f64 * (v0540 + -v0539));
    R_D_w.set(4, 0, (v0257 + v0341) * v0548);
    R_D_w.set(4, 1, (v0269 + v0287) * v0548);
    R_D_w.set(4, 2, (v0278 + v0306) * v0548);
    R_D_w.set(5, 0, (2i64) as f64 * (v0542 + v0544));
    R_D_w.set(5, 1, (2i64) as f64 * (v0533 + v0545));
    R_D_w.set(5, 2, (2i64) as f64 * (v0532 + v0536));
    R_D_w.set(6, 0, (2i64) as f64 * (v0543 + v0546));
    R_D_w.set(6, 1, (2i64) as f64 * (v0541 + v0547));
    R_D_w.set(6, 2, (2i64) as f64 * (v0531 + v0538));
    R_D_w.set(7, 0, (2i64) as f64 * (v0542 + -v0544));
    R_D_w.set(7, 1, (2i64) as f64 * (v0533 + -v0545));
    R_D_w.set(7, 2, (2i64) as f64 * (v0532 + -v0536));
    R_D_w.set(8, 0, (v0256 + v0341) * v0548);
    R_D_w.set(8, 1, (v0268 + v0287) * v0548);
    R_D_w.set(8, 2, (v0277 + v0306) * v0548);
  }
  let v0227: f64 = v0042 * v0042;
  let v0209: f64 = v0027 * v0027;
  let v0550: f64 = -v0038;
  let v0210: f64 = v0034 * v0034;
  let v0549: f64 = -(2i64) as f64;
  let v0232: f64 = v0027 * v0034;
  let v0218: f64 = v0034 * v0042;
  let v0215: f64 = v0027 * v0042;
  R.set(0, 0, (1i64) as f64 + (v0209 + v0210) * v0549);
  R.set(0, 1, (2i64) as f64 * (v0034 * v0550 + v0215));
  R.set(0, 2, (2i64) as f64 * (v0218 + v0027 * v0038));
  R.set(1, 0, (2i64) as f64 * (v0215 + v0034 * v0038));
  R.set(1, 1, (1i64) as f64 + (v0210 + v0227) * v0549);
  R.set(1, 2, (2i64) as f64 * (v0042 * v0550 + v0232));
  R.set(2, 0, (2i64) as f64 * (v0218 + v0027 * v0550));
  R.set(2, 1, (2i64) as f64 * (v0232 + v0038 * v0042));
  R.set(2, 2, (1i64) as f64 + (v0209 + v0227) * v0549);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn quaternion_from_matrix<T0, T1, >(R: &T0, q_xyzw: &mut T1) -> ()
where
  T0: wrenfold_traits::Span2D<3, 3, ValueType = f64>,
  T1: wrenfold_traits::OutputSpan2D<4, 1, ValueType = f64>,
{
  // Operation counts:
  // add: 13
  // branch: 3
  // call: 2
  // compare: 3
  // divide: 2
  // multiply: 10
  // negate: 5
  // total: 38
  
  let v0002: f64 = R.get(1, 1);
  let v0001: f64 = R.get(0, 0);
  let v0031: f64 = R.get(0, 1);
  let v0010: f64 = R.get(1, 2);
  let v0023: f64 = R.get(2, 0);
  let v0102: f64 = v0001 + v0002;
  let v0003: f64 = R.get(2, 2);
  let v0032: f64 = R.get(1, 0);
  let v0008: f64 = R.get(2, 1);
  let v0022: f64 = R.get(0, 2);
  let v0107: f64 = v0003 + (1i64) as f64;
  let v0007: f64 = 0.5f64;
  let v0062: f64 = v0032 + -v0031;
  let v0013: f64 = v0008 + -v0010;
  let v0048: f64 = v0022 + -v0023;
  let v0046: f64;
  let v0060: f64;
  let v0071: f64;
  let v0080: f64;
  if ((0i64) as f64) < (v0003 + v0102) {
    let v0017: f64 = (v0102 + v0107).sqrt();
    let v0092: f64 = v0007 * ((1i64) as f64 / v0017);
    v0046 = v0013 * v0092;
    v0060 = v0048 * v0092;
    v0071 = v0062 * v0092;
    v0080 = v0007 * v0017;
  } else {
    let v0028: f64 = (v0107 + -v0102).sqrt();
    let v0093: f64 = v0007 * ((1i64) as f64 / v0028);
    let v0024: f64 = v0022 + v0023;
    let v0050: f64 = v0008 + v0010;
    let v0033: f64 = v0031 + v0032;
    let v0030: f64 = v0024 * v0093;
    let v0051: f64 = v0050 * v0093;
    let v0065: f64 = v0007 * v0028;
    let v0074: f64 = v0062 * v0093;
    if (v0001) < (v0002) {
      if (v0002) < (v0003) {
        v0046 = v0030;
        v0060 = v0051;
        v0071 = v0065;
        v0080 = v0074;
      } else {
        let v0036: f64 = ((v0002 + (1i64) as f64) + -(v0001 + v0003)).sqrt();
        let v0094: f64 = v0007 * ((1i64) as f64 / v0036);
        v0046 = v0033 * v0094;
        v0060 = v0007 * v0036;
        v0071 = v0050 * v0094;
        v0080 = v0048 * v0094;
      }
    } else {
      if (v0001) < (v0003) {
        v0046 = v0030;
        v0060 = v0051;
        v0071 = v0065;
        v0080 = v0074;
      } else {
        let v0042: f64 = ((v0001 + (1i64) as f64) + -(v0002 + v0003)).sqrt();
        let v0095: f64 = v0007 * ((1i64) as f64 / v0042);
        v0046 = v0007 * v0042;
        v0060 = v0033 * v0095;
        v0071 = v0024 * v0095;
        v0080 = v0013 * v0095;
      }
    }
  }
  q_xyzw.set(0, 0, v0046);
  q_xyzw.set(1, 0, v0060);
  q_xyzw.set(2, 0, v0071);
  q_xyzw.set(3, 0, v0080);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn rotation_vector_from_matrix<T0, T1, >(R: &T0, w: &mut T1) -> ()
where
  T0: wrenfold_traits::Span2D<3, 3, ValueType = f64>,
  T1: wrenfold_traits::OutputSpan2D<3, 1, ValueType = f64>,
{
  // Operation counts:
  // add: 19
  // branch: 8
  // call: 7
  // compare: 6
  // divide: 5
  // multiply: 20
  // negate: 6
  // total: 71
  
  let v0003: f64 = R.get(1, 1);
  let v0002: f64 = R.get(0, 0);
  let v0004: f64 = R.get(2, 2);
  let v0124: f64 = v0002 + v0003;
  let v0129: f64 = v0004 + (1i64) as f64;
  let v0029: f64 = (v0129 + -v0124).sqrt();
  let v0038: f64 = R.get(0, 1);
  let v0042: f64 = ((v0002 + (1i64) as f64) + -(v0003 + v0004)).sqrt();
  let v0023: f64 = R.get(1, 2);
  let v0034: f64 = ((v0003 + (1i64) as f64) + -(v0002 + v0004)).sqrt();
  let v0011: f64 = R.get(2, 0);
  let v0008: f64 = 0.5f64;
  let v0039: f64 = R.get(1, 0);
  let v0024: f64 = R.get(2, 1);
  let v0009: f64 = R.get(0, 2);
  let v0115: f64 = v0008 * ((1i64) as f64 / v0029);
  let v0050: f64 = v0039 + -v0038;
  let v0116: f64 = v0008 * ((1i64) as f64 / v0042);
  let v0065: f64 = v0024 + -v0023;
  let v0037: bool = (v0002) < (v0004);
  let v0117: f64 = v0008 * ((1i64) as f64 / v0034);
  let v0014: f64 = v0009 + -v0011;
  let v0022: bool = (v0003) < (v0004);
  let v0021: bool = (v0002) < (v0003);
  let v0018: f64 = (v0124 + v0129).sqrt();
  let v0007: bool = ((0i64) as f64) < (v0004 + v0124);
  let v0089: f64;
  if v0007 {
    v0089 = v0008 * v0018;
  } else {
    let v0083: f64 = v0050 * v0115;
    if v0021 {
      if v0022 {
        v0089 = v0083;
      } else {
        v0089 = v0014 * v0117;
      }
    } else {
      if v0037 {
        v0089 = v0083;
      } else {
        v0089 = v0065 * v0116;
      }
    }
  }
  let v0093: i64;
  if (v0089) < ((0i64) as f64) {
    v0093 = -1i64;
  } else {
    v0093 = 1i64;
  }
  let v0047: f64;
  let v0062: f64;
  let v0074: f64;
  if v0007 {
    let v0114: f64 = v0008 * ((1i64) as f64 / v0018);
    v0047 = v0014 * v0114;
    v0062 = v0050 * v0114;
    v0074 = v0065 * v0114;
  } else {
    let v0025: f64 = v0023 + v0024;
    let v0058: f64 = v0009 + v0011;
    let v0040: f64 = v0038 + v0039;
    let v0031: f64 = v0025 * v0115;
    let v0053: f64 = v0008 * v0029;
    let v0067: f64 = v0058 * v0115;
    if v0021 {
      if v0022 {
        v0047 = v0031;
        v0062 = v0053;
        v0074 = v0067;
      } else {
        v0047 = v0008 * v0034;
        v0062 = v0025 * v0117;
        v0074 = v0040 * v0117;
      }
    } else {
      if v0037 {
        v0047 = v0031;
        v0062 = v0053;
        v0074 = v0067;
      } else {
        v0047 = v0040 * v0116;
        v0062 = v0058 * v0116;
        v0074 = v0008 * v0042;
      }
    }
  }
  let v0077: f64 = (v0047 * v0047 + v0062 * v0062 + v0074 * v0074).sqrt();
  let v0099: f64;
  if (1e-16f64) < (v0077) {
    v0099 = ((1i64) as f64 / v0077) * (v0077).atan2((v0089).abs()) * (2i64) as f64 * (v0093) as f64;
  } else {
    v0099 = (2i64 * v0093) as f64;
  }
  w.set(0, 0, v0074 * v0099);
  w.set(1, 0, v0047 * v0099);
  w.set(2, 0, v0062 * v0099);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn no_required_outputs<>(x: f64, out1: Option<&mut f64>, out2: Option<&mut f64>) -> ()
{
  // Operation counts:
  // add: 1
  // branch: 2
  // call: 2
  // multiply: 1
  // total: 6
  
  let v001: f64 = x;
  if let Some(out1) = out1 {
    *out1 = (v001).cos() + (2i64) as f64;
  }
  if let Some(out2) = out2 {
    *out2 = (2i64) as f64 * (v001).abs();
  }
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn custom_type_1<>(p: &crate::types::Point2d) -> crate::types::Point2d
{
  // Operation counts:
  // add: 1
  // branch: 1
  // call: 1
  // compare: 1
  // divide: 1
  // multiply: 4
  // total: 9
  
  let v004: f64 = p.y();
  let v002: f64 = p.x();
  let v007: f64 = (v002 * v002 + v004 * v004).sqrt();
  let v014: f64;
  let v016: f64;
  if ((0i64) as f64) < (v007) {
    let v012: f64 = (1i64) as f64 / v007;
    v014 = v002 * v012;
    v016 = v004 * v012;
  } else {
    v014 = (0i64) as f64;
    v016 = (0i64) as f64;
  }
  crate::types::Point2d::new(v014, v016)
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn custom_type_2<T3, >(theta: f64, radius: f64, out: Option<&mut crate::types::Point2d>, D_inputs: Option<&mut T3>) -> ()
where
  T3: wrenfold_traits::OutputSpan2D<2, 2, ValueType = f64>,
{
  // Operation counts:
  // branch: 2
  // call: 2
  // multiply: 2
  // negate: 1
  // total: 7
  
  let v001: f64 = theta;
  let v004: f64 = (v001).sin();
  let v000: f64 = radius;
  let v002: f64 = (v001).cos();
  let v013: f64 = v000 * v004;
  let v003: f64 = v000 * v002;
  if let Some(out) = out {
    *out = crate::types::Point2d::new(v003, v013);
  }
  if let Some(D_inputs) = D_inputs {
    D_inputs.set(0, 0, -v013);
    D_inputs.set(0, 1, v002);
    D_inputs.set(1, 0, v003);
    D_inputs.set(1, 1, v004);
  }
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn custom_type_3<>(out: &mut crate::types::Point2d) -> ()
{
  // Operation counts:
  // multiply: 2
  // total: 2
  
  *out = crate::types::Point2d::new(0.5f64 * std::f64::consts::PI, std::f64::consts::E * (3i64) as f64);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn nested_custom_type_1<>(c: &crate::types::Circle, p: &crate::types::Point2d) -> crate::types::Circle
{
  // Operation counts:
  // add: 3
  // branch: 1
  // call: 1
  // compare: 1
  // multiply: 2
  // negate: 2
  // total: 10
  
  let v002: f64 = c.center.y();
  let v001: f64 = c.center.x();
  let v012: f64 = v002 + -p.y();
  let v008: f64 = v001 + -p.x();
  let v016: f64 = c.radius;
  let v015: f64 = (v008 * v008 + v012 * v012).sqrt();
  let v018: f64;
  if (v015) <= (v016) {
    v018 = v016;
  } else {
    v018 = v015;
  }
  crate::types::Circle {
    center: crate::types::Point2d::new(v001, v002),
    radius: v018
  }
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn external_function_call_1<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // add: 1
  // call: 1
  // multiply: 2
  // total: 4
  
  let v005: f64 = y;
  let v000: f64 = x;
  v000 * crate::external_functions::external_function_1(v000 * (2i64) as f64, v005 + (-5i64) as f64) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn external_function_call_2<T0, T1, >(u: &T0, v: &T1) -> f64
where
  T0: wrenfold_traits::Span2D<2, 1, ValueType = f64>,
  T1: wrenfold_traits::Span2D<2, 1, ValueType = f64>,
{
  // Operation counts:
  // add: 2
  // call: 1
  // multiply: 2
  // total: 5
  
  let v009: f64 = v.get(1, 0);
  let v010: f64 = 1f64 + v009;
  let v003: f64 = u.get(1, 0);
  let v001: f64 = u.get(0, 0);
  let v007: f64 = v.get(0, 0);
  crate::external_functions::external_function_2(&nalgebra::SMatrix::<f64, 2, 3>::new(-2f64 + v001, v003 * v003, (1i64) as f64, v007, v010 * v010, (1i64) as f64)) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn external_function_call_3<T1, >(x: f64, v: &T1) -> nalgebra::SMatrix<f64, 2, 2>
where
  T1: wrenfold_traits::Span2D<2, 1, ValueType = f64>,
{
  // Operation counts:
  // call: 1
  // multiply: 1
  // total: 2
  
  let v000: f64 = x;
  let v004: f64 = v.get(1, 0);
  let v003: f64 = v.get(0, 0);
  let v006: nalgebra::SMatrix<f64, 2, 2> = crate::external_functions::external_function_3(&nalgebra::SMatrix::<f64, 2, 1>::new(v000, v000 * v000), &nalgebra::SMatrix::<f64, 2, 1>::new(v003, v004));
  nalgebra::SMatrix::<f64, 2, 2>::new(v006[(0, 0)], v006[(0, 1)], v006[(1, 0)], v006[(1, 1)])
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn external_function_call_4<>(a: f64, b: f64) -> f64
{
  // Operation counts:
  // add: 1
  // branch: 1
  // call: 3
  // compare: 1
  // multiply: 1
  // negate: 1
  // total: 8
  
  let v002: f64 = b;
  let v000: f64 = a;
  let v010: crate::types::Point2d = crate::external_functions::external_function_4(&crate::types::Point2d::new(v000 + -v002, v002 * (2i64) as f64));
  let v013: f64 = v010.x();
  let v011: f64 = v010.y();
  let v016: f64;
  if ((v011).abs()) < ((v013).abs()) {
    v016 = v013;
  } else {
    v016 = v011;
  }
  v016
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn external_function_call_5<>(c: &crate::types::Circle, x: f64, y: f64) -> f64
{
  // Operation counts:
  // add: 1
  // call: 1
  // multiply: 1
  // total: 3
  
  let v004: f64 = y;
  let v003: f64 = x;
  crate::external_functions::external_function_5(c, &crate::types::Circle {
    center: crate::types::Point2d::new(v003, v004),
    radius: 1f64
  }) as f64 * (2i64) as f64 + (-1i64) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn external_function_call_6<>(x: f64, y: f64) -> crate::types::Point2d
{
  // Operation counts:
  // branch: 1
  // call: 4
  // compare: 1
  // multiply: 1
  // total: 7
  
  let v002: f64 = x;
  let v000: f64 = y;
  let v005: f64;
  let v012: f64;
  if ((v000).abs()) < ((v002).abs()) {
    v005 = v002;
    v012 = v000 * (2i64) as f64;
  } else {
    v005 = v000;
    v012 = v002 * (3i64) as f64;
  }
  crate::external_functions::external_function_4(&crate::external_functions::external_function_4(&crate::types::Point2d::new(v005, v012)))
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn integer_argument_1<>(x: i64, y: f64) -> f64
{
  // Operation counts:
  // multiply: 1
  // total: 1
  
  let v00: i64 = x;
  let v01: f64 = y;
  v01 * (v00) as f64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn integer_output_values_1<>(x: i64, y: f64, baz: &mut i64) -> i64
{
  // Operation counts:
  // add: 1
  // multiply: 1
  // negate: 1
  // total: 3
  
  let v002: f64 = y;
  let v000: i64 = x;
  let v005: f64 = (v000) as f64;
  *baz = (v005 + -v002) as i64;
  (v002 * v005) as i64
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn integer_struct_member_1<>(inputs: &crate::types::MixedNumerics, scale: f64) -> f64
{
  // Operation counts:
  // branch: 1
  // call: 2
  // compare: 1
  // multiply: 1
  // total: 5
  
  let v002: i64 = inputs.mode;
  let v006: f64 = inputs.value;
  let v012: f64;
  if (1i64) == ((v002).abs()) {
    let v005: f64 = scale;
    v012 = (v005 * v006).cos();
  } else {
    v012 = (v006).sin() * (v002) as f64;
  }
  v012
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn integer_struct_member_2<>(x: f64, y: f64) -> crate::types::MixedNumerics
{
  // Operation counts:
  // add: 1
  // multiply: 1
  // negate: 1
  // total: 3
  
  let v002: f64 = y;
  let v000: f64 = x;
  crate::types::MixedNumerics {
    value: v000 + -v002,
    mode: (v000 * v002) as i64
  }
}


