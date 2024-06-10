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
  let v03: f64 = v01 * v02;
  let v00: f64 = z;
  let v04: f64 = v00 + v03;
  v04
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
  let v022: f64 = v001 * v003;
  let v011: f64 = v001 * v007;
  let v010: f64 = v003 * v006;
  let v025: f64 = -v022;
  let v008: f64 = v006 * v007;
  let v012: f64 = v010 + v011;
  let v009: f64 = v008 + v025;
  if let Some(D_theta) = D_theta {
    let v024: f64 = -v012;
    D_theta.set(0, 0, v024);
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
  
  let v002: f64 = v.get(1, 0);
  let v000: f64 = v.get(0, 0);
  let v003: f64 = v002 * v002;
  let v001: f64 = v000 * v000;
  let v004: f64 = v.get(2, 0);
  let v016: f64 = v001 + v003;
  let v005: f64 = v004 * v004;
  let v017: f64 = v005 + v016;
  let v007: f64 = (v017).sqrt();
  let v011: f64 = (1i64) as f64 / v007;
  let v014: f64 = v004 * v011;
  let v013: f64 = v002 * v011;
  let v012: f64 = v000 * v011;
  D_v.set(0, 0, v012);
  D_v.set(0, 1, v013);
  D_v.set(0, 2, v014);
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
  let v003: bool = ((0i64) as f64) <= (v001);
  let v005: i64;
  if v003 {
    v005 = 1i64;
  } else {
    v005 = 0i64;
  }
  let v006: f64 = (v005) as f64;
  v006
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
  let v003: bool = ((0i64) as f64) < (v001);
  let v009: i64;
  if v003 {
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
  let v010: f64 = (v009) as f64;
  v010
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn signum_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: i64 = (0.0f64 < v00) as i64 - (v00 < 0.0f64) as i64;
  let v02: f64 = (v01) as f64;
  v02
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn abs_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).abs();
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn floor_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: i64 = (v00).floor() as i64;
  let v02: f64 = (v01) as f64;
  v02
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn cosh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).cosh();
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn sinh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).sinh();
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn tanh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).tanh();
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn acosh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).acosh();
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn asinh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).asinh();
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn atanh_test<>(x: f64) -> f64
{
  // Operation counts:
  // call: 1
  // total: 1
  
  let v00: f64 = x;
  let v01: f64 = (v00).atanh();
  v01
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
  
  let v001: f64 = x;
  let v000: f64 = y;
  let v005: f64 = v001 * v001;
  let v004: f64 = v000 * v000;
  let v006: f64 = v004 + v005;
  let v009: f64 = (1i64) as f64 / v006;
  let v016: f64 = v000 * v009;
  let v010: f64 = v001 * v009;
  let v018: f64 = -v016;
  let v002: f64 = (v000).atan2(v001);
  *D_y = v010;
  *D_x = v018;
  v002
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
  let v003: f64 = (v002).abs();
  let v001: f64 = (v000).abs();
  let v004: bool = (v001) < (v003);
  let v034: f64;
  if v004 {
    let v008: bool = ((0i64) as f64) < (v000);
    let v015: f64;
    if v008 {
      let v009: f64 = v000 * v002;
      v015 = (v009).cos();
    } else {
      let v012: f64 = (v002).cos();
      v015 = v012 + (2i64) as f64;
    }
    let v016: f64 = (v015).abs();
    let v017: f64 = (v016).sqrt();
    let v036: f64 = -v017;
    let v022: f64 = v015 * (3i64) as f64;
    v034 = v022 + v036;
  } else {
    let v025: bool = ((0i64) as f64) < (v002);
    let v029: f64;
    if v025 {
      v029 = (v001).ln();
    } else {
      let v027: f64 = (v000).atan2(v002);
      v029 = (3i64) as f64 * v027;
    }
    let v030: f64 = (v029).abs();
    let v032: f64 = (v030).powf(0.3333333333333333f64);
    let v024: f64 = 0.2f64;
    v034 = v024 * v032;
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
  let v003: f64 = (v002).abs();
  let v001: f64 = (v000).abs();
  let v004: bool = (v001) < (v003);
  let v044: f64;
  if v004 {
    let v007: bool = ((0i64) as f64) < (v002);
    if v007 {
      let v008: bool = ((0i64) as f64) < (v000);
      if v008 {
        let v048: f64 = v000 * v002;
        let v049: f64 = std::f64::consts::PI * v048;
        v044 = (v049).cos();
      } else {
        let v020: f64 = (1i64) as f64 / v000;
        let v050: f64 = v002 * (3i64) as f64;
        let v022: f64 = v020 * (22i64) as f64;
        let v053: f64 = -v050;
        let v023: f64 = (v022).sin();
        v044 = v023 + v053;
      }
    } else {
      let v031: f64 = v002 * 0.1f64;
      let v029: f64 = v000 * 0.4f64;
      let v032: f64 = (v029).atan2(v031);
      let v052: f64 = -v000;
      let v034: f64 = v032 * (19i64) as f64;
      v044 = v034 + v052;
    }
  } else {
    let v046: f64 = v000 + (2i64) as f64;
    let v047: f64 = v002 * v046;
    let v042: f64 = (v047).abs();
    v044 = (v042).sqrt();
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
  
  let v0005: f64 = w.get(1, 0);
  let v0003: f64 = w.get(0, 0);
  let v0006: f64 = v0005 * v0005;
  let v0004: f64 = v0003 * v0003;
  let v0007: f64 = w.get(2, 0);
  let v0550: f64 = v0004 + v0006;
  let v0008: f64 = v0007 * v0007;
  let v0551: f64 = v0008 + v0550;
  let v0019: f64 = 0.25f64;
  let v0205: f64 = v0019 * v0551;
  let v0206: f64 = (1i64) as f64 + v0205;
  let v0014: f64 = 0.5f64;
  let v0010: f64 = (v0551).sqrt();
  let v0024: f64 = (v0206).sqrt();
  let v0016: f64 = v0010 * v0014;
  let v0025: f64 = (1i64) as f64 / v0024;
  let v0017: f64 = (v0016).sin();
  let v0013: f64 = (1i64) as f64 / v0010;
  let v0476: f64 = v0014 * v0025;
  let v0470: f64 = v0013 * v0017;
  let v0037: f64 = (v0016).cos();
  let v0011: bool = (1e-16f64) < (v0010);
  let v0027: f64;
  let v0034: f64;
  let v0038: f64;
  let v0042: f64;
  if v0011 {
    v0027 = v0005 * v0470;
    v0034 = v0007 * v0470;
    v0038 = v0037;
    v0042 = v0003 * v0470;
  } else {
    v0027 = v0005 * v0476;
    v0034 = v0007 * v0476;
    v0038 = v0025;
    v0042 = v0003 * v0476;
  }
  if let Some(R_D_w) = R_D_w {
    let v0076: f64;
    let v0083: f64;
    let v0093: f64;
    let v0099: f64;
    let v0108: f64;
    let v0115: f64;
    let v0123: f64;
    let v0129: f64;
    let v0137: f64;
    if v0011 {
      let v0528: f64 = v0010 * v0010;
      let v0529: f64 = v0010 * v0528;
      let v0065: f64 = (1i64) as f64 / v0529;
      let v0519: f64 = v0014 * v0037;
      let v0067: f64 = (1i64) as f64 / v0551;
      let v0556: f64 = -v0065;
      let v0526: f64 = v0067 * v0519;
      let v0246: f64 = v0017 * v0556;
      let v0258: f64 = v0006 * v0556;
      let v0248: f64 = v0246 + v0526;
      let v0259: f64 = v0013 + v0258;
      let v0111: f64 = -0.5f64;
      let v0479: f64 = v0003 * v0248;
      let v0260: f64 = v0017 * v0259;
      let v0087: f64 = v0006 * v0526;
      let v0487: f64 = v0007 * v0248;
      let v0273: f64 = v0008 * v0248;
      let v0495: f64 = v0111 * v0470;
      let v0282: f64 = v0004 * v0248;
      v0076 = v0005 * v0479;
      v0083 = v0007 * v0479;
      v0093 = v0087 + v0260;
      v0099 = v0005 * v0487;
      v0108 = v0273 + v0470;
      v0115 = v0003 * v0495;
      v0123 = v0282 + v0470;
      v0129 = v0005 * v0495;
      v0137 = v0007 * v0495;
    } else {
      let v0530: f64 = v0024 * v0024;
      let v0531: f64 = v0024 * v0530;
      let v0074: f64 = (1i64) as f64 / v0531;
      let v0070: f64 = -0.125f64;
      let v0471: f64 = v0070 * v0074;
      let v0480: f64 = v0003 * v0471;
      let v0090: f64 = v0006 * v0471;
      let v0482: f64 = v0005 * v0471;
      let v0106: f64 = v0008 * v0471;
      let v0472: f64 = v0003 * v0074;
      let v0113: f64 = -0.25f64;
      let v0121: f64 = v0004 * v0471;
      let v0474: f64 = v0005 * v0074;
      let v0475: f64 = v0007 * v0074;
      v0076 = v0005 * v0480;
      v0083 = v0007 * v0480;
      v0093 = v0090 + v0476;
      v0099 = v0007 * v0482;
      v0108 = v0106 + v0476;
      v0115 = v0113 * v0472;
      v0123 = v0121 + v0476;
      v0129 = v0113 * v0474;
      v0137 = v0113 * v0475;
    }
    let v0363: f64 = v0042 * v0137;
    let v0287: f64 = v0038 * v0083;
    let v0359: f64 = v0038 * v0076;
    let v0356: f64 = v0042 * v0129;
    let v0353: f64 = v0038 * v0123;
    let v0352: f64 = v0042 * v0115;
    let v0299: f64 = v0038 * v0108;
    let v0296: f64 = v0034 * v0137;
    let v0293: f64 = v0038 * v0099;
    let v0290: f64 = v0034 * v0129;
    let v0284: f64 = v0034 * v0115;
    let v0383: f64 = v0027 * v0137;
    let v0377: f64 = v0038 * v0093;
    let v0376: f64 = v0027 * v0129;
    let v0371: f64 = v0027 * v0115;
    let v0538: f64 = v0287 + v0363;
    let v0364: f64 = v0027 * v0108;
    let v0267: f64 = v0034 * v0099;
    let v0547: f64 = v0356 + v0359;
    let v0358: f64 = v0034 * v0093;
    let v0275: f64 = v0027 * v0099;
    let v0546: f64 = v0352 + v0353;
    let v0311: f64 = v0034 * v0076;
    let v0298: f64 = v0027 * v0083;
    let v0316: f64 = v0042 * v0108;
    let v0255: f64 = v0034 * v0083;
    let v0297: f64 = v0042 * v0099;
    let v0305: f64 = v0034 * v0123;
    let v0304: f64 = v0042 * v0083;
    let v0541: f64 = v0296 + v0299;
    let v0539: f64 = v0290 + v0293;
    let v0291: f64 = v0042 * v0093;
    let v0254: f64 = v0027 * v0076;
    let v0536: f64 = v0284 + v0287;
    let v0286: f64 = v0027 * v0123;
    let v0285: f64 = v0042 * v0076;
    let v0540: f64 = v0293 + v0383;
    let v0549: f64 = v0376 + v0377;
    let v0548: f64 = v0359 + v0371;
    let v0266: f64 = v0027 * v0093;
    let v0339: f64 = v0042 * v0123;
    let v0559: f64 = -v0538;
    let v0534: f64 = v0267 + v0364;
    let v0564: f64 = -v0547;
    let v0535: f64 = v0275 + v0358;
    let v0563: f64 = -v0546;
    let v0544: f64 = v0298 + v0311;
    let v0533: f64 = v0255 + v0316;
    let v0543: f64 = v0297 + v0311;
    let v0545: f64 = v0304 + v0305;
    let v0276: f64 = v0034 * v0108;
    let v0562: f64 = -v0541;
    let v0542: f64 = v0297 + v0298;
    let v0560: f64 = -v0539;
    let v0532: f64 = v0254 + v0291;
    let v0558: f64 = -v0536;
    let v0537: f64 = v0285 + v0286;
    let v0561: f64 = -v0540;
    let v0566: f64 = -v0549;
    let v0565: f64 = -v0548;
    let v0554: f64 = -(4i64) as f64;
    let v0414: f64 = v0275 + v0304;
    let v0410: f64 = v0266 + v0285;
    let v0406: f64 = v0254 + v0339;
    let v0468: f64 = v0534 + v0559;
    let v0463: f64 = v0535 + v0564;
    let v0458: f64 = v0544 + v0563;
    let v0384: f64 = v0533 + v0540;
    let v0378: f64 = v0543 + v0549;
    let v0372: f64 = v0545 + v0548;
    let v0366: f64 = v0534 + v0538;
    let v0360: f64 = v0535 + v0547;
    let v0354: f64 = v0544 + v0546;
    let v0348: f64 = v0276 + v0304;
    let v0344: f64 = v0267 + v0285;
    let v0340: f64 = v0255 + v0339;
    let v0453: f64 = v0542 + v0562;
    let v0448: f64 = v0532 + v0560;
    let v0443: f64 = v0537 + v0558;
    let v0438: f64 = v0533 + v0561;
    let v0433: f64 = v0543 + v0566;
    let v0428: f64 = v0545 + v0565;
    let v0300: f64 = v0541 + v0542;
    let v0294: f64 = v0532 + v0539;
    let v0288: f64 = v0536 + v0537;
    let v0277: f64 = v0275 + v0276;
    let v0268: f64 = v0266 + v0267;
    let v0256: f64 = v0254 + v0255;
    let v0415: f64 = v0414 * v0554;
    let v0411: f64 = v0410 * v0554;
    let v0407: f64 = v0406 * v0554;
    let v0403: f64 = (2i64) as f64 * v0468;
    let v0397: f64 = (2i64) as f64 * v0463;
    let v0391: f64 = (2i64) as f64 * v0458;
    let v0385: f64 = (2i64) as f64 * v0384;
    let v0379: f64 = (2i64) as f64 * v0378;
    let v0373: f64 = (2i64) as f64 * v0372;
    let v0367: f64 = (2i64) as f64 * v0366;
    let v0361: f64 = (2i64) as f64 * v0360;
    let v0355: f64 = (2i64) as f64 * v0354;
    let v0349: f64 = v0348 * v0554;
    let v0345: f64 = v0344 * v0554;
    let v0341: f64 = v0340 * v0554;
    let v0337: f64 = (2i64) as f64 * v0453;
    let v0331: f64 = (2i64) as f64 * v0448;
    let v0325: f64 = (2i64) as f64 * v0443;
    let v0319: f64 = (2i64) as f64 * v0438;
    let v0313: f64 = (2i64) as f64 * v0433;
    let v0307: f64 = (2i64) as f64 * v0428;
    let v0301: f64 = (2i64) as f64 * v0300;
    let v0295: f64 = (2i64) as f64 * v0294;
    let v0289: f64 = (2i64) as f64 * v0288;
    let v0278: f64 = v0277 * v0554;
    let v0269: f64 = v0268 * v0554;
    let v0257: f64 = v0256 * v0554;
    R_D_w.set(0, 0, v0257);
    R_D_w.set(0, 1, v0269);
    R_D_w.set(0, 2, v0278);
    R_D_w.set(1, 0, v0289);
    R_D_w.set(1, 1, v0295);
    R_D_w.set(1, 2, v0301);
    R_D_w.set(2, 0, v0307);
    R_D_w.set(2, 1, v0313);
    R_D_w.set(2, 2, v0319);
    R_D_w.set(3, 0, v0325);
    R_D_w.set(3, 1, v0331);
    R_D_w.set(3, 2, v0337);
    R_D_w.set(4, 0, v0341);
    R_D_w.set(4, 1, v0345);
    R_D_w.set(4, 2, v0349);
    R_D_w.set(5, 0, v0355);
    R_D_w.set(5, 1, v0361);
    R_D_w.set(5, 2, v0367);
    R_D_w.set(6, 0, v0373);
    R_D_w.set(6, 1, v0379);
    R_D_w.set(6, 2, v0385);
    R_D_w.set(7, 0, v0391);
    R_D_w.set(7, 1, v0397);
    R_D_w.set(7, 2, v0403);
    R_D_w.set(8, 0, v0407);
    R_D_w.set(8, 1, v0411);
    R_D_w.set(8, 2, v0415);
  }
  let v0225: f64 = v0042 * v0042;
  let v0207: f64 = v0027 * v0027;
  let v0557: f64 = -v0038;
  let v0208: f64 = v0034 * v0034;
  let v0555: f64 = -(2i64) as f64;
  let v0243: f64 = v0207 + v0225;
  let v0525: f64 = v0038 * v0042;
  let v0230: f64 = v0027 * v0034;
  let v0234: f64 = v0027 * v0557;
  let v0216: f64 = v0034 * v0042;
  let v0229: f64 = v0042 * v0557;
  let v0226: f64 = v0208 + v0225;
  let v0523: f64 = v0034 * v0038;
  let v0213: f64 = v0027 * v0042;
  let v0521: f64 = v0027 * v0038;
  let v0212: f64 = v0034 * v0557;
  let v0209: f64 = v0207 + v0208;
  let v0244: f64 = v0243 * v0555;
  let v0239: f64 = v0230 + v0525;
  let v0235: f64 = v0216 + v0234;
  let v0231: f64 = v0229 + v0230;
  let v0227: f64 = v0226 * v0555;
  let v0222: f64 = v0213 + v0523;
  let v0218: f64 = v0216 + v0521;
  let v0214: f64 = v0212 + v0213;
  let v0210: f64 = v0209 * v0555;
  let v0245: f64 = (1i64) as f64 + v0244;
  let v0240: f64 = (2i64) as f64 * v0239;
  let v0236: f64 = (2i64) as f64 * v0235;
  let v0232: f64 = (2i64) as f64 * v0231;
  let v0228: f64 = (1i64) as f64 + v0227;
  let v0223: f64 = (2i64) as f64 * v0222;
  let v0219: f64 = (2i64) as f64 * v0218;
  let v0215: f64 = (2i64) as f64 * v0214;
  let v0211: f64 = (1i64) as f64 + v0210;
  R.set(0, 0, v0211);
  R.set(0, 1, v0215);
  R.set(0, 2, v0219);
  R.set(1, 0, v0223);
  R.set(1, 1, v0228);
  R.set(1, 2, v0232);
  R.set(2, 0, v0236);
  R.set(2, 1, v0240);
  R.set(2, 2, v0245);
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
  let v0101: f64 = v0001 + v0002;
  let v0003: f64 = R.get(2, 2);
  let v0111: f64 = -v0031;
  let v0032: f64 = R.get(1, 0);
  let v0109: f64 = -v0010;
  let v0008: f64 = R.get(2, 1);
  let v0110: f64 = -v0023;
  let v0022: f64 = R.get(0, 2);
  let v0107: f64 = v0003 + v0101;
  let v0106: f64 = v0003 + (1i64) as f64;
  let v0007: f64 = 0.5f64;
  let v0062: f64 = v0032 + v0111;
  let v0013: f64 = v0008 + v0109;
  let v0048: f64 = v0022 + v0110;
  let v0006: bool = ((0i64) as f64) < (v0107);
  let v0046: f64;
  let v0060: f64;
  let v0071: f64;
  let v0080: f64;
  if v0006 {
    let v0016: f64 = v0101 + v0106;
    let v0017: f64 = (v0016).sqrt();
    let v0018: f64 = (1i64) as f64 / v0017;
    let v0091: f64 = v0007 * v0018;
    v0046 = v0013 * v0091;
    v0060 = v0048 * v0091;
    v0071 = v0062 * v0091;
    v0080 = v0007 * v0017;
  } else {
    let v0112: f64 = -v0101;
    let v0084: f64 = v0106 + v0112;
    let v0028: f64 = (v0084).sqrt();
    let v0029: f64 = (1i64) as f64 / v0028;
    let v0092: f64 = v0007 * v0029;
    let v0024: f64 = v0022 + v0023;
    let v0050: f64 = v0008 + v0010;
    let v0033: f64 = v0031 + v0032;
    let v0030: f64 = v0024 * v0092;
    let v0051: f64 = v0050 * v0092;
    let v0065: f64 = v0007 * v0028;
    let v0074: f64 = v0062 * v0092;
    let v0020: bool = (v0001) < (v0002);
    if v0020 {
      let v0021: bool = (v0002) < (v0003);
      if v0021 {
        v0046 = v0030;
        v0060 = v0051;
        v0071 = v0065;
        v0080 = v0074;
      } else {
        let v0102: f64 = v0001 + v0003;
        let v0113: f64 = -v0102;
        let v0105: f64 = v0002 + (1i64) as f64;
        let v0087: f64 = v0105 + v0113;
        let v0036: f64 = (v0087).sqrt();
        let v0037: f64 = (1i64) as f64 / v0036;
        let v0093: f64 = v0007 * v0037;
        v0046 = v0033 * v0093;
        v0060 = v0007 * v0036;
        v0071 = v0050 * v0093;
        v0080 = v0048 * v0093;
      }
    } else {
      let v0040: bool = (v0001) < (v0003);
      if v0040 {
        v0046 = v0030;
        v0060 = v0051;
        v0071 = v0065;
        v0080 = v0074;
      } else {
        let v0103: f64 = v0002 + v0003;
        let v0114: f64 = -v0103;
        let v0104: f64 = v0001 + (1i64) as f64;
        let v0090: f64 = v0104 + v0114;
        let v0042: f64 = (v0090).sqrt();
        let v0056: f64 = (1i64) as f64 / v0042;
        let v0094: f64 = v0007 * v0056;
        v0046 = v0007 * v0042;
        v0060 = v0033 * v0094;
        v0071 = v0024 * v0094;
        v0080 = v0013 * v0094;
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
  let v0126: f64 = v0002 + v0003;
  let v0128: f64 = v0003 + v0004;
  let v0127: f64 = v0002 + v0004;
  let v0139: f64 = -v0126;
  let v0131: f64 = v0004 + (1i64) as f64;
  let v0141: f64 = -v0128;
  let v0129: f64 = v0002 + (1i64) as f64;
  let v0140: f64 = -v0127;
  let v0130: f64 = v0003 + (1i64) as f64;
  let v0106: f64 = v0131 + v0139;
  let v0112: f64 = v0129 + v0141;
  let v0109: f64 = v0130 + v0140;
  let v0029: f64 = (v0106).sqrt();
  let v0038: f64 = R.get(0, 1);
  let v0042: f64 = (v0112).sqrt();
  let v0023: f64 = R.get(1, 2);
  let v0034: f64 = (v0109).sqrt();
  let v0011: f64 = R.get(2, 0);
  let v0030: f64 = (1i64) as f64 / v0029;
  let v0008: f64 = 0.5f64;
  let v0137: f64 = -v0038;
  let v0039: f64 = R.get(1, 0);
  let v0043: f64 = (1i64) as f64 / v0042;
  let v0138: f64 = -v0023;
  let v0024: f64 = R.get(2, 1);
  let v0055: f64 = (1i64) as f64 / v0034;
  let v0136: f64 = -v0011;
  let v0009: f64 = R.get(0, 2);
  let v0017: f64 = v0126 + v0131;
  let v0132: f64 = v0004 + v0126;
  let v0114: f64 = v0008 * v0030;
  let v0050: f64 = v0039 + v0137;
  let v0115: f64 = v0008 * v0043;
  let v0065: f64 = v0024 + v0138;
  let v0037: bool = (v0002) < (v0004);
  let v0116: f64 = v0008 * v0055;
  let v0014: f64 = v0009 + v0136;
  let v0022: bool = (v0003) < (v0004);
  let v0021: bool = (v0002) < (v0003);
  let v0018: f64 = (v0017).sqrt();
  let v0007: bool = ((0i64) as f64) < (v0132);
  let v0089: f64;
  if v0007 {
    v0089 = v0008 * v0018;
  } else {
    let v0083: f64 = v0050 * v0114;
    if v0021 {
      if v0022 {
        v0089 = v0083;
      } else {
        v0089 = v0014 * v0116;
      }
    } else {
      if v0037 {
        v0089 = v0083;
      } else {
        v0089 = v0065 * v0115;
      }
    }
  }
  let v0092: bool = (v0089) < ((0i64) as f64);
  let v0093: i64;
  if v0092 {
    v0093 = -1i64;
  } else {
    v0093 = 1i64;
  }
  let v0047: f64;
  let v0062: f64;
  let v0074: f64;
  if v0007 {
    let v0019: f64 = (1i64) as f64 / v0018;
    let v0113: f64 = v0008 * v0019;
    v0047 = v0014 * v0113;
    v0062 = v0050 * v0113;
    v0074 = v0065 * v0113;
  } else {
    let v0025: f64 = v0023 + v0024;
    let v0058: f64 = v0009 + v0011;
    let v0040: f64 = v0038 + v0039;
    let v0031: f64 = v0025 * v0114;
    let v0053: f64 = v0008 * v0029;
    let v0067: f64 = v0058 * v0114;
    if v0021 {
      if v0022 {
        v0047 = v0031;
        v0062 = v0053;
        v0074 = v0067;
      } else {
        v0047 = v0008 * v0034;
        v0062 = v0025 * v0116;
        v0074 = v0040 * v0116;
      }
    } else {
      if v0037 {
        v0047 = v0031;
        v0062 = v0053;
        v0074 = v0067;
      } else {
        v0047 = v0040 * v0115;
        v0062 = v0058 * v0115;
        v0074 = v0008 * v0042;
      }
    }
  }
  let v0063: f64 = v0062 * v0062;
  let v0048: f64 = v0047 * v0047;
  let v0134: f64 = v0048 + v0063;
  let v0075: f64 = v0074 * v0074;
  let v0135: f64 = v0075 + v0134;
  let v0077: f64 = (v0135).sqrt();
  let v0078: bool = (1e-16f64) < (v0077);
  let v0099: f64;
  if v0078 {
    let v0090: f64 = (v0089).abs();
    let v0091: f64 = (v0077).atan2(v0090);
    let v0080: f64 = (1i64) as f64 / v0077;
    let v0123: f64 = v0080 * v0091;
    let v0124: f64 = (2i64) as f64 * v0123;
    let v0095: f64 = (v0093) as f64;
    v0099 = v0095 * v0124;
  } else {
    let v0097: i64 = 2i64 * v0093;
    v0099 = (v0097) as f64;
  }
  let v0102: f64 = v0062 * v0099;
  let v0101: f64 = v0047 * v0099;
  let v0100: f64 = v0074 * v0099;
  w.set(0, 0, v0100);
  w.set(1, 0, v0101);
  w.set(2, 0, v0102);
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
    let v002: f64 = (v001).cos();
    let v004: f64 = v002 + (2i64) as f64;
    *out1 = v004;
  }
  if let Some(out2) = out2 {
    let v006: f64 = (v001).abs();
    let v007: f64 = (2i64) as f64 * v006;
    *out2 = v007;
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
  let v005: f64 = v004 * v004;
  let v003: f64 = v002 * v002;
  let v006: f64 = v003 + v005;
  let v007: f64 = (v006).sqrt();
  let v009: bool = ((0i64) as f64) < (v007);
  let v014: f64;
  let v016: f64;
  if v009 {
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
  let v011: f64 = v000 * v004;
  let v003: f64 = v000 * v002;
  if let Some(out) = out {
    *out = crate::types::Point2d::new(v003, v011);
  }
  if let Some(D_inputs) = D_inputs {
    let v012: f64 = -v011;
    D_inputs.set(0, 0, v012);
    D_inputs.set(0, 1, v002);
    D_inputs.set(1, 0, v003);
    D_inputs.set(1, 1, v004);
  }
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
  
  let v010: f64 = p.y();
  let v005: f64 = p.x();
  let v021: f64 = -v010;
  let v002: f64 = c.center.y();
  let v020: f64 = -v005;
  let v001: f64 = c.center.x();
  let v012: f64 = v002 + v021;
  let v008: f64 = v001 + v020;
  let v013: f64 = v012 * v012;
  let v009: f64 = v008 * v008;
  let v014: f64 = v009 + v013;
  let v016: f64 = c.radius;
  let v015: f64 = (v014).sqrt();
  let v017: bool = (v015) <= (v016);
  let v018: f64;
  if v017 {
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
  let v007: f64 = v005 + (-5i64) as f64;
  let v003: f64 = v000 * (2i64) as f64;
  let v008: f64 = crate::external_functions::external_function_1(v003, v007) as f64;
  let v009: f64 = v000 * v008;
  v009
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
  let v011: f64 = v010 * v010;
  let v007: f64 = v.get(0, 0);
  let v004: f64 = v003 * v003;
  let v002: f64 = -2f64 + v001;
  let v012: nalgebra::SMatrix<f64, 2, 3> = nalgebra::SMatrix::<f64, 2, 3>::new(v002, v004, (1i64) as f64, v007, v011, (1i64) as f64);
  let v013: f64 = crate::external_functions::external_function_2(&v012) as f64;
  v013
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
  let v001: f64 = v000 * v000;
  let v005: nalgebra::SMatrix<f64, 2, 1> = nalgebra::SMatrix::<f64, 2, 1>::new(v003, v004);
  let v002: nalgebra::SMatrix<f64, 2, 1> = nalgebra::SMatrix::<f64, 2, 1>::new(v000, v001);
  let v006: nalgebra::SMatrix<f64, 2, 2> = crate::external_functions::external_function_3(&v002, &v005);
  let v010: f64 = v006[(1, 1)];
  let v009: f64 = v006[(1, 0)];
  let v008: f64 = v006[(0, 1)];
  let v007: f64 = v006[(0, 0)];
  nalgebra::SMatrix::<f64, 2, 2>::new(v007, v008, v009, v010)
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
  let v018: f64 = -v002;
  let v000: f64 = a;
  let v008: f64 = v002 * (2i64) as f64;
  let v005: f64 = v000 + v018;
  let v009: crate::types::Point2d = crate::types::Point2d::new(v005, v008);
  let v010: crate::types::Point2d = crate::external_functions::external_function_4(&v009);
  let v013: f64 = v010.x();
  let v011: f64 = v010.y();
  let v014: f64 = (v013).abs();
  let v012: f64 = (v011).abs();
  let v015: bool = (v012) < (v014);
  let v016: f64;
  if v015 {
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
  let v006: crate::types::Circle = crate::types::Circle {
    center: crate::types::Point2d::new(v003, v004),
    radius: 1f64
  };
  let v007: f64 = crate::external_functions::external_function_5(c, &v006) as f64;
  let v009: f64 = v007 * (2i64) as f64;
  let v011: f64 = v009 + (-1i64) as f64;
  v011
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
  let v003: f64 = (v002).abs();
  let v001: f64 = (v000).abs();
  let v004: bool = (v001) < (v003);
  let v005: f64;
  let v012: f64;
  if v004 {
    v005 = v002;
    v012 = v000 * (2i64) as f64;
  } else {
    v005 = v000;
    v012 = v002 * (3i64) as f64;
  }
  let v013: crate::types::Point2d = crate::types::Point2d::new(v005, v012);
  let v014: crate::types::Point2d = crate::external_functions::external_function_4(&v013);
  let v015: crate::types::Point2d = crate::external_functions::external_function_4(&v014);
  let v017: f64 = v015.y();
  let v016: f64 = v015.x();
  crate::types::Point2d::new(v016, v017)
}


