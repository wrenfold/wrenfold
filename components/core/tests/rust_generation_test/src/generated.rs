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
  // add: 3
  // branch: 1
  // call: 2
  // multiply: 4
  // negate: 3
  // total: 13
  
  let v001: f64 = theta;
  let v002: f64 = (v001).sin();
  let v000: f64 = v.get(1, 0);
  let v006: f64 = (v001).cos();
  let v005: f64 = v.get(0, 0);
  let v003: f64 = v000 * v002;
  let v007: f64 = v005 * v006;
  let v004: f64 = -v003;
  let v010: f64 = v000 * v006;
  let v009: f64 = v002 * v005;
  let v008: f64 = v004 + v007;
  if let Some(D_theta) = D_theta {
    let v014: f64 = -v010;
    let v013: f64 = -v009;
    let v015: f64 = v013 + v014;
    D_theta.set(0, 0, v015);
    D_theta.set(1, 0, v008);
  }
  let v011: f64 = v009 + v010;
  v_rot.set(0, 0, v008);
  v_rot.set(1, 0, v011);
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
  let v006: f64 = v001 + v003;
  let v005: f64 = v004 * v004;
  let v007: f64 = v005 + v006;
  let v008: f64 = (v007).sqrt();
  let v012: f64 = (1i64) as f64 / v008;
  let v015: f64 = v004 * v012;
  let v014: f64 = v002 * v012;
  let v013: f64 = v000 * v012;
  D_v.set(0, 0, v013);
  D_v.set(0, 1, v014);
  D_v.set(0, 2, v015);
  v008
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
  // branch: 3
  // compare: 2
  // total: 5
  
  let v004: f64 = y;
  let v001: f64 = x;
  let v006: bool = ((0i64) as f64) < (v004);
  let v003: bool = ((0i64) as f64) < (v001);
  let v010: i64;
  if v003 {
    if v006 {
      v010 = 0i64;
    } else {
      v010 = 1i64;
    }
  } else {
    if v006 {
      v010 = 1i64;
    } else {
      v010 = 0i64;
    }
  }
  let v011: f64 = (v010) as f64;
  v011
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
  let v012: f64 = v000 * v009;
  let v010: f64 = v001 * v009;
  let v013: f64 = -v012;
  let v002: f64 = (v000).atan2(v001);
  *D_y = v010;
  *D_x = v013;
  v002
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn nested_conditionals_1<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // add: 2
  // branch: 3
  // call: 10
  // compare: 3
  // multiply: 4
  // negate: 1
  // total: 23
  
  let v002: f64 = x;
  let v000: f64 = y;
  let v003: f64 = (v002).abs();
  let v001: f64 = (v000).abs();
  let v004: bool = (v001) < (v003);
  let v034: f64;
  if v004 {
    let v007: bool = ((0i64) as f64) < (v000);
    let v014: f64;
    if v007 {
      let v008: f64 = v000 * v002;
      v014 = (v008).cos();
    } else {
      let v011: f64 = (v002).cos();
      v014 = v011 + (2i64) as f64;
    }
    let v015: f64 = (v014).abs();
    let v016: f64 = (v015).sqrt();
    let v020: f64 = v014 * (3i64) as f64;
    let v017: f64 = -v016;
    v034 = v017 + v020;
  } else {
    let v024: bool = ((0i64) as f64) < (v002);
    let v029: f64;
    if v024 {
      v029 = (v001).ln();
    } else {
      let v026: f64 = (v000).atan2(v002);
      v029 = (3i64) as f64 * v026;
    }
    let v030: f64 = (v029).abs();
    let v032: f64 = (v030).powf(0.3333333333333333f64);
    let v022: f64 = 0.2f64;
    v034 = v022 * v032;
  }
  v034
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn nested_conditionals_2<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // add: 3
  // branch: 3
  // call: 7
  // compare: 3
  // divide: 1
  // multiply: 8
  // negate: 1
  // total: 26
  
  let v002: f64 = x;
  let v000: f64 = y;
  let v003: f64 = (v002).abs();
  let v001: f64 = (v000).abs();
  let v011: f64 = v000 * v002;
  let v004: bool = (v001) < (v003);
  let v044: f64;
  if v004 {
    let v007: bool = ((0i64) as f64) < (v002);
    if v007 {
      let v009: bool = ((0i64) as f64) < (v000);
      if v009 {
        let v012: f64 = std::f64::consts::PI * v011;
        v044 = (v012).cos();
      } else {
        let v020: f64 = (1i64) as f64 / v000;
        let v022: f64 = v020 * (22i64) as f64;
        let v023: f64 = (v022).sin();
        let v016: f64 = v002 * (-3i64) as f64;
        v044 = v016 + v023;
      }
    } else {
      let v031: f64 = v002 * 0.1f64;
      let v029: f64 = v000 * 0.4f64;
      let v032: f64 = (v029).atan2(v031);
      let v034: f64 = v032 * (19i64) as f64;
      let v026: f64 = -v000;
      v044 = v026 + v034;
    }
  } else {
    let v039: f64 = v002 * (2i64) as f64;
    let v041: f64 = v011 + v039;
    let v042: f64 = (v041).abs();
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
  // add: 88
  // branch: 3
  // call: 4
  // compare: 1
  // divide: 5
  // multiply: 151
  // negate: 6
  // total: 258
  
  let v0005: f64 = w.get(1, 0);
  let v0003: f64 = w.get(0, 0);
  let v0006: f64 = v0005 * v0005;
  let v0004: f64 = v0003 * v0003;
  let v0007: f64 = w.get(2, 0);
  let v0020: f64 = 0.25f64;
  let v0009: f64 = v0004 + v0006;
  let v0008: f64 = v0007 * v0007;
  let v0021: f64 = v0004 * v0020;
  let v0010: f64 = v0008 + v0009;
  let v0025: f64 = (1i64) as f64 + v0021;
  let v0022: f64 = v0006 * v0020;
  let v0013: f64 = 0.5f64;
  let v0011: f64 = (v0010).sqrt();
  let v0026: f64 = v0022 + v0025;
  let v0023: f64 = v0008 * v0020;
  let v0014: f64 = v0011 * v0013;
  let v0027: f64 = v0023 + v0026;
  let v0015: f64 = (v0014).sin();
  let v0028: f64 = (v0027).sqrt();
  let v0018: f64 = v0005 * v0015;
  let v0017: f64 = (1i64) as f64 / v0011;
  let v0037: f64 = v0007 * v0015;
  let v0049: f64 = v0003 * v0015;
  let v0031: f64 = v0005 * v0013;
  let v0030: f64 = (1i64) as f64 / v0028;
  let v0051: f64 = v0003 * v0013;
  let v0019: f64 = v0017 * v0018;
  let v0038: f64 = v0017 * v0037;
  let v0050: f64 = v0017 * v0049;
  let v0057: f64 = (v0014).cos();
  let v0012: bool = (1e-16f64) < (v0011);
  let v0033: f64;
  let v0041: f64;
  let v0053: f64;
  let v0058: f64;
  if v0012 {
    v0033 = v0019;
    v0041 = v0038;
    v0053 = v0050;
    v0058 = v0057;
  } else {
    let v0039: f64 = v0007 * v0013;
    v0033 = v0030 * v0031;
    v0041 = v0030 * v0039;
    v0053 = v0030 * v0051;
    v0058 = v0030;
  }
  let v0067: f64 = v0033 * (2i64) as f64;
  let v0071: f64 = v0041 * (2i64) as f64;
  let v0055: f64 = v0053 * (2i64) as f64;
  let v0084: f64 = (-2i64) as f64 * v0053;
  let v0060: f64 = (-2i64) as f64 * v0041;
  let v0088: f64 = v0033 * (-2i64) as f64;
  if let Some(R_D_w) = R_D_w {
    let v0125: f64;
    let v0141: f64;
    let v0159: f64;
    let v0175: f64;
    let v0194: f64;
    let v0213: f64;
    let v0227: f64;
    let v0248: f64;
    let v0266: f64;
    if v0012 {
      let v0101: f64 = v0011 * v0011;
      let v0102: f64 = v0011 * v0101;
      let v0104: f64 = (1i64) as f64 / v0102;
      let v0147: f64 = v0015 * v0104;
      let v0110: f64 = (1i64) as f64 / v0010;
      let v0112: f64 = v0005 * v0051;
      let v0106: f64 = v0005 * v0049;
      let v0134: f64 = v0007 * v0051;
      let v0130: f64 = v0007 * v0049;
      let v0148: f64 = v0006 * v0147;
      let v0150: f64 = v0013 * v0110;
      let v0168: f64 = v0007 * v0031;
      let v0164: f64 = v0007 * v0018;
      let v0184: f64 = v0008 * v0147;
      let v0203: f64 = v0004 * v0147;
      let v0113: f64 = v0110 * v0112;
      let v0107: f64 = v0104 * v0106;
      let v0135: f64 = v0110 * v0134;
      let v0131: f64 = v0104 * v0130;
      let v0149: f64 = -v0148;
      let v0146: f64 = v0015 * v0017;
      let v0151: f64 = v0057 * v0150;
      let v0169: f64 = v0110 * v0168;
      let v0165: f64 = v0104 * v0164;
      let v0185: f64 = -v0184;
      let v0204: f64 = -v0203;
      let v0114: f64 = v0057 * v0113;
      let v0108: f64 = -v0107;
      let v0136: f64 = v0057 * v0135;
      let v0132: f64 = -v0131;
      let v0153: f64 = v0146 + v0149;
      let v0152: f64 = v0006 * v0151;
      let v0170: f64 = v0057 * v0169;
      let v0166: f64 = -v0165;
      let v0189: f64 = v0146 + v0185;
      let v0188: f64 = v0008 * v0151;
      let v0208: f64 = v0146 + v0204;
      let v0207: f64 = v0004 * v0151;
      let v0220: f64 = -0.5f64;
      v0125 = v0108 + v0114;
      v0141 = v0132 + v0136;
      v0159 = v0152 + v0153;
      v0175 = v0166 + v0170;
      v0194 = v0188 + v0189;
      v0213 = v0207 + v0208;
      v0227 = v0050 * v0220;
      v0248 = v0019 * v0220;
      v0266 = v0038 * v0220;
    } else {
      let v0117: f64 = v0028 * v0028;
      let v0118: f64 = v0028 * v0117;
      let v0121: f64 = -0.125f64;
      let v0120: f64 = (1i64) as f64 / v0118;
      let v0122: f64 = v0003 * v0005;
      let v0138: f64 = v0003 * v0007;
      let v0156: f64 = v0120 * v0121;
      let v0172: f64 = v0005 * v0007;
      let v0123: f64 = v0120 * v0122;
      let v0139: f64 = v0120 * v0138;
      let v0157: f64 = v0006 * v0156;
      let v0155: f64 = v0013 * v0030;
      let v0173: f64 = v0120 * v0172;
      let v0192: f64 = v0008 * v0156;
      let v0211: f64 = v0004 * v0156;
      let v0225: f64 = v0003 * v0120;
      let v0224: f64 = -0.25f64;
      let v0246: f64 = v0005 * v0120;
      let v0264: f64 = v0007 * v0120;
      v0125 = v0121 * v0123;
      v0141 = v0121 * v0139;
      v0159 = v0155 + v0157;
      v0175 = v0121 * v0173;
      v0194 = v0155 + v0192;
      v0213 = v0155 + v0211;
      v0227 = v0224 * v0225;
      v0248 = v0224 * v0246;
      v0266 = v0224 * v0264;
    }
    let v0277: f64 = (-2i64) as f64 * v0058;
    let v0380: f64 = v0067 * v0194;
    let v0320: f64 = v0141 * v0277;
    let v0365: f64 = v0067 * v0175;
    let v0278: f64 = v0125 * v0277;
    let v0293: f64 = v0071 * v0125;
    let v0260: f64 = v0067 * v0141;
    let v0311: f64 = v0071 * v0141;
    let v0308: f64 = v0055 * v0194;
    let v0218: f64 = (2i64) as f64 * v0058;
    let v0257: f64 = v0055 * v0175;
    let v0284: f64 = v0071 * v0213;
    let v0281: f64 = v0055 * v0141;
    let v0383: f64 = v0071 * v0175;
    let v0368: f64 = v0071 * v0159;
    let v0305: f64 = v0175 * v0277;
    let v0236: f64 = v0055 * v0159;
    let v0201: f64 = v0055 * v0125;
    let v0239: f64 = v0067 * v0125;
    let v0216: f64 = v0067 * v0213;
    let v0343: f64 = v0053 * (-4i64) as f64;
    let v0127: f64 = v0033 * (-4i64) as f64;
    let v0429: f64 = v0320 + v0380;
    let v0423: f64 = v0278 + v0365;
    let v0413: f64 = v0084 * v0227;
    let v0360: f64 = v0260 + v0293;
    let v0408: f64 = v0308 + v0311;
    let v0242: f64 = v0175 * v0218;
    let v0398: f64 = v0067 * v0248;
    let v0300: f64 = v0257 + v0293;
    let v0393: f64 = v0281 + v0284;
    let v0371: f64 = v0125 * v0218;
    let v0387: f64 = v0380 + v0383;
    let v0219: f64 = v0141 * v0218;
    let v0375: f64 = v0365 + v0368;
    let v0356: f64 = v0055 * v0227;
    let v0143: f64 = v0041 * (-4i64) as f64;
    let v0335: f64 = v0060 * v0266;
    let v0273: f64 = v0257 + v0260;
    let v0330: f64 = v0236 + v0305;
    let v0324: f64 = v0201 + v0320;
    let v0315: f64 = v0305 + v0308;
    let v0296: f64 = v0088 * v0248;
    let v0288: f64 = v0278 + v0281;
    let v0269: f64 = v0071 * v0266;
    let v0252: f64 = v0236 + v0239;
    let v0231: f64 = v0201 + v0216;
    let v0352: f64 = v0141 * v0343;
    let v0182: f64 = v0127 * v0175;
    let v0348: f64 = v0125 * v0343;
    let v0162: f64 = v0127 * v0159;
    let v0344: f64 = v0213 * v0343;
    let v0128: f64 = v0125 * v0127;
    let v0430: f64 = v0383 + v0429;
    let v0428: f64 = v0084 * v0266;
    let v0424: f64 = v0368 + v0423;
    let v0422: f64 = v0084 * v0248;
    let v0418: f64 = v0360 + v0413;
    let v0416: f64 = v0213 * v0277;
    let v0409: f64 = v0242 + v0408;
    let v0407: f64 = v0067 * v0266;
    let v0403: f64 = v0300 + v0398;
    let v0401: f64 = v0159 * v0218;
    let v0394: f64 = v0371 + v0393;
    let v0392: f64 = v0067 * v0227;
    let v0388: f64 = v0219 + v0387;
    let v0386: f64 = v0055 * v0266;
    let v0376: f64 = v0371 + v0375;
    let v0374: f64 = v0055 * v0248;
    let v0361: f64 = v0356 + v0360;
    let v0359: f64 = v0213 * v0218;
    let v0197: f64 = v0143 * v0194;
    let v0178: f64 = v0143 * v0175;
    let v0144: f64 = v0141 * v0143;
    let v0340: f64 = v0273 + v0335;
    let v0338: f64 = v0194 * v0277;
    let v0331: f64 = v0239 + v0330;
    let v0329: f64 = v0060 * v0248;
    let v0325: f64 = v0216 + v0324;
    let v0323: f64 = v0060 * v0227;
    let v0316: f64 = v0311 + v0315;
    let v0314: f64 = v0088 * v0266;
    let v0301: f64 = v0296 + v0300;
    let v0299: f64 = v0159 * v0277;
    let v0289: f64 = v0284 + v0288;
    let v0287: f64 = v0088 * v0227;
    let v0274: f64 = v0269 + v0273;
    let v0272: f64 = v0194 * v0218;
    let v0253: f64 = v0242 + v0252;
    let v0251: f64 = v0071 * v0248;
    let v0232: f64 = v0219 + v0231;
    let v0230: f64 = v0071 * v0227;
    let v0434: f64 = v0182 + v0352;
    let v0433: f64 = v0162 + v0348;
    let v0432: f64 = v0128 + v0344;
    let v0431: f64 = v0428 + v0430;
    let v0425: f64 = v0422 + v0424;
    let v0419: f64 = v0416 + v0418;
    let v0410: f64 = v0407 + v0409;
    let v0404: f64 = v0401 + v0403;
    let v0395: f64 = v0392 + v0394;
    let v0389: f64 = v0386 + v0388;
    let v0377: f64 = v0374 + v0376;
    let v0362: f64 = v0359 + v0361;
    let v0353: f64 = v0197 + v0352;
    let v0349: f64 = v0178 + v0348;
    let v0345: f64 = v0144 + v0344;
    let v0341: f64 = v0338 + v0340;
    let v0332: f64 = v0329 + v0331;
    let v0326: f64 = v0323 + v0325;
    let v0317: f64 = v0314 + v0316;
    let v0302: f64 = v0299 + v0301;
    let v0290: f64 = v0287 + v0289;
    let v0275: f64 = v0272 + v0274;
    let v0254: f64 = v0251 + v0253;
    let v0233: f64 = v0230 + v0232;
    let v0198: f64 = v0182 + v0197;
    let v0179: f64 = v0162 + v0178;
    let v0145: f64 = v0128 + v0144;
    R_D_w.set(0, 0, v0145);
    R_D_w.set(0, 1, v0179);
    R_D_w.set(0, 2, v0198);
    R_D_w.set(1, 0, v0233);
    R_D_w.set(1, 1, v0254);
    R_D_w.set(1, 2, v0275);
    R_D_w.set(2, 0, v0290);
    R_D_w.set(2, 1, v0302);
    R_D_w.set(2, 2, v0317);
    R_D_w.set(3, 0, v0326);
    R_D_w.set(3, 1, v0332);
    R_D_w.set(3, 2, v0341);
    R_D_w.set(4, 0, v0345);
    R_D_w.set(4, 1, v0349);
    R_D_w.set(4, 2, v0353);
    R_D_w.set(5, 0, v0362);
    R_D_w.set(5, 1, v0377);
    R_D_w.set(5, 2, v0389);
    R_D_w.set(6, 0, v0395);
    R_D_w.set(6, 1, v0404);
    R_D_w.set(6, 2, v0410);
    R_D_w.set(7, 0, v0419);
    R_D_w.set(7, 1, v0425);
    R_D_w.set(7, 2, v0431);
    R_D_w.set(8, 0, v0432);
    R_D_w.set(8, 1, v0433);
    R_D_w.set(8, 2, v0434);
  }
  let v0074: f64 = v0053 * v0053;
  let v0034: f64 = v0033 * v0033;
  let v0076: f64 = (-2i64) as f64 * v0074;
  let v0042: f64 = v0041 * v0041;
  let v0036: f64 = v0034 * (-2i64) as f64;
  let v0078: f64 = (1i64) as f64 + v0076;
  let v0093: f64 = v0055 * v0058;
  let v0082: f64 = v0041 * v0067;
  let v0089: f64 = v0058 * v0088;
  let v0065: f64 = v0041 * v0055;
  let v0085: f64 = v0058 * v0084;
  let v0044: f64 = (-2i64) as f64 * v0042;
  let v0072: f64 = v0058 * v0071;
  let v0056: f64 = v0033 * v0055;
  let v0068: f64 = v0058 * v0067;
  let v0061: f64 = v0058 * v0060;
  let v0046: f64 = (1i64) as f64 + v0036;
  let v0097: f64 = v0036 + v0078;
  let v0094: f64 = v0082 + v0093;
  let v0090: f64 = v0065 + v0089;
  let v0086: f64 = v0082 + v0085;
  let v0079: f64 = v0044 + v0078;
  let v0073: f64 = v0056 + v0072;
  let v0069: f64 = v0065 + v0068;
  let v0062: f64 = v0056 + v0061;
  let v0047: f64 = v0044 + v0046;
  R.set(0, 0, v0047);
  R.set(0, 1, v0062);
  R.set(0, 2, v0069);
  R.set(1, 0, v0073);
  R.set(1, 1, v0079);
  R.set(1, 2, v0086);
  R.set(2, 0, v0090);
  R.set(2, 1, v0094);
  R.set(2, 2, v0097);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn quaternion_from_matrix<T0, T1, >(R: &T0, q_xyzw: &mut T1) -> ()
where
  T0: wrenfold_traits::Span2D<3, 3, ValueType = f64>,
  T1: wrenfold_traits::OutputSpan2D<4, 1, ValueType = f64>,
{
  // Operation counts:
  // add: 19
  // branch: 4
  // call: 4
  // compare: 4
  // divide: 4
  // multiply: 20
  // negate: 6
  // total: 61
  
  let v0002: f64 = R.get(1, 1);
  let v0001: f64 = R.get(0, 0);
  let v0048: f64 = R.get(0, 1);
  let v0018: f64 = R.get(1, 2);
  let v0035: f64 = R.get(2, 0);
  let v0004: f64 = v0001 + v0002;
  let v0003: f64 = R.get(2, 2);
  let v0079: f64 = -v0048;
  let v0047: f64 = R.get(1, 0);
  let v0019: f64 = -v0018;
  let v0017: f64 = R.get(2, 1);
  let v0063: f64 = -v0035;
  let v0034: f64 = R.get(0, 2);
  let v0005: f64 = v0003 + v0004;
  let v0008: f64 = 0.5f64;
  let v0080: f64 = v0047 + v0079;
  let v0011: f64 = v0001 + (1i64) as f64;
  let v0020: f64 = v0017 + v0019;
  let v0064: f64 = v0034 + v0063;
  let v0007: bool = ((0i64) as f64) < (v0005);
  let v0062: f64;
  let v0078: f64;
  let v0091: f64;
  let v0102: f64;
  if v0007 {
    let v0012: f64 = v0002 + v0011;
    let v0013: f64 = v0003 + v0012;
    let v0014: f64 = (v0013).sqrt();
    let v0016: f64 = (1i64) as f64 / v0014;
    let v0021: f64 = v0008 * v0016;
    v0062 = v0020 * v0021;
    v0078 = v0021 * v0064;
    v0091 = v0021 * v0080;
    v0102 = v0008 * v0014;
  } else {
    let v0028: f64 = v0003 + (1i64) as f64;
    let v0025: f64 = -v0001;
    let v0029: f64 = v0025 + v0028;
    let v0026: f64 = -v0002;
    let v0030: f64 = v0026 + v0029;
    let v0031: f64 = (v0030).sqrt();
    let v0033: f64 = (1i64) as f64 / v0031;
    let v0037: f64 = v0008 * v0033;
    let v0036: f64 = v0034 + v0035;
    let v0067: f64 = v0017 + v0018;
    let v0039: f64 = -v0003;
    let v0049: f64 = v0047 + v0048;
    let v0038: f64 = v0036 * v0037;
    let v0069: f64 = v0037 * v0067;
    let v0083: f64 = v0008 * v0031;
    let v0094: f64 = v0037 * v0080;
    let v0023: bool = (v0001) < (v0002);
    if v0023 {
      let v0024: bool = (v0002) < (v0003);
      if v0024 {
        v0062 = v0038;
        v0078 = v0069;
        v0091 = v0083;
        v0102 = v0094;
      } else {
        let v0041: f64 = v0002 + (1i64) as f64;
        let v0042: f64 = v0025 + v0041;
        let v0043: f64 = v0039 + v0042;
        let v0044: f64 = (v0043).sqrt();
        let v0046: f64 = (1i64) as f64 / v0044;
        let v0050: f64 = v0008 * v0046;
        v0062 = v0049 * v0050;
        v0078 = v0008 * v0044;
        v0091 = v0050 * v0067;
        v0102 = v0050 * v0064;
      }
    } else {
      let v0053: bool = (v0001) < (v0003);
      if v0053 {
        v0062 = v0038;
        v0078 = v0069;
        v0091 = v0083;
        v0102 = v0094;
      } else {
        let v0056: f64 = v0011 + v0026;
        let v0057: f64 = v0039 + v0056;
        let v0058: f64 = (v0057).sqrt();
        let v0073: f64 = (1i64) as f64 / v0058;
        let v0074: f64 = v0008 * v0073;
        v0062 = v0008 * v0058;
        v0078 = v0049 * v0074;
        v0091 = v0036 * v0074;
        v0102 = v0020 * v0074;
      }
    }
  }
  q_xyzw.set(0, 0, v0062);
  q_xyzw.set(1, 0, v0078);
  q_xyzw.set(2, 0, v0091);
  q_xyzw.set(3, 0, v0102);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if, clippy::needless_late_init, unused_variables)]
pub fn rotation_vector_from_matrix<T0, T1, >(R: &T0, w: &mut T1) -> ()
where
  T0: wrenfold_traits::Span2D<3, 3, ValueType = f64>,
  T1: wrenfold_traits::OutputSpan2D<3, 1, ValueType = f64>,
{
  // Operation counts:
  // add: 21
  // branch: 9
  // call: 6
  // compare: 5
  // divide: 5
  // multiply: 31
  // negate: 6
  // total: 83
  
  let v0004: f64 = R.get(2, 2);
  let v0002: f64 = R.get(0, 0);
  let v0003: f64 = R.get(1, 1);
  let v0029: f64 = v0004 + (1i64) as f64;
  let v0026: f64 = -v0002;
  let v0027: f64 = -v0003;
  let v0012: f64 = v0002 + (1i64) as f64;
  let v0042: f64 = v0003 + (1i64) as f64;
  let v0030: f64 = v0026 + v0029;
  let v0051: f64 = v0012 + v0027;
  let v0040: f64 = -v0004;
  let v0043: f64 = v0026 + v0042;
  let v0031: f64 = v0027 + v0030;
  let v0052: f64 = v0040 + v0051;
  let v0044: f64 = v0040 + v0043;
  let v0032: f64 = (v0031).sqrt();
  let v0053: f64 = (v0052).sqrt();
  let v0045: f64 = (v0044).sqrt();
  let v0013: f64 = v0003 + v0012;
  let v0019: f64 = R.get(2, 0);
  let v0057: f64 = R.get(0, 1);
  let v0036: f64 = R.get(1, 2);
  let v0005: f64 = v0002 + v0003;
  let v0034: f64 = (1i64) as f64 / v0032;
  let v0009: f64 = 0.5f64;
  let v0055: f64 = (1i64) as f64 / v0053;
  let v0071: f64 = (1i64) as f64 / v0045;
  let v0014: f64 = v0004 + v0013;
  let v0020: f64 = -v0019;
  let v0018: f64 = R.get(0, 2);
  let v0065: f64 = -v0057;
  let v0056: f64 = R.get(1, 0);
  let v0082: f64 = -v0036;
  let v0035: f64 = R.get(2, 1);
  let v0006: f64 = v0004 + v0005;
  let v0038: f64 = v0009 * v0034;
  let v0059: f64 = v0009 * v0055;
  let v0048: bool = (v0002) < (v0004);
  let v0072: f64 = v0009 * v0071;
  let v0025: bool = (v0003) < (v0004);
  let v0024: bool = (v0002) < (v0003);
  let v0015: f64 = (v0014).sqrt();
  let v0021: f64 = v0018 + v0020;
  let v0066: f64 = v0056 + v0065;
  let v0083: f64 = v0035 + v0082;
  let v0008: bool = ((0i64) as f64) < (v0006);
  let v0063: f64;
  let v0080: f64;
  let v0094: f64;
  if v0008 {
    let v0017: f64 = (1i64) as f64 / v0015;
    let v0022: f64 = v0009 * v0017;
    v0063 = v0021 * v0022;
    v0080 = v0022 * v0066;
    v0094 = v0022 * v0083;
  } else {
    let v0037: f64 = v0035 + v0036;
    let v0075: f64 = v0018 + v0019;
    let v0058: f64 = v0056 + v0057;
    let v0039: f64 = v0037 * v0038;
    let v0069: f64 = v0009 * v0032;
    let v0087: f64 = v0038 * v0075;
    if v0024 {
      if v0025 {
        v0063 = v0039;
        v0080 = v0069;
        v0094 = v0087;
      } else {
        v0063 = v0009 * v0045;
        v0080 = v0037 * v0072;
        v0094 = v0058 * v0072;
      }
    } else {
      if v0048 {
        v0063 = v0039;
        v0080 = v0069;
        v0094 = v0087;
      } else {
        v0063 = v0058 * v0059;
        v0080 = v0059 * v0075;
        v0094 = v0009 * v0053;
      }
    }
  }
  let v0081: f64 = v0080 * v0080;
  let v0064: f64 = v0063 * v0063;
  let v0096: f64 = v0064 + v0081;
  let v0095: f64 = v0094 * v0094;
  let v0097: f64 = v0095 + v0096;
  let v0098: f64 = (v0097).sqrt();
  let v0099: bool = (1e-16f64) < (v0098);
  let v0121: f64;
  let v0128: f64;
  let v0135: f64;
  if v0099 {
    let v0113: f64;
    if v0008 {
      v0113 = v0009 * v0015;
    } else {
      let v0105: f64 = v0038 * v0066;
      if v0024 {
        if v0025 {
          v0113 = v0105;
        } else {
          v0113 = v0021 * v0072;
        }
      } else {
        if v0048 {
          v0113 = v0105;
        } else {
          v0113 = v0059 * v0083;
        }
      }
    }
    let v0102: f64 = (1i64) as f64 / v0098;
    let v0116: f64 = v0102 * (2i64) as f64;
    let v0114: f64 = (v0098).atan2(v0113);
    let v0117: f64 = v0114 * v0116;
    v0121 = v0094 * v0117;
    v0128 = v0063 * v0117;
    v0135 = v0080 * v0117;
  } else {
    v0121 = v0094 * (2i64) as f64;
    v0128 = v0063 * (2i64) as f64;
    v0135 = v0080 * (2i64) as f64;
  }
  w.set(0, 0, v0121);
  w.set(1, 0, v0128);
  w.set(2, 0, v0135);
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
    let v008: f64 = (2i64) as f64 * v006;
    *out2 = v008;
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
  let v015: f64;
  let v018: f64;
  if v009 {
    let v012: f64 = (1i64) as f64 / v007;
    v015 = v002 * v012;
    v018 = v004 * v012;
  } else {
    v015 = (0i64) as f64;
    v018 = (0i64) as f64;
  }
  crate::types::Point2d::new(v015, v018)
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
  let v005: f64 = v000 * v004;
  let v003: f64 = v000 * v002;
  if let Some(out) = out {
    *out = crate::types::Point2d::new(v003, v005);
  }
  if let Some(D_inputs) = D_inputs {
    let v007: f64 = -v005;
    D_inputs.set(0, 0, v007);
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
  
  let v008: f64 = p.y();
  let v004: f64 = p.x();
  let v009: f64 = -v008;
  let v002: f64 = c.center.y();
  let v005: f64 = -v004;
  let v001: f64 = c.center.x();
  let v010: f64 = v002 + v009;
  let v006: f64 = v001 + v005;
  let v011: f64 = v010 * v010;
  let v007: f64 = v006 * v006;
  let v012: f64 = v007 + v011;
  let v014: f64 = c.radius;
  let v013: f64 = (v012).sqrt();
  let v015: bool = (v013) <= (v014);
  let v016: f64;
  if v015 {
    v016 = v014;
  } else {
    v016 = v013;
  }
  crate::types::Circle {
    center: crate::types::Point2d::new(v001, v002),
    radius: v016
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
  let v013: nalgebra::SMatrix<f64, 2, 3> = nalgebra::SMatrix::<f64, 2, 3>::new(v002, v004, (1i64) as f64, v007, v011, (1i64) as f64);
  let v014: f64 = crate::external_functions::external_function_2(&v013) as f64;
  v014
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
  
  let v001: f64 = b;
  let v002: f64 = -v001;
  let v000: f64 = a;
  let v006: f64 = v001 * (2i64) as f64;
  let v003: f64 = v000 + v002;
  let v007: crate::types::Point2d = crate::types::Point2d::new(v003, v006);
  let v008: crate::types::Point2d = crate::external_functions::external_function_4(&v007);
  let v011: f64 = v008.x();
  let v009: f64 = v008.y();
  let v012: f64 = (v011).abs();
  let v010: f64 = (v009).abs();
  let v013: bool = (v010) < (v012);
  let v014: f64;
  if v013 {
    v014 = v011;
  } else {
    v014 = v009;
  }
  v014
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
  // multiply: 2
  // total: 8
  
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

