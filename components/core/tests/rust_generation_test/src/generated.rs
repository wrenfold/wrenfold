//! Machine generated code.
#![rustfmt::skip]

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  // multiply: 5
  // negate: 1
  // total: 11
  
  let v001: f64 = theta;
  let v002: f64 = f64::sin(v001);
  let v000: f64 = v.get(1, 0);
  let v006: f64 = f64::cos(v001);
  let v005: f64 = v.get(0, 0);
  let v003: f64 = v000 * v002;
  let v010: f64 = v000 * v006;
  let v009: f64 = v002 * v005;
  let v007: f64 = v005 * v006;
  let v004: f64 = -v003;
  let v011: f64 = v009 + v010;
  let v008: f64 = v004 + v007;
  if let Some(D_theta) = D_theta {
    let v015: f64 = v011 * (-1i64) as f64;
    D_theta.set(0, 0, v015);
    D_theta.set(1, 0, v008);
  }
  v_rot.set(0, 0, v008);
  v_rot.set(1, 0, v011);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  let v008: f64 = f64::sqrt(v007);
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
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
pub fn exclusive_or<>(x: f64, y: f64) -> f64
{
  // Operation counts:
  // branch: 3
  // compare: 2
  // total: 5
  
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
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
pub fn signum_and_abs<>(x: f64, abs: &mut f64) -> f64
{
  // Operation counts:
  // call: 2
  // total: 2
  
  let v00: f64 = x;
  let v03: f64 = f64::abs(v00);
  let v01: f64 = ((0.0f64 < v00) as i64 - (v00 < 0.0f64) as i64) as f64;
  *abs = v03;
  v01
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  let v002: f64 = f64::atan2(v000, v001);
  *D_y = v010;
  *D_x = v013;
  v002
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  let v003: f64 = f64::abs(v002);
  let v001: f64 = f64::abs(v000);
  let v004: bool = (v001) < (v003);
  let v032: f64;
  if v004 {
    let v007: bool = ((0i64) as f64) < (v000);
    let v014: f64;
    if v007 {
      let v008: f64 = v000 * v002;
      v014 = f64::cos(v008);
    } else {
      let v011: f64 = f64::cos(v002);
      v014 = v011 + (2i64) as f64;
    }
    let v015: f64 = f64::abs(v014);
    let v016: f64 = f64::sqrt(v015);
    let v020: f64 = v014 * (3i64) as f64;
    let v017: f64 = -v016;
    v032 = v017 + v020;
  } else {
    let v023: bool = ((0i64) as f64) < (v002);
    let v027: f64;
    if v023 {
      v027 = f64::ln(v001);
    } else {
      let v025: f64 = f64::atan2(v000, v002);
      v027 = (3i64) as f64 * v025;
    }
    let v028: f64 = f64::abs(v027);
    let v030: f64 = f64::powf(v028, 0.3333333333333333f64);
    let v022: f64 = 0.2f64;
    v032 = v022 * v030;
  }
  v032
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  let v003: f64 = f64::abs(v002);
  let v001: f64 = f64::abs(v000);
  let v010: f64 = v000 * v002;
  let v004: bool = (v001) < (v003);
  let v043: f64;
  if v004 {
    let v007: bool = ((0i64) as f64) < (v002);
    if v007 {
      let v008: bool = ((0i64) as f64) < (v000);
      if v008 {
        let v011: f64 = std::f64::consts::PI * v010;
        v043 = f64::cos(v011);
      } else {
        let v019: f64 = (1i64) as f64 / v000;
        let v021: f64 = v019 * (22i64) as f64;
        let v022: f64 = f64::sin(v021);
        let v015: f64 = v002 * (-3i64) as f64;
        v043 = v015 + v022;
      }
    } else {
      let v030: f64 = v002 * 0.1f64;
      let v028: f64 = v000 * 0.4f64;
      let v031: f64 = f64::atan2(v028, v030);
      let v033: f64 = v031 * (19i64) as f64;
      let v025: f64 = -v000;
      v043 = v025 + v033;
    }
  } else {
    let v038: f64 = v002 * (2i64) as f64;
    let v040: f64 = v010 + v038;
    let v041: f64 = f64::abs(v040);
    v043 = f64::sqrt(v041);
  }
  v043
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  let v0024: f64 = (1i64) as f64 + v0021;
  let v0022: f64 = v0006 * v0020;
  let v0013: f64 = 0.5f64;
  let v0011: f64 = f64::sqrt(v0010);
  let v0025: f64 = v0022 + v0024;
  let v0023: f64 = v0008 * v0020;
  let v0014: f64 = v0011 * v0013;
  let v0026: f64 = v0023 + v0025;
  let v0015: f64 = f64::sin(v0014);
  let v0027: f64 = f64::sqrt(v0026);
  let v0018: f64 = v0005 * v0015;
  let v0017: f64 = (1i64) as f64 / v0011;
  let v0035: f64 = v0007 * v0015;
  let v0045: f64 = v0003 * v0015;
  let v0029: f64 = v0005 * v0013;
  let v0028: f64 = (1i64) as f64 / v0027;
  let v0047: f64 = v0003 * v0013;
  let v0019: f64 = v0017 * v0018;
  let v0036: f64 = v0017 * v0035;
  let v0046: f64 = v0017 * v0045;
  let v0053: f64 = f64::cos(v0014);
  let v0012: bool = (1e-16f64) < (v0011);
  let v0031: f64;
  let v0039: f64;
  let v0049: f64;
  let v0054: f64;
  if v0012 {
    v0031 = v0019;
    v0039 = v0036;
    v0049 = v0046;
    v0054 = v0053;
  } else {
    let v0037: f64 = v0007 * v0013;
    v0031 = v0028 * v0029;
    v0039 = v0028 * v0037;
    v0049 = v0028 * v0047;
    v0054 = v0028;
  }
  let v0063: f64 = v0039 * (2i64) as f64;
  let v0060: f64 = v0031 * (2i64) as f64;
  let v0051: f64 = v0049 * (2i64) as f64;
  let v0072: f64 = (-2i64) as f64 * v0049;
  let v0055: f64 = (-2i64) as f64 * v0039;
  let v0075: f64 = v0031 * (-2i64) as f64;
  if let Some(R_D_w) = R_D_w {
    let v0107: f64;
    let v0123: f64;
    let v0139: f64;
    let v0155: f64;
    let v0172: f64;
    let v0189: f64;
    let v0201: f64;
    let v0218: f64;
    let v0233: f64;
    if v0012 {
      let v0086: f64 = v0011 * v0011;
      let v0087: f64 = v0011 * v0086;
      let v0088: f64 = (1i64) as f64 / v0087;
      let v0143: f64 = v0015 * v0088;
      let v0093: f64 = (1i64) as f64 / v0010;
      let v0095: f64 = v0005 * v0047;
      let v0090: f64 = v0005 * v0045;
      let v0116: f64 = v0007 * v0047;
      let v0112: f64 = v0007 * v0045;
      let v0132: f64 = v0007 * v0029;
      let v0128: f64 = v0007 * v0018;
      let v0144: f64 = v0006 * v0143;
      let v0146: f64 = v0013 * v0093;
      let v0162: f64 = v0008 * v0143;
      let v0179: f64 = v0004 * v0143;
      let v0096: f64 = v0093 * v0095;
      let v0091: f64 = v0088 * v0090;
      let v0117: f64 = v0093 * v0116;
      let v0113: f64 = v0088 * v0112;
      let v0133: f64 = v0093 * v0132;
      let v0129: f64 = v0088 * v0128;
      let v0145: f64 = -v0144;
      let v0142: f64 = v0015 * v0017;
      let v0147: f64 = v0053 * v0146;
      let v0163: f64 = -v0162;
      let v0180: f64 = -v0179;
      let v0097: f64 = v0053 * v0096;
      let v0092: f64 = -v0091;
      let v0118: f64 = v0053 * v0117;
      let v0114: f64 = -v0113;
      let v0134: f64 = v0053 * v0133;
      let v0130: f64 = -v0129;
      let v0149: f64 = v0142 + v0145;
      let v0148: f64 = v0006 * v0147;
      let v0167: f64 = v0142 + v0163;
      let v0166: f64 = v0008 * v0147;
      let v0184: f64 = v0142 + v0180;
      let v0183: f64 = v0004 * v0147;
      let v0194: f64 = -0.5f64;
      v0107 = v0092 + v0097;
      v0123 = v0114 + v0118;
      v0139 = v0130 + v0134;
      v0155 = v0148 + v0149;
      v0172 = v0166 + v0167;
      v0189 = v0183 + v0184;
      v0201 = v0046 * v0194;
      v0218 = v0019 * v0194;
      v0233 = v0036 * v0194;
    } else {
      let v0100: f64 = v0027 * v0027;
      let v0101: f64 = v0027 * v0100;
      let v0103: f64 = -0.125f64;
      let v0102: f64 = (1i64) as f64 / v0101;
      let v0104: f64 = v0003 * v0005;
      let v0120: f64 = v0003 * v0007;
      let v0136: f64 = v0005 * v0007;
      let v0152: f64 = v0102 * v0103;
      let v0105: f64 = v0102 * v0104;
      let v0121: f64 = v0102 * v0120;
      let v0137: f64 = v0102 * v0136;
      let v0153: f64 = v0006 * v0152;
      let v0151: f64 = v0013 * v0028;
      let v0170: f64 = v0008 * v0152;
      let v0187: f64 = v0004 * v0152;
      let v0199: f64 = v0003 * v0102;
      let v0198: f64 = -0.25f64;
      let v0216: f64 = v0005 * v0102;
      let v0231: f64 = v0007 * v0102;
      v0107 = v0103 * v0105;
      v0123 = v0103 * v0121;
      v0139 = v0103 * v0137;
      v0155 = v0151 + v0153;
      v0172 = v0151 + v0170;
      v0189 = v0151 + v0187;
      v0201 = v0198 * v0199;
      v0218 = v0198 * v0216;
      v0233 = v0198 * v0231;
    }
    let v0241: f64 = (-2i64) as f64 * v0054;
    let v0319: f64 = v0063 * v0139;
    let v0273: f64 = v0123 * v0241;
    let v0308: f64 = v0060 * v0139;
    let v0242: f64 = v0107 * v0241;
    let v0253: f64 = v0063 * v0107;
    let v0227: f64 = v0060 * v0123;
    let v0266: f64 = v0051 * v0172;
    let v0264: f64 = v0063 * v0123;
    let v0192: f64 = (2i64) as f64 * v0054;
    let v0225: f64 = v0051 * v0139;
    let v0246: f64 = v0063 * v0189;
    let v0244: f64 = v0051 * v0123;
    let v0321: f64 = v0060 * v0172;
    let v0310: f64 = v0063 * v0155;
    let v0262: f64 = v0139 * v0241;
    let v0208: f64 = v0060 * v0107;
    let v0177: f64 = v0051 * v0107;
    let v0210: f64 = v0051 * v0155;
    let v0191: f64 = v0060 * v0189;
    let v0291: f64 = v0049 * (-4i64) as f64;
    let v0109: f64 = v0031 * (-4i64) as f64;
    let v0358: f64 = v0273 + v0319;
    let v0353: f64 = v0242 + v0308;
    let v0345: f64 = v0072 * v0201;
    let v0304: f64 = v0227 + v0253;
    let v0341: f64 = v0264 + v0266;
    let v0212: f64 = v0139 * v0192;
    let v0333: f64 = v0060 * v0218;
    let v0258: f64 = v0225 + v0253;
    let v0329: f64 = v0244 + v0246;
    let v0312: f64 = v0107 * v0192;
    let v0324: f64 = v0319 + v0321;
    let v0193: f64 = v0123 * v0192;
    let v0315: f64 = v0308 + v0310;
    let v0301: f64 = v0051 * v0201;
    let v0124: f64 = v0039 * (-4i64) as f64;
    let v0285: f64 = v0055 * v0233;
    let v0238: f64 = v0225 + v0227;
    let v0281: f64 = v0208 + v0262;
    let v0276: f64 = v0177 + v0273;
    let v0269: f64 = v0262 + v0264;
    let v0255: f64 = v0075 * v0218;
    let v0249: f64 = v0242 + v0244;
    let v0235: f64 = v0063 * v0233;
    let v0221: f64 = v0208 + v0210;
    let v0204: f64 = v0177 + v0191;
    let v0298: f64 = v0123 * v0291;
    let v0160: f64 = v0109 * v0139;
    let v0295: f64 = v0107 * v0291;
    let v0157: f64 = v0109 * v0155;
    let v0292: f64 = v0189 * v0291;
    let v0110: f64 = v0107 * v0109;
    let v0359: f64 = v0321 + v0358;
    let v0357: f64 = v0072 * v0233;
    let v0354: f64 = v0310 + v0353;
    let v0352: f64 = v0072 * v0218;
    let v0349: f64 = v0304 + v0345;
    let v0347: f64 = v0189 * v0241;
    let v0342: f64 = v0212 + v0341;
    let v0340: f64 = v0060 * v0233;
    let v0337: f64 = v0258 + v0333;
    let v0335: f64 = v0155 * v0192;
    let v0330: f64 = v0312 + v0329;
    let v0328: f64 = v0060 * v0201;
    let v0325: f64 = v0193 + v0324;
    let v0323: f64 = v0051 * v0233;
    let v0316: f64 = v0312 + v0315;
    let v0314: f64 = v0051 * v0218;
    let v0305: f64 = v0301 + v0304;
    let v0303: f64 = v0189 * v0192;
    let v0174: f64 = v0124 * v0172;
    let v0141: f64 = v0124 * v0139;
    let v0125: f64 = v0123 * v0124;
    let v0289: f64 = v0238 + v0285;
    let v0287: f64 = v0172 * v0241;
    let v0282: f64 = v0210 + v0281;
    let v0280: f64 = v0055 * v0218;
    let v0277: f64 = v0191 + v0276;
    let v0275: f64 = v0055 * v0201;
    let v0270: f64 = v0266 + v0269;
    let v0268: f64 = v0075 * v0233;
    let v0259: f64 = v0255 + v0258;
    let v0257: f64 = v0155 * v0241;
    let v0250: f64 = v0246 + v0249;
    let v0248: f64 = v0075 * v0201;
    let v0239: f64 = v0235 + v0238;
    let v0237: f64 = v0172 * v0192;
    let v0222: f64 = v0212 + v0221;
    let v0220: f64 = v0063 * v0218;
    let v0205: f64 = v0193 + v0204;
    let v0203: f64 = v0063 * v0201;
    let v0363: f64 = v0160 + v0298;
    let v0362: f64 = v0157 + v0295;
    let v0361: f64 = v0110 + v0292;
    let v0360: f64 = v0357 + v0359;
    let v0355: f64 = v0352 + v0354;
    let v0350: f64 = v0347 + v0349;
    let v0343: f64 = v0340 + v0342;
    let v0338: f64 = v0335 + v0337;
    let v0331: f64 = v0328 + v0330;
    let v0326: f64 = v0323 + v0325;
    let v0317: f64 = v0314 + v0316;
    let v0306: f64 = v0303 + v0305;
    let v0299: f64 = v0174 + v0298;
    let v0296: f64 = v0141 + v0295;
    let v0293: f64 = v0125 + v0292;
    let v0290: f64 = v0287 + v0289;
    let v0283: f64 = v0280 + v0282;
    let v0278: f64 = v0275 + v0277;
    let v0271: f64 = v0268 + v0270;
    let v0260: f64 = v0257 + v0259;
    let v0251: f64 = v0248 + v0250;
    let v0240: f64 = v0237 + v0239;
    let v0223: f64 = v0220 + v0222;
    let v0206: f64 = v0203 + v0205;
    let v0175: f64 = v0160 + v0174;
    let v0158: f64 = v0141 + v0157;
    let v0126: f64 = v0110 + v0125;
    R_D_w.set(0, 0, v0126);
    R_D_w.set(0, 1, v0158);
    R_D_w.set(0, 2, v0175);
    R_D_w.set(1, 0, v0206);
    R_D_w.set(1, 1, v0223);
    R_D_w.set(1, 2, v0240);
    R_D_w.set(2, 0, v0251);
    R_D_w.set(2, 1, v0260);
    R_D_w.set(2, 2, v0271);
    R_D_w.set(3, 0, v0278);
    R_D_w.set(3, 1, v0283);
    R_D_w.set(3, 2, v0290);
    R_D_w.set(4, 0, v0293);
    R_D_w.set(4, 1, v0296);
    R_D_w.set(4, 2, v0299);
    R_D_w.set(5, 0, v0306);
    R_D_w.set(5, 1, v0317);
    R_D_w.set(5, 2, v0326);
    R_D_w.set(6, 0, v0331);
    R_D_w.set(6, 1, v0338);
    R_D_w.set(6, 2, v0343);
    R_D_w.set(7, 0, v0350);
    R_D_w.set(7, 1, v0355);
    R_D_w.set(7, 2, v0360);
    R_D_w.set(8, 0, v0361);
    R_D_w.set(8, 1, v0362);
    R_D_w.set(8, 2, v0363);
  }
  let v0066: f64 = v0049 * v0049;
  let v0032: f64 = v0031 * v0031;
  let v0067: f64 = (-2i64) as f64 * v0066;
  let v0040: f64 = v0039 * v0039;
  let v0034: f64 = v0032 * (-2i64) as f64;
  let v0068: f64 = (1i64) as f64 + v0067;
  let v0079: f64 = v0051 * v0054;
  let v0071: f64 = v0039 * v0060;
  let v0076: f64 = v0054 * v0075;
  let v0059: f64 = v0039 * v0051;
  let v0073: f64 = v0054 * v0072;
  let v0041: f64 = (-2i64) as f64 * v0040;
  let v0064: f64 = v0054 * v0063;
  let v0052: f64 = v0031 * v0051;
  let v0061: f64 = v0054 * v0060;
  let v0056: f64 = v0054 * v0055;
  let v0042: f64 = (1i64) as f64 + v0034;
  let v0082: f64 = v0034 + v0068;
  let v0080: f64 = v0071 + v0079;
  let v0077: f64 = v0059 + v0076;
  let v0074: f64 = v0071 + v0073;
  let v0069: f64 = v0041 + v0068;
  let v0065: f64 = v0052 + v0064;
  let v0062: f64 = v0059 + v0061;
  let v0057: f64 = v0052 + v0056;
  let v0043: f64 = v0041 + v0042;
  R.set(0, 0, v0043);
  R.set(0, 1, v0057);
  R.set(0, 2, v0062);
  R.set(1, 0, v0065);
  R.set(1, 1, v0069);
  R.set(1, 2, v0074);
  R.set(2, 0, v0077);
  R.set(2, 1, v0080);
  R.set(2, 2, v0082);
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
    let v002: f64 = f64::cos(v001);
    let v004: f64 = v002 + (2i64) as f64;
    *out1 = v004;
  }
  if let Some(out2) = out2 {
    let v006: f64 = f64::abs(v001);
    let v007: f64 = (2i64) as f64 * v006;
    *out2 = v007;
  }
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  
  let v003: f64 = p.y();
  let v001: f64 = p.x();
  let v004: f64 = v003 * v003;
  let v002: f64 = v001 * v001;
  let v005: f64 = v002 + v004;
  let v006: f64 = f64::sqrt(v005);
  let v008: bool = ((0i64) as f64) < (v006);
  let v013: f64;
  let v015: f64;
  if v008 {
    let v011: f64 = (1i64) as f64 / v006;
    v013 = v001 * v011;
    v015 = v003 * v011;
  } else {
    v013 = (0i64) as f64;
    v015 = (0i64) as f64;
  }
  crate::types::Point2d::new(v013, v015)
}

#[inline]
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  let v004: f64 = f64::sin(v001);
  let v000: f64 = radius;
  let v002: f64 = f64::cos(v001);
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
#[allow(non_snake_case, clippy::unused_unit, clippy::collapsible_else_if)]
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
  
  let v006: f64 = p.y();
  let v002: f64 = p.x();
  let v007: f64 = -v006;
  let v001: f64 = c.center.y();
  let v003: f64 = -v002;
  let v000: f64 = c.center.x();
  let v008: f64 = v001 + v007;
  let v004: f64 = v000 + v003;
  let v009: f64 = v008 * v008;
  let v005: f64 = v004 * v004;
  let v010: f64 = v005 + v009;
  let v012: f64 = c.radius;
  let v011: f64 = f64::sqrt(v010);
  let v013: bool = (v011) <= (v012);
  let v014: f64;
  if v013 {
    v014 = v012;
  } else {
    v014 = v011;
  }
  crate::types::Circle {
    center: crate::types::Point2d::new(v000, v001),
    radius: v014
  }
}

