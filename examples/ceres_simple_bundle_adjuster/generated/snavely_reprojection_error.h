// Machine generated code.
#pragma once
#include <cmath>
#include <cstdint>

#include <wrenfold/span.h>


namespace gen {

template <typename Scalar, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5>
void snavely_reprojection_error(const T0& camera, const T1& point, const T2& measured_xy, T3&& residuals, T4&& residuals_D_camera, T5&& residuals_D_point)
{
  auto _camera = wf::make_input_span<9, 1>(camera);
  auto _point = wf::make_input_span<3, 1>(point);
  auto _measured_xy = wf::make_input_span<2, 1>(measured_xy);
  auto _residuals = wf::make_output_span<2, 1>(residuals);
  auto _residuals_D_camera = wf::make_optional_output_span<2, 9>(residuals_D_camera);
  auto _residuals_D_point = wf::make_optional_output_span<2, 3>(residuals_D_point);

  // Operation counts:
  // add: 141
  // branch: 4
  // call: 4
  // compare: 1
  // divide: 7
  // multiply: 288
  // negate: 20
  // total: 465

  const Scalar v00016 = _camera(2, 0);
  const Scalar v00014 = _camera(1, 0);
  const Scalar v00012 = _camera(0, 0);
  const Scalar v01282 = v00016 * v00016;
  const Scalar v01279 = v00014 * v00014;
  const Scalar v01260 = v00012 * v00012;
  const Scalar v00018 = v01260 + v01279 + v01282;
  const Scalar v00023 = static_cast<Scalar>(0.5);
  const Scalar v00019 = std::sqrt(v00018);
  const Scalar v00033 = std::sqrt(static_cast<Scalar>(1) + v00018 * static_cast<Scalar>(0.25));
  const Scalar v00025 = v00019 * v00023;
  const Scalar v00034 = static_cast<Scalar>(1) / v00033;
  const Scalar v00026 = std::sin(v00025);
  const Scalar v00022 = static_cast<Scalar>(1) / v00019;
  const Scalar v01264 = v00023 * v00034;
  const Scalar v01249 = v00022 * v00026;
  const Scalar v00037 = std::cos(v00025);
  const bool v00020 = static_cast<Scalar>(1e-16) < v00019;
  Scalar v00036;
  Scalar v00038;
  Scalar v00043;
  Scalar v00046;
  if (v00020) {
    v00036 = v00014 * v01249;
    v00038 = v00037;
    v00043 = v00012 * v01249;
    v00046 = v00016 * v01249;
  } else {
    v00036 = v00014 * v01264;
    v00038 = v00034;
    v00043 = v00012 * v01264;
    v00046 = v00016 * v01264;
  }
  const Scalar v01440 = -v00038;
  const Scalar v01442 = -v00036;
  const Scalar v01322 = v00036 * v00046;
  const Scalar v01325 = v00038 * v00046;
  const Scalar v01321 = v00036 * v00043;
  const Scalar v00464 = v00046 * v00046;
  const Scalar v00458 = v00043 * v00043;
  const Scalar v00451 = v00043 * v00046;
  const Scalar v00459 = v00036 * v00036;
  const Scalar v01324 = v00038 * v00043;
  const Scalar v00055 = _point(2, 0);
  const Scalar v00483 = v01321 + v01325;
  const Scalar v00009 = _point(0, 0);
  const Scalar v01437 = -static_cast<Scalar>(2);
  const Scalar v00487 = v00458 + v00464;
  const Scalar v00474 = v00451 + v00036 * v00038;
  const Scalar v00050 = _point(1, 0);
  const Scalar v00465 = v00459 + v00464;
  const Scalar v00456 = v01322 + v01324;
  const Scalar v00452 = v00038 * v01442 + v00451;
  const Scalar v00462 = static_cast<Scalar>(1) + (v00458 + v00459) * v01437;
  const Scalar v00077 = _camera(4, 0);
  const Scalar v00063 = _camera(3, 0);
  const Scalar v00008 = _camera(5, 0);
  const Scalar v00924 = v00077 + v00050 * (static_cast<Scalar>(1) + v00487 * v01437) + static_cast<Scalar>(2) * (v00009 * v00483 + v00055 * (v00043 * v01440 + v01322));
  const Scalar v00919 = v00063 + v00009 * (static_cast<Scalar>(1) + v00465 * v01437) + static_cast<Scalar>(2) * (v00050 * (v00046 * v01440 + v01321) + v00055 * v00474);
  const Scalar v00914 = v00008 + v00055 * v00462 + static_cast<Scalar>(2) * (v00009 * v00452 + v00050 * v00456);
  const Scalar v01357 = v00914 * v00914;
  const Scalar v01149 = v00919 * v00919 + v00924 * v00924;
  const Scalar v00062 = static_cast<Scalar>(1) / v01357;
  const Scalar v01223 = static_cast<Scalar>(-1) * static_cast<Scalar>(-1);
  const Scalar v01403 = v01223 * (v00062 * v01149);
  const Scalar v00007 = _camera(8, 0);
  const Scalar v00090 = v00007 * v01403;
  const Scalar v00006 = _camera(7, 0);
  const Scalar v00091 = v00006 + v00090;
  const Scalar v00593 = v00090 + v00091;
  const Scalar v00093 = static_cast<Scalar>(1) + v00091 * v01403;
  const Scalar v00094 = static_cast<Scalar>(1) / v00914;
  const Scalar v00004 = _camera(6, 0);
  const Scalar v01445 = -v00062;
  const Scalar v01244 = v00093 * v00919;
  const Scalar v01363 = v01223 * ((static_cast<Scalar>(1) / (v00914 * v01357)) * v01149);
  const Scalar v01365 = (v00593 * v00924) * v01437;
  const Scalar v01369 = (v00593 * v00919) * v01437;
  const Scalar v01229 = static_cast<Scalar>(2) * v00593;
  const Scalar v01245 = v00093 * v00924;
  const Scalar v01240 = v00004 * v00094;
  const Scalar v01233 = v00062 * v00093;
  const Scalar v01371 = v01244 * v01445;
  const Scalar v01438 = -v00004;
  const Scalar v01443 = -v00919;
  if (static_cast<bool>(_residuals_D_camera)) {
    Scalar v00111;
    Scalar v00123;
    Scalar v00129;
    Scalar v00138;
    Scalar v00207;
    Scalar v00214;
    Scalar v00220;
    Scalar v00272;
    Scalar v00279;
    if (v00020) {
      const Scalar v01441 = -(static_cast<Scalar>(1) / (v00019 * v00019 * v00019));
      const Scalar v01384 = (static_cast<Scalar>(1) / v00018) * (v00023 * v00037);
      const Scalar v00505 = v00026 * v01441 + v01384;
      const Scalar v01375 = static_cast<Scalar>(-0.5) * v01249;
      const Scalar v01281 = v00014 * v00505;
      v00111 = v00012 * v01375;
      v00123 = v00012 * v01281;
      v00129 = v00012 * (v00016 * v00505);
      v00138 = v01260 * v01384 + v00026 * (v00022 + v01260 * v01441);
      v00207 = v00014 * v01375;
      v00214 = v00505 * v01279 + v01249;
      v00220 = v00016 * v01281;
      v00272 = v00016 * v01375;
      v00279 = v00505 * v01282 + v01249;
    } else {
      const Scalar v00109 = static_cast<Scalar>(1) / (v00033 * v00033 * v00033);
      const Scalar v01251 = v00109 * static_cast<Scalar>(-0.125);
      const Scalar v00105 = static_cast<Scalar>(-0.25);
      v00111 = v00105 * (v00012 * v00109);
      v00123 = v01251 * (v00012 * v00014);
      v00129 = v01251 * (v00012 * v00016);
      v00138 = v01251 * v01260 + v01264;
      v00207 = v00105 * (v00014 * v00109);
      v00214 = v01251 * v01279 + v01264;
      v00220 = v01251 * (v00014 * v00016);
      v00272 = v00105 * (v00016 * v00109);
      v00279 = v01251 * v01282 + v01264;
    }
    const Scalar v00565 = v00038 * v00129;
    const Scalar v00544 = v00038 * v00123;
    const Scalar v00649 = v00038 * v00220;
    const Scalar v01430 = v00565 + v00043 * v00272;
    const Scalar v00626 = v00046 * v00220;
    const Scalar v00612 = v00043 * v00220;
    const Scalar v00522 = v00036 * v00129;
    const Scalar v00532 = v00046 * v00129;
    const Scalar v01436 = v00046 * v00272 + v00038 * v00279;
    const Scalar v01428 = v00544 + v00043 * v00207;
    const Scalar v00617 = v00036 * v00220;
    const Scalar v00528 = v00036 * v00123;
    const Scalar v00523 = v00046 * v00123;
    const Scalar v01434 = v00046 * v00207 + v00649;
    const Scalar v01420 = v00043 * v00111 + v00038 * v00138;
    const Scalar v00537 = v00043 * v00123;
    const Scalar v00517 = v00043 * v00129;
    const Scalar v01429 = v00046 * v00111 + v00565;
    const Scalar v01432 = v00626 + v00036 * v00279;
    const Scalar v01422 = v00522 + v00612;
    const Scalar v01435 = v00649 + v00036 * v00272;
    const Scalar v01425 = v00532 + v00043 * v00279;
    const Scalar v01431 = v00617 + v00046 * v00214;
    const Scalar v01424 = v00528 + v00043 * v00214;
    const Scalar v01433 = v00036 * v00207 + v00038 * v00214;
    const Scalar v01423 = v00523 + v00612;
    const Scalar v01421 = v00522 + v00523;
    const Scalar v01426 = v00537 + v00036 * v00138;
    const Scalar v01427 = v00036 * v00111 + v00544;
    const Scalar v01419 = v00517 + v00046 * v00138;
    const Scalar v01439 = -static_cast<Scalar>(4);
    const Scalar v00706 = v00046 * v00279;
    const Scalar v00622 = v00036 * v00214;
    const Scalar v00527 = v00043 * v00138;
    const Scalar v01373 = v00050 * v01439;
    const Scalar v01372 = v00009 * v01439;
    const Scalar v01056 = (v00517 + v00706) * v01373 + static_cast<Scalar>(2) * (v00009 * (v01422 + v01436) + v00055 * (v01432 + -v01430));
    const Scalar v01046 = (v00617 + v00706) * v01372 + static_cast<Scalar>(2) * (v00050 * (v01422 + -v01436) + v00055 * (v01425 + v01435));
    const Scalar v01374 = v00055 * v01439;
    const Scalar v01010 = (v00537 + v00626) * v01373 + static_cast<Scalar>(2) * (v00009 * (v01424 + v01434) + v00055 * (v01431 + -v01428));
    const Scalar v01000 = (v00622 + v00626) * v01372 + static_cast<Scalar>(2) * (v00050 * (v01424 + -v01434) + v00055 * (v01423 + v01433));
    const Scalar v00964 = (v00527 + v00532) * v01373 + static_cast<Scalar>(2) * (v00009 * (v01426 + v01429) + v00055 * (v01421 + -v01420));
    const Scalar v00954 = (v00528 + v00532) * v01372 + static_cast<Scalar>(2) * (v00050 * (v01426 + -v01429) + v00055 * (v01419 + v01427));
    const Scalar v01036 = (v00517 + v00617) * v01374 + static_cast<Scalar>(2) * (v00009 * (v01425 + -v01435) + v00050 * (v01430 + v01432));
    const Scalar v00990 = (v00537 + v00622) * v01374 + static_cast<Scalar>(2) * (v00009 * (v01423 + -v01433) + v00050 * (v01428 + v01431));
    const Scalar v00944 = (v00527 + v00528) * v01374 + static_cast<Scalar>(2) * (v00009 * (v01419 + -v01427) + v00050 * (v01420 + v01421));
    const Scalar v01181 = v01036 * v01363 + (v00919 * v01046 + v00924 * v01056) * v01445;
    const Scalar v01169 = v00990 * v01363 + (v00919 * v01000 + v00924 * v01010) * v01445;
    const Scalar v01157 = v00944 * v01363 + (v00919 * v00954 + v00924 * v00964) * v01445;
    const Scalar v01366 = v01223 * v01445;
    const Scalar v01238 = v00062 * v00924;
    const Scalar v01379 = v01245 * v01445;
    const Scalar v01237 = v00062 * v00919;
    const Scalar v01418 = (v01223 * v01240) * ((v01149 * v01149) * v01366);
    const Scalar v01414 = v01240 * v01366;
    const Scalar v01265 = v00093 * v00094;
    const Scalar v01395 = (v00094 * v01229 * v01363 + v01233) * v01223;
    const Scalar v01383 = v00094 * v01438;
    const Scalar v00371 = v01365 * (v01237 * v01240);
    _residuals_D_camera(0, 0) = (v00944 * v01371 + v00094 * (v00093 * v00954 + v01157 * v01369)) * v01438;
    _residuals_D_camera(0, 1) = (v00990 * v01371 + v00094 * (v00093 * v01000 + v01169 * v01369)) * v01438;
    _residuals_D_camera(0, 2) = (v01036 * v01371 + v00094 * (v00093 * v01046 + v01181 * v01369)) * v01438;
    _residuals_D_camera(0, 3) = (v00093 + v01237 * (v00919 * v01229)) * v01383;
    _residuals_D_camera(0, 4) = v00371;
    _residuals_D_camera(0, 5) = (v00004 * v00919) * v01395;
    _residuals_D_camera(0, 6) = v01265 * v01443;
    _residuals_D_camera(0, 7) = (v00919 * v01149) * v01414;
    _residuals_D_camera(0, 8) = v01237 * v01418;
    _residuals_D_camera(1, 0) = (v00944 * v01379 + v00094 * (v00093 * v00964 + v01157 * v01365)) * v01438;
    _residuals_D_camera(1, 1) = (v00990 * v01379 + v00094 * (v00093 * v01010 + v01169 * v01365)) * v01438;
    _residuals_D_camera(1, 2) = (v01036 * v01379 + v00094 * (v00093 * v01056 + v01181 * v01365)) * v01438;
    _residuals_D_camera(1, 3) = v00371;
    _residuals_D_camera(1, 4) = (v00093 + v01238 * (v00924 * v01229)) * v01383;
    _residuals_D_camera(1, 5) = (v00004 * v00924) * v01395;
    _residuals_D_camera(1, 6) = v01265 * -v00924;
    _residuals_D_camera(1, 7) = (v00924 * v01149) * v01414;
    _residuals_D_camera(1, 8) = v01238 * v01418;
  }
  if (static_cast<bool>(_residuals_D_point)) {
    const Scalar v00876 = v00046 * v01442 + v01324;
    const Scalar v01234 = static_cast<Scalar>(2) * v00919;
    const Scalar v00846 = v00043 * v01442 + v01325;
    const Scalar v00852 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v00487;
    const Scalar v01390 = v00924 * v01437;
    const Scalar v00822 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v00465;
    const Scalar v01364 = v01223 * v01229;
    const Scalar v01411 = v00924 * v01364;
    const Scalar v01213 = v00462 * v01363 + (v00474 * v01443 + v00876 * v00924) * (static_cast<Scalar>(2) * v00062);
    const Scalar v01242 = static_cast<Scalar>(2) * v00093;
    const Scalar v01205 = (static_cast<Scalar>(2) * v00456) * v01363 + v00062 * (v00852 * v00924 + v00846 * v01234);
    const Scalar v01201 = (static_cast<Scalar>(2) * v00452) * v01363 + v00062 * (v00822 * v00919 + v00483 * v01390);
    const Scalar v01410 = v00919 * v01364;
    const Scalar v01367 = v01223 * v01233;
    const Scalar v01409 = v00456 * v01367;
    _residuals_D_point(0, 0) = v00004 * (v00452 * (v01234 * v01367) + v00094 * (v00093 * v00822 + v01201 * v01410));
    _residuals_D_point(0, 1) = v00004 * (v01234 * v01409 + v00094 * (v00846 * v01242 + v01205 * v01410));
    _residuals_D_point(0, 2) = (v00462 * v01371 + v00094 * (v00474 * v01242 + v01213 * v01369)) * v01438;
    _residuals_D_point(1, 0) = ((v00452 * v01233) * v01390 + v00094 * (v00483 * v01242 + v01201 * v01365)) * v01438;
    _residuals_D_point(1, 1) = v00004 * ((static_cast<Scalar>(2) * v00924) * v01409 + v00094 * (v00093 * v00852 + v01205 * v01411));
    _residuals_D_point(1, 2) = v00004 * (v00462 * v00924 * v01367 + v00094 * (v00876 * v01242 + v01213 * v01411));
  }
  const Scalar v00097 = _measured_xy(1, 0);
  const Scalar v00001 = _measured_xy(0, 0);
  _residuals(0, 0) = -(v00001 + v01240 * v01244);
  _residuals(1, 0) = -(v00097 + v01240 * v01245);
}

} // namespace gen
