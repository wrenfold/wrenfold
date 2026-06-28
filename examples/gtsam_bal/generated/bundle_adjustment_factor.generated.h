// Machine generated code.
#pragma once
#include <cmath>
#include <cstdint>

#include <wrenfold/span.h>


namespace gen {

template <typename Scalar, typename T1, typename T2, typename T3, typename T4>
void bundle_adjustment_factor(const gtsam::SfmCamera& camera, const T1& p_world, T2&& p_image, T3&& p_image_D_camera, T4&& p_image_D_point)
{
  auto _p_world = wf::make_input_span<3, 1>(p_world);
  auto _p_image = wf::make_output_span<2, 1>(p_image);
  auto _p_image_D_camera = wf::make_optional_output_span<2, 9>(p_image_D_camera);
  auto _p_image_D_point = wf::make_optional_output_span<2, 3>(p_image_D_point);

  // Operation counts:
  // add: 144
  // branch: 2
  // divide: 8
  // multiply: 328
  // negate: 13
  // total: 495
  
  const Scalar v00008 = camera.pose().rotation().toQuaternion().y();
  const Scalar v00021 = camera.pose().rotation().toQuaternion().z();
  const Scalar v00003 = camera.pose().rotation().toQuaternion().x();
  const Scalar v01275 = -v00008;
  const Scalar v00023 = camera.pose().rotation().toQuaternion().w();
  const Scalar v01276 = -v00021;
  const Scalar v01274 = -v00003;
  const Scalar v01139 = v00003 * v00021;
  const Scalar v00012 = _p_world(2, 0);
  const Scalar v01170 = v00021 * v00023;
  const Scalar v01137 = v00003 * v00008;
  const Scalar v00027 = _p_world(1, 0);
  const Scalar v00377 = v00021 * v00021;
  const Scalar v00364 = v00008 * v00008;
  const Scalar v01163 = v00008 * v00021;
  const Scalar v01140 = v00003 * v00023;
  const Scalar v00017 = _p_world(0, 0);
  const Scalar v00363 = v00003 * v00003;
  const Scalar v01164 = v00008 * v00023;
  const Scalar v00400 = v00023 * v01275 + v01139;
  const Scalar v00015 = v00012 + -camera.pose().translation()(2, 0);
  const Scalar v00396 = v01137 + v01170;
  const Scalar v00030 = v00027 + -camera.pose().translation()(1, 0);
  const Scalar v01273 = -static_cast<Scalar>(2);
  const Scalar v00391 = v00364 + v00377;
  const Scalar v00387 = v01140 + v01163;
  const Scalar v00383 = v00023 * v01276 + v01137;
  const Scalar v00020 = v00017 + -camera.pose().translation()(0, 0);
  const Scalar v00378 = v00363 + v00377;
  const Scalar v00374 = v00023 * v01274 + v01163;
  const Scalar v00370 = v01139 + v01164;
  const Scalar v00365 = v00363 + v00364;
  const Scalar v00393 = static_cast<Scalar>(1) + v00391 * v01273;
  const Scalar v00380 = static_cast<Scalar>(1) + v00378 * v01273;
  const Scalar v00367 = static_cast<Scalar>(1) + v00365 * v01273;
  const Scalar v00797 = v00020 * v00393 + static_cast<Scalar>(2) * (v00030 * v00396 + v00015 * v00400);
  const Scalar v00792 = v00030 * v00380 + static_cast<Scalar>(2) * (v00020 * v00383 + v00015 * v00387);
  const Scalar v00787 = v00015 * v00367 + static_cast<Scalar>(2) * (v00020 * v00370 + v00030 * v00374);
  const Scalar v01217 = v00787 * v00787;
  const Scalar v00404 = v00792 * v00792 + v00797 * v00797;
  const Scalar v01092 = (static_cast<Scalar>(1) / v01217) * v00404;
  const Scalar v00061 = camera.calibration().k2();
  const Scalar v00063 = camera.calibration().k1();
  const Scalar v00068 = camera.calibration().fx();
  const Scalar v00066 = static_cast<Scalar>(1) + (v00061 * v01092 + v00063) * v01092;
  const Scalar v00083 = v00787 * v01217;
  const Scalar v01078 = static_cast<Scalar>(2) * v00792;
  const Scalar v01079 = static_cast<Scalar>(2) * v00797;
  const Scalar v01103 = (static_cast<Scalar>(1) / v00787) * v00068;
  const Scalar v01083 = v00066 * v00792;
  const Scalar v01084 = v00066 * v00797;
  if (static_cast<bool>(_p_image_D_camera)) {
    const Scalar v01277 = -v00023;
    const Scalar v01138 = v00003 * v00015;
    const Scalar v01162 = v00008 * v00020;
    const Scalar v03283 = static_cast<Scalar>(1) / v01217;
    const Scalar v01174 = v00030 * static_cast<Scalar>(4);
    const Scalar v01169 = v00020 * static_cast<Scalar>(4);
    const Scalar v03211 = v00404 * v03283;
    const Scalar v00482 = v00015 * v00021;
    const Scalar v00446 = v00008 * v00030;
    const Scalar v01171 = v00021 * v00030;
    const Scalar v00445 = v00003 * v00020;
    const Scalar v03067 = v00061 * v03211;
    const Scalar v00576 = v00008 * v01274 + v01170;
    const Scalar v00573 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v00391;
    const Scalar v00637 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v00378;
    const Scalar v00084 = static_cast<Scalar>(1) / v00083;
    const Scalar v00602 = v00021 * v01274 + v01164;
    const Scalar v00458 = v01174 * v01276 + static_cast<Scalar>(2) * (v00020 * v01277 + v00008 * v00015);
    const Scalar v00453 = v01169 * v01276 + static_cast<Scalar>(2) * (v01138 + v00023 * v00030);
    const Scalar v00483 = v00446 + v00482;
    const Scalar v00489 = v01174 * v01274 + static_cast<Scalar>(2) * (v01162 + v00015 * v00023);
    const Scalar v01167 = v00015 * static_cast<Scalar>(4);
    const Scalar v00412 = v00015 * v01275 + v01171;
    const Scalar v00416 = v00020 * v01276 + v01138;
    const Scalar v00519 = v00445 + v00482;
    const Scalar v00516 = v01169 * v01275 + static_cast<Scalar>(2) * (v00015 * v01277 + v00003 * v00030);
    const Scalar v02844 = v00063 + v03067;
    const Scalar v01279 = -v00084;
    const Scalar v01221 = (v00084 * v00404) * v01273;
    const Scalar v00627 = v00021 * v01275 + v01140;
    const Scalar v01080 = static_cast<Scalar>(2) * v03283;
    const Scalar v01224 = v00404 * v01279;
    const Scalar v00599 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v00365;
    const Scalar v00447 = v00445 + v00446;
    const Scalar v00480 = v01167 * v01274 + static_cast<Scalar>(2) * (v00030 * v01277 + v00020 * v00021);
    const Scalar v00408 = v00030 * v01274 + v01162;
    const Scalar v00511 = v01167 * v01275 + static_cast<Scalar>(2) * (v00020 * v00023 + v01171);
    const Scalar v01075 = static_cast<Scalar>(2) * (v02844 + v03067);
    const Scalar v02314 = static_cast<Scalar>(1) + v02844 * v03211;
    const Scalar v01218 = v00792 * v01075;
    const Scalar v00994 = (v00370 * v00404) * v01273 * v01279 + (v00573 * v00797 + v00576 * v01078) * v03283;
    const Scalar v01073 = static_cast<Scalar>(2) * v02314;
    const Scalar v01006 = v00627 * v01221 + (v00637 * v00792 + (v00396 * v00797) * v01273) * v03283;
    const Scalar v01002 = v00599 * v01224 + (v00602 * v00797 + v00387 * -v00792) * v01080;
    const Scalar v00958 = v00447 * v01221 + (v00453 * v00797 + v00458 * v00792) * v03283;
    const Scalar v00966 = v00480 * v01224 + (v00489 * v00792 + v00483 * v01079) * v03283;
    const Scalar v00954 = v00408 * v01221 + (v00416 * v00792 + v00412 * v00797) * v01080;
    const Scalar v00978 = v00511 * v01224 + (v00516 * v00797 + v00519 * v01078) * v03283;
    const Scalar v01219 = v00797 * v01075;
    const Scalar v01280 = -v02314;
    const Scalar v01081 = v00792 * v03283;
    const Scalar v01220 = (v02314 * v03283) * v01273;
    const Scalar v01082 = v00797 * v03283;
    const Scalar v01774 = static_cast<Scalar>(1) / v00787;
    const Scalar v01230 = v00370 * v01273;
    const Scalar v01222 = v01081 * v01280;
    const Scalar v01261 = v00792 * v01220;
    const Scalar v01223 = v01082 * v01280;
    const Scalar v01262 = v00797 * v01220;
    const Scalar v00887 = v01222 * v01230 + (v00576 * v01073 + v00994 * v01218) * v01774;
    const Scalar v00895 = v00627 * v01261 + (v00637 * v02314 + v01006 * v01218) * v01774;
    const Scalar v00891 = v00599 * v01222 + ((v00387 * v02314) * v01273 + v01002 * v01218) * v01774;
    const Scalar v00875 = v00447 * v01261 + (v00458 * v02314 + v00958 * v01218) * v01774;
    const Scalar v00879 = v00480 * v01222 + (v00489 * v02314 + v00966 * v01218) * v01774;
    const Scalar v00871 = v00408 * v01261 + (v00416 * v01073 + v00954 * v01218) * v01774;
    const Scalar v00883 = v00511 * v01222 + (v00519 * v01073 + v00978 * v01218) * v01774;
    const Scalar v00836 = v01223 * v01230 + (v00573 * v02314 + v00994 * v01219) * v01774;
    const Scalar v00852 = v00627 * v01262 + ((v00396 * v02314) * v01273 + v01006 * v01219) * v01774;
    const Scalar v00844 = v00599 * v01223 + (v00602 * v01073 + v01002 * v01219) * v01774;
    const Scalar v00812 = v00447 * v01262 + (v00453 * v02314 + v00958 * v01219) * v01774;
    const Scalar v00820 = v00480 * v01223 + (v00483 * v01073 + v00966 * v01219) * v01774;
    const Scalar v00804 = v00408 * v01262 + (v00412 * v01073 + v00954 * v01219) * v01774;
    const Scalar v00828 = v00511 * v01223 + (v00516 * v02314 + v00978 * v01219) * v01774;
    const Scalar v01094 = static_cast<Scalar>(2) * v00068;
    const Scalar v01195 = v00068 * v00367;
    const Scalar v01196 = v00068 * v00380;
    const Scalar v01197 = v00068 * v00393;
    const Scalar v01105 = v00068 * static_cast<Scalar>(0.5);
    const Scalar v01104 = v00068 * static_cast<Scalar>(-0.5);
    const Scalar v01525 = v00404 * (v00068 * v01774);
    const Scalar v01268 = v01082 * v01525;
    _p_image_D_camera(0, 0) = (v00003 * v00804 + v00008 * v00812) * v01104 + (v00023 * v00820 + v00021 * v00828) * v01105;
    _p_image_D_camera(0, 1) = (v00021 * v00820 + v00008 * v00804) * v01104 + (v00023 * v00828 + v00003 * v00812) * v01105;
    _p_image_D_camera(0, 2) = (v00003 * v00828 + v00021 * v00804) * v01104 + (v00008 * v00820 + v00023 * v00812) * v01105;
    _p_image_D_camera(0, 3) = v00836 * v01197 + (v00400 * v00844 + v00396 * v00852) * v01094;
    _p_image_D_camera(0, 4) = v00852 * v01196 + (v00383 * v00836 + v00387 * v00844) * v01094;
    _p_image_D_camera(0, 5) = v00844 * v01195 + (v00374 * v00852 + v00370 * v00836) * v01094;
    _p_image_D_camera(0, 6) = v01084 * v01774;
    _p_image_D_camera(0, 7) = v01268;
    _p_image_D_camera(0, 8) = v01268 * v03211;
    _p_image_D_camera(1, 0) = (v00003 * v00871 + v00008 * v00875) * v01104 + (v00023 * v00879 + v00021 * v00883) * v01105;
    _p_image_D_camera(1, 1) = (v00021 * v00879 + v00008 * v00871) * v01104 + (v00023 * v00883 + v00003 * v00875) * v01105;
    _p_image_D_camera(1, 2) = (v00003 * v00883 + v00021 * v00871) * v01104 + (v00008 * v00879 + v00023 * v00875) * v01105;
    _p_image_D_camera(1, 3) = v00887 * v01197 + (v00400 * v00891 + v00396 * v00895) * v01094;
    _p_image_D_camera(1, 4) = v00895 * v01196 + (v00383 * v00887 + v00387 * v00891) * v01094;
    _p_image_D_camera(1, 5) = v00891 * v01195 + (v00374 * v00895 + v00370 * v00887) * v01094;
    _p_image_D_camera(1, 6) = v01083 * v01774;
    _p_image_D_camera(1, 7) = v01081 * v01525;
    _p_image_D_camera(1, 8) = (v01081 * (v00404 * v01103)) * v03211;
  }
  if (static_cast<bool>(_p_image_D_point)) {
    const Scalar v01505 = static_cast<Scalar>(1) / v01217;
    const Scalar v01493 = v00404 * v01505;
    const Scalar v01469 = v00061 * v01493;
    const Scalar v01467 = static_cast<Scalar>(1) / v00083;
    const Scalar v01433 = v00063 + v01469;
    const Scalar v01381 = v01273 * (v00404 * v01467);
    const Scalar v01353 = static_cast<Scalar>(2) * (v01433 + v01469);
    const Scalar v01352 = static_cast<Scalar>(1) + v01433 * v01493;
    const Scalar v01327 = v00792 * v01353;
    const Scalar v01070 = v00367 * (v00404 * -v01467) + (v00400 * v00797 + v00387 * v00792) * (static_cast<Scalar>(2) * v01505);
    const Scalar v01326 = static_cast<Scalar>(2) * v01352;
    const Scalar v01062 = v00374 * v01381 + (v00380 * v00792 + v00396 * v01079) * v01505;
    const Scalar v01058 = v00370 * v01381 + (v00393 * v00797 + v00383 * v01078) * v01505;
    const Scalar v01316 = v00797 * v01353;
    const Scalar v01302 = -v01352;
    const Scalar v01300 = v01273 * (v01352 * v01505);
    const Scalar v01294 = static_cast<Scalar>(1) / v00787;
    _p_image_D_point(0, 0) = v00068 * ((v00370 * v00797) * v01300 + (v00393 * v01352 + v01058 * v01316) * v01294);
    _p_image_D_point(0, 1) = v00068 * (v00374 * (v00797 * v01300) + (v00396 * v01326 + v01062 * v01316) * v01294);
    _p_image_D_point(0, 2) = v00068 * (v00367 * ((v00797 * v01505) * v01302) + (v00400 * v01326 + v01070 * v01316) * v01294);
    _p_image_D_point(1, 0) = v00068 * ((v00370 * v00792) * v01300 + (v00383 * v01326 + v01058 * v01327) * v01294);
    _p_image_D_point(1, 1) = v00068 * (v00374 * (v00792 * v01300) + (v00380 * v01352 + v01062 * v01327) * v01294);
    _p_image_D_point(1, 2) = v00068 * (v00367 * ((v00792 * v01505) * v01302) + (v00387 * v01326 + v01070 * v01327) * v01294);
  }
  _p_image(0, 0) = v01084 * v01103;
  _p_image(1, 0) = v01083 * v01103;
}

} // namespace gen