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
  // add: 190
  // branch: 4
  // call: 7
  // compare: 1
  // divide: 12
  // multiply: 375
  // negate: 26
  // total: 615
  
  const Scalar v000016 = _camera(2, 0);
  const Scalar v000014 = _camera(1, 0);
  const Scalar v000012 = _camera(0, 0);
  const Scalar v001282 = v000016 * v000016;
  const Scalar v001279 = v000014 * v000014;
  const Scalar v001260 = v000012 * v000012;
  const Scalar v000018 = v001260 + v001279 + v001282;
  const Scalar v000449 = static_cast<Scalar>(1) + v000018 * static_cast<Scalar>(0.25);
  const Scalar v000023 = static_cast<Scalar>(0.5);
  const bool v000020 = static_cast<Scalar>(1e-16) < std::sqrt(v000018);
  Scalar v000036;
  Scalar v000038;
  Scalar v000043;
  Scalar v000046;
  if (v000020) {
    const Scalar v011620 = std::sqrt(v000018);
    const Scalar v000025 = v000023 * v011620;
    const Scalar v001249 = (static_cast<Scalar>(1) / v011620) * std::sin(v000025);
    v000036 = v000014 * v001249;
    v000038 = std::cos(v000025);
    v000043 = v000012 * v001249;
    v000046 = v000016 * v001249;
  } else {
    const Scalar v001264 = v000023 * (static_cast<Scalar>(1) / std::sqrt(v000449));
    v000036 = v000014 * v001264;
    v000038 = static_cast<Scalar>(1) / std::sqrt(v000449);
    v000043 = v000012 * v001264;
    v000046 = v000016 * v001264;
  }
  const Scalar v001440 = -v000038;
  const Scalar v001322 = v000036 * v000046;
  const Scalar v001321 = v000036 * v000043;
  const Scalar v000464 = v000046 * v000046;
  const Scalar v000458 = v000043 * v000043;
  const Scalar v000451 = v000043 * v000046;
  const Scalar v000459 = v000036 * v000036;
  const Scalar v000055 = _point(2, 0);
  const Scalar v000009 = _point(0, 0);
  const Scalar v001437 = -static_cast<Scalar>(2);
  const Scalar v000050 = _point(1, 0);
  const Scalar v000077 = _camera(4, 0);
  const Scalar v000063 = _camera(3, 0);
  const Scalar v000008 = _camera(5, 0);
  const Scalar v000924 = v000077 + v000050 * (static_cast<Scalar>(1) + (v000458 + v000464) * v001437) + static_cast<Scalar>(2) * (v000009 * (v001321 + v000038 * v000046) + v000055 * (v000043 * v001440 + v001322));
  const Scalar v000919 = v000063 + v000009 * (static_cast<Scalar>(1) + (v000459 + v000464) * v001437) + static_cast<Scalar>(2) * (v000050 * (v000046 * v001440 + v001321) + v000055 * (v000451 + v000036 * v000038));
  const Scalar v000914 = v000008 + v000055 * (static_cast<Scalar>(1) + (v000458 + v000459) * v001437) + static_cast<Scalar>(2) * (v000009 * (v000038 * -v000036 + v000451) + v000050 * (v001322 + v000038 * v000043));
  const Scalar v001223 = static_cast<Scalar>(-1) * static_cast<Scalar>(-1);
  const Scalar v001403 = v001223 * ((static_cast<Scalar>(1) / (v000914 * v000914)) * (v000919 * v000919 + v000924 * v000924));
  const Scalar v000007 = _camera(8, 0);
  const Scalar v000006 = _camera(7, 0);
  const Scalar v000093 = static_cast<Scalar>(1) + (v000006 + v000007 * v001403) * v001403;
  const Scalar v000004 = _camera(6, 0);
  const Scalar v001245 = v000093 * v000924;
  const Scalar v001244 = v000093 * v000919;
  const Scalar v001240 = v000004 * (static_cast<Scalar>(1) / v000914);
  const Scalar v001438 = -v000004;
  if (static_cast<bool>(_residuals_D_camera)) {
    Scalar v000111;
    Scalar v000123;
    Scalar v000129;
    Scalar v000138;
    Scalar v000207;
    Scalar v000214;
    Scalar v000220;
    Scalar v000272;
    Scalar v000279;
    if (v000020) {
      const Scalar v011555 = std::sqrt(v000018);
      const Scalar v011544 = v000023 * v011555;
      const Scalar v001441 = -(static_cast<Scalar>(1) / (v011555 * v011555 * v011555));
      const Scalar v011507 = std::sin(v011544);
      const Scalar v011479 = static_cast<Scalar>(1) / v011555;
      const Scalar v001384 = (static_cast<Scalar>(1) / v000018) * (v000023 * std::cos(v011544));
      const Scalar v011458 = v011479 * v011507;
      const Scalar v000505 = v001441 * v011507 + v001384;
      const Scalar v001375 = static_cast<Scalar>(-0.5) * v011458;
      const Scalar v001281 = v000014 * v000505;
      v000111 = v000012 * v001375;
      v000123 = v000012 * v001281;
      v000129 = v000012 * (v000016 * v000505);
      v000138 = v001260 * v001384 + (v001260 * v001441 + v011479) * v011507;
      v000207 = v000014 * v001375;
      v000214 = v000505 * v001279 + v011458;
      v000220 = v000016 * v001281;
      v000272 = v000016 * v001375;
      v000279 = v000505 * v001282 + v011458;
    } else {
      const Scalar v011594 = std::sqrt(v000449);
      const Scalar v000109 = static_cast<Scalar>(1) / (v011594 * v011594 * v011594);
      const Scalar v001251 = v000109 * static_cast<Scalar>(-0.125);
      const Scalar v000105 = static_cast<Scalar>(-0.25);
      const Scalar v011559 = v000023 * (static_cast<Scalar>(1) / v011594);
      v000111 = v000105 * (v000012 * v000109);
      v000123 = v001251 * (v000012 * v000014);
      v000129 = v001251 * (v000012 * v000016);
      v000138 = v001251 * v001260 + v011559;
      v000207 = v000105 * (v000014 * v000109);
      v000214 = v001251 * v001279 + v011559;
      v000220 = v001251 * (v000014 * v000016);
      v000272 = v000105 * (v000016 * v000109);
      v000279 = v001251 * v001282 + v011559;
    }
    const Scalar v011421 = -v000038;
    const Scalar v011403 = v000036 * v000046;
    const Scalar v011400 = v000036 * v000043;
    const Scalar v011399 = v000046 * v000046;
    const Scalar v011398 = v000043 * v000043;
    const Scalar v011396 = v000043 * v000046;
    const Scalar v011392 = v000036 * v000036;
    const Scalar v000565 = v000038 * v000129;
    const Scalar v000544 = v000038 * v000123;
    const Scalar v000649 = v000038 * v000220;
    const Scalar v001430 = v000565 + v000043 * v000272;
    const Scalar v000626 = v000046 * v000220;
    const Scalar v000612 = v000043 * v000220;
    const Scalar v000522 = v000036 * v000129;
    const Scalar v000532 = v000046 * v000129;
    const Scalar v001436 = v000046 * v000272 + v000038 * v000279;
    const Scalar v001428 = v000544 + v000043 * v000207;
    const Scalar v000617 = v000036 * v000220;
    const Scalar v000528 = v000036 * v000123;
    const Scalar v000523 = v000046 * v000123;
    const Scalar v001434 = v000046 * v000207 + v000649;
    const Scalar v001420 = v000043 * v000111 + v000038 * v000138;
    const Scalar v000537 = v000043 * v000123;
    const Scalar v000517 = v000043 * v000129;
    const Scalar v001429 = v000046 * v000111 + v000565;
    const Scalar v009127 = v000077 + v000050 * (static_cast<Scalar>(1) + v001437 * (v011398 + v011399)) + static_cast<Scalar>(2) * (v000009 * (v011400 + v000038 * v000046) + v000055 * (v000043 * v011421 + v011403));
    const Scalar v009126 = v000063 + v000009 * (static_cast<Scalar>(1) + v001437 * (v011392 + v011399)) + static_cast<Scalar>(2) * (v000050 * (v000046 * v011421 + v011400) + v000055 * (v011396 + v000036 * v000038));
    const Scalar v009125 = v000008 + v000055 * (static_cast<Scalar>(1) + v001437 * (v011392 + v011398)) + static_cast<Scalar>(2) * (v000009 * (v000038 * -v000036 + v011396) + v000050 * (v000038 * v000043 + v011403));
    const Scalar v001432 = v000626 + v000036 * v000279;
    const Scalar v001422 = v000522 + v000612;
    const Scalar v001435 = v000649 + v000036 * v000272;
    const Scalar v001425 = v000532 + v000043 * v000279;
    const Scalar v001431 = v000617 + v000046 * v000214;
    const Scalar v001424 = v000528 + v000043 * v000214;
    const Scalar v001433 = v000036 * v000207 + v000038 * v000214;
    const Scalar v001423 = v000523 + v000612;
    const Scalar v001421 = v000522 + v000523;
    const Scalar v001426 = v000537 + v000036 * v000138;
    const Scalar v001427 = v000036 * v000111 + v000544;
    const Scalar v001419 = v000517 + v000046 * v000138;
    const Scalar v008502 = v009125 * v009125;
    const Scalar v007992 = v009126 * v009126 + v009127 * v009127;
    const Scalar v007991 = static_cast<Scalar>(1) / v008502;
    const Scalar v001439 = -static_cast<Scalar>(4);
    const Scalar v000706 = v000046 * v000279;
    const Scalar v000622 = v000036 * v000214;
    const Scalar v000527 = v000043 * v000138;
    const Scalar v001373 = v000050 * v001439;
    const Scalar v001372 = v000009 * v001439;
    const Scalar v007047 = v001223 * (v007991 * v007992);
    const Scalar v006631 = v000007 * v007047;
    const Scalar v001056 = (v000517 + v000706) * v001373 + static_cast<Scalar>(2) * (v000009 * (v001422 + v001436) + v000055 * (v001432 + -v001430));
    const Scalar v001046 = (v000617 + v000706) * v001372 + static_cast<Scalar>(2) * (v000050 * (v001422 + -v001436) + v000055 * (v001425 + v001435));
    const Scalar v001374 = v000055 * v001439;
    const Scalar v001010 = (v000537 + v000626) * v001373 + static_cast<Scalar>(2) * (v000009 * (v001424 + v001434) + v000055 * (v001431 + -v001428));
    const Scalar v001000 = (v000622 + v000626) * v001372 + static_cast<Scalar>(2) * (v000050 * (v001424 + -v001434) + v000055 * (v001423 + v001433));
    const Scalar v000964 = (v000527 + v000532) * v001373 + static_cast<Scalar>(2) * (v000009 * (v001426 + v001429) + v000055 * (v001421 + -v001420));
    const Scalar v000954 = (v000528 + v000532) * v001372 + static_cast<Scalar>(2) * (v000050 * (v001426 + -v001429) + v000055 * (v001419 + v001427));
    const Scalar v006239 = v000006 + v006631;
    const Scalar v000593 = v006239 + v006631;
    const Scalar v001445 = -v007991;
    const Scalar v001363 = v001223 * ((static_cast<Scalar>(1) / (v008502 * v009125)) * v007992);
    const Scalar v001036 = (v000517 + v000617) * v001374 + static_cast<Scalar>(2) * (v000009 * (v001425 + -v001435) + v000050 * (v001430 + v001432));
    const Scalar v000990 = (v000537 + v000622) * v001374 + static_cast<Scalar>(2) * (v000009 * (v001423 + -v001433) + v000050 * (v001428 + v001431));
    const Scalar v000944 = (v000527 + v000528) * v001374 + static_cast<Scalar>(2) * (v000009 * (v001419 + -v001427) + v000050 * (v001420 + v001421));
    const Scalar v001365 = (v000593 * v009127) * v001437;
    const Scalar v001181 = v001036 * v001363 + (v001046 * v009126 + v001056 * v009127) * v001445;
    const Scalar v005469 = static_cast<Scalar>(1) + v006239 * v007047;
    const Scalar v001169 = v000990 * v001363 + (v001000 * v009126 + v001010 * v009127) * v001445;
    const Scalar v001157 = v000944 * v001363 + (v000954 * v009126 + v000964 * v009127) * v001445;
    const Scalar v001369 = (v000593 * v009126) * v001437;
    const Scalar v001229 = static_cast<Scalar>(2) * v000593;
    const Scalar v005340 = static_cast<Scalar>(1) / v009125;
    const Scalar v001366 = v001223 * v001445;
    const Scalar v001238 = v007991 * v009127;
    const Scalar v005240 = v001445 * (v005469 * v009127);
    const Scalar v005233 = v000004 * v005340;
    const Scalar v001237 = v007991 * v009126;
    const Scalar v005205 = v001445 * (v005469 * v009126);
    const Scalar v001415 = (v007992 * v007992) * v001366;
    const Scalar v001414 = v001366 * v005233;
    const Scalar v001265 = v005340 * v005469;
    const Scalar v001395 = (v001229 * v001363 * v005340 + v005469 * v007991) * v001223;
    const Scalar v001383 = v001438 * v005340;
    const Scalar v000371 = v001365 * (v001237 * v005233);
    _residuals_D_camera(0, 0) = (v000944 * v005205 + (v000954 * v005469 + v001157 * v001369) * v005340) * v001438;
    _residuals_D_camera(0, 1) = (v000990 * v005205 + (v001000 * v005469 + v001169 * v001369) * v005340) * v001438;
    _residuals_D_camera(0, 2) = (v001036 * (v001244 * v001445) + (v001046 * v005469 + v001181 * v001369) * v005340) * v001438;
    _residuals_D_camera(0, 3) = (v001237 * (v001229 * v009126) + v005469) * v001383;
    _residuals_D_camera(0, 4) = v000371;
    _residuals_D_camera(0, 5) = (v000004 * v009126) * v001395;
    _residuals_D_camera(0, 6) = v001265 * -v009126;
    _residuals_D_camera(0, 7) = (v007992 * v009126) * v001414;
    _residuals_D_camera(0, 8) = v001237 * (v001415 * (v001223 * v005233));
    _residuals_D_camera(1, 0) = (v000944 * v005240 + (v000964 * v005469 + v001157 * v001365) * v005340) * v001438;
    _residuals_D_camera(1, 1) = (v000990 * v005240 + (v001010 * v005469 + v001169 * v001365) * v005340) * v001438;
    _residuals_D_camera(1, 2) = (v001036 * (v001245 * v001445) + (v001056 * v005469 + v001181 * v001365) * v005340) * v001438;
    _residuals_D_camera(1, 3) = v000371;
    _residuals_D_camera(1, 4) = (v001238 * (v001229 * v009127) + v005469) * v001383;
    _residuals_D_camera(1, 5) = (v000004 * v009127) * v001395;
    _residuals_D_camera(1, 6) = v001265 * -v009127;
    _residuals_D_camera(1, 7) = (v007992 * v009127) * v001414;
    _residuals_D_camera(1, 8) = v001238 * ((v001223 * v001240) * v001415);
  }
  if (static_cast<bool>(_residuals_D_point)) {
    const Scalar v005160 = -v000038;
    const Scalar v005158 = -v000036;
    const Scalar v005148 = v000036 * v000046;
    const Scalar v005146 = v000038 * v000046;
    const Scalar v005145 = v000036 * v000043;
    const Scalar v005144 = v000046 * v000046;
    const Scalar v005143 = v000043 * v000043;
    const Scalar v005141 = v000043 * v000046;
    const Scalar v005137 = v000036 * v000036;
    const Scalar v005136 = v000038 * v000043;
    const Scalar v005036 = v005145 + v005146;
    const Scalar v005035 = v005143 + v005144;
    const Scalar v005034 = v005141 + v000036 * v000038;
    const Scalar v005032 = v005137 + v005144;
    const Scalar v005031 = v005136 + v005148;
    const Scalar v005030 = v000038 * v005158 + v005141;
    const Scalar v004435 = static_cast<Scalar>(1) + v001437 * (v005137 + v005143);
    const Scalar v003589 = v000077 + v000050 * (static_cast<Scalar>(1) + v001437 * v005035) + static_cast<Scalar>(2) * (v000009 * v005036 + v000055 * (v000043 * v005160 + v005148));
    const Scalar v003588 = v000063 + v000009 * (static_cast<Scalar>(1) + v001437 * v005032) + static_cast<Scalar>(2) * (v000050 * (v000046 * v005160 + v005145) + v000055 * v005034);
    const Scalar v003587 = v000008 + v000055 * v004435 + static_cast<Scalar>(2) * (v000009 * v005030 + v000050 * v005031);
    const Scalar v003217 = v003587 * v003587;
    const Scalar v002886 = v003588 * v003588 + v003589 * v003589;
    const Scalar v002885 = static_cast<Scalar>(1) / v003217;
    const Scalar v002380 = v001223 * (v002885 * v002886);
    const Scalar v002195 = v000007 * v002380;
    const Scalar v002020 = v000006 + v002195;
    const Scalar v000876 = v000046 * v005158 + v005136;
    const Scalar v001234 = static_cast<Scalar>(2) * v003588;
    const Scalar v000846 = v000043 * v005158 + v005146;
    const Scalar v000852 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v005035;
    const Scalar v001390 = v001437 * v003589;
    const Scalar v000822 = static_cast<Scalar>(-1) + static_cast<Scalar>(2) * v005032;
    const Scalar v001844 = v002020 + v002195;
    const Scalar v001721 = v001223 * ((static_cast<Scalar>(1) / (v003217 * v003587)) * v002886);
    const Scalar v001364 = v001223 * (static_cast<Scalar>(2) * v001844);
    const Scalar v001628 = static_cast<Scalar>(1) + v002020 * v002380;
    const Scalar v001411 = v001364 * v003589;
    const Scalar v001213 = v001721 * v004435 + (-v003588 * v005034 + v000876 * v003589) * (static_cast<Scalar>(2) * v002885);
    const Scalar v001242 = static_cast<Scalar>(2) * v001628;
    const Scalar v001205 = (static_cast<Scalar>(2) * v005031) * v001721 + (v000852 * v003589 + v000846 * v001234) * v002885;
    const Scalar v001543 = v001628 * v002885;
    const Scalar v001201 = (static_cast<Scalar>(2) * v005030) * v001721 + (v000822 * v003588 + v001390 * v005036) * v002885;
    const Scalar v001410 = v001364 * v003588;
    const Scalar v001367 = v001223 * v001543;
    const Scalar v001473 = static_cast<Scalar>(1) / v003587;
    const Scalar v001409 = v001367 * v005031;
    _residuals_D_point(0, 0) = v000004 * ((v001234 * v001367) * v005030 + (v000822 * v001628 + v001201 * v001410) * v001473);
    _residuals_D_point(0, 1) = v000004 * (v001234 * v001409 + (v000846 * v001242 + v001205 * v001410) * v001473);
    _residuals_D_point(0, 2) = (((v001628 * v003588) * -v002885) * v004435 + (v001242 * v005034 + v001213 * (v001437 * (v001844 * v003588))) * v001473) * v001438;
    _residuals_D_point(1, 0) = ((v001543 * v005030) * v001390 + (v001242 * v005036 + v001201 * (v001437 * (v001844 * v003589))) * v001473) * v001438;
    _residuals_D_point(1, 1) = v000004 * ((static_cast<Scalar>(2) * v003589) * v001409 + (v000852 * v001628 + v001205 * v001411) * v001473);
    _residuals_D_point(1, 2) = v000004 * (v001367 * v003589 * v004435 + (v000876 * v001242 + v001213 * v001411) * v001473);
  }
  const Scalar v000097 = _measured_xy(1, 0);
  const Scalar v000001 = _measured_xy(0, 0);
  _residuals(0, 0) = -(v000001 + v001240 * v001244);
  _residuals(1, 0) = -(v000097 + v001240 * v001245);
}

} // namespace gen