// Machine generated code.
#pragma once
#include <cmath>
#include <cstdint>

#include <wrenfold/span.h>

namespace gen {

template <typename Scalar, typename T0, typename T3>
Scalar rosenbrock(const T0& xy, const Scalar a, const Scalar b, T3&& f_D_xy) {
  auto _xy = wf::make_input_span<2, 1>(xy);
  auto _f_D_xy = wf::make_output_span<1, 2>(f_D_xy);

  // Operation counts:
  // add: 5
  // multiply: 7
  // negate: 2
  // total: 14

  const Scalar v003 = _xy(0, 0);
  const Scalar v041 = -v003;
  const Scalar v001 = _xy(1, 0);
  const Scalar v006 = v001 + v003 * v041;
  const Scalar v000 = b;
  const Scalar v036 = v000 * v006;
  const Scalar v040 = static_cast<Scalar>(2) * v036;
  const Scalar v008 = a;
  const Scalar v010 = v008 + v041;
  _f_D_xy(0, 0) = static_cast<Scalar>(2) * (v003 + -(v008 + v003 * v040));
  _f_D_xy(0, 1) = v040;
  return v006 * v036 + v010 * v010;
}

}  // namespace gen
