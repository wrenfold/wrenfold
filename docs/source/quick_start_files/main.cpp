#include <iostream>

// Defining `WF_SPAN_EIGEN_SUPPORT` will enable automatic conversion of Eigen types to
// wrenfold spans.
#define WF_SPAN_EIGEN_SUPPORT
#include <wrenfold/span.h>

// We assume our generated function was saved in `rosenbrock.h`:
#include "rosenbrock.h"

int main() {
  constexpr double a = 2.0;
  constexpr double b = 10.0;

  // The global minimum is (a, a^2):
  const Eigen::Vector2d xy_minimum{a, a * a};

  // Note that we can pass RowVector2d directly for the `f_D_xy` argument:
  Eigen::RowVector2d f_D_xy;
  const double f = gen::rosenbrock(xy_minimum, a, b, f_D_xy);

  // A crude test: at the minimum f should be zero, and f_D_xy = [0, 0]
  std::cout << "f = " << f << "\n";
  std::cout << "f_D_xy = [" << f_D_xy.x() << ", " << f_D_xy.y() << "]" << std::endl;
  return 0;
}
