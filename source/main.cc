#include <iostream>
#include <memory>

#include <fmt/ostream.h>

#include "expressions.h"


using namespace math;

int main() {
  const auto x = MakeVar("x");
  const auto y = MakeVar("y");
  // const auto w = MakeNum(3);
  // const auto z = x * y * w;
  // const auto f = x * x * x;

  fmt::print("{}\n", x * y * x);
  fmt::print("{}\n", (x * y * x).Diff(y, 1));

  // fmt::print("{}\n", );

  // std::cout << z.ToString() << std::endl;
  // std::cout << z.Diff(x).ToString() << std::endl;
  // std::cout << z.Diff(y).ToString() << std::endl;
  // std::cout << f.Diff(x).ToString() << std::endl;

  return 0;
}
