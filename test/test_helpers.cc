#include "test_helpers.h"

#include <fmt/format.h>

#include "plain_formatter.h"
#include "tree_formatter.h"

namespace math {

inline std::size_t CountNewlines(const std::string& str) {
  return std::count(str.begin(), str.end(), '\n');
}

testing::AssertionResult FormatFailedResult(const std::string_view description,
                                            const std::string& name_a, const std::string& name_b,
                                            const Expr& a, const Expr& b) {
  // If the formatted string has multiple lines, preface it w/ a line break:
  const std::string a_str = a.ToString();
  const std::string b_str = b.ToString();
  const std::string_view a_prefix = CountNewlines(a_str) > 0 ? "\n" : " ";
  const std::string_view b_prefix = CountNewlines(b_str) > 0 ? "\n" : " ";

  // clang-format off
  return testing::AssertionFailure() << fmt::format(
             "{} {} {}, where:\n{} ={}{}\nand {} ={}{}\n"
             "expression tree for `{}`:\n{}\n"
             "expression tree for `{}`:\n{}",
             name_a, description, name_b,
             name_a, a_prefix, a_str, name_b, b_prefix, b_str,
             name_a, FormatDebugTree(a),
             name_b, FormatDebugTree(b));
  // clang-format on
}

}  // namespace math
