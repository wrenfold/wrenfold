// wrenfold symbolic code generator.
// Copyright (c) 2026 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "type_casters.h"

#include <nanobind/stl/complex.h>

#include "numpy_conversion.h"

#include "wf/expressions/matrix.h"
#include "wf/numerical_casts.h"
#include "wf/utility/overloaded_visit.h"

namespace wf {
namespace py = nanobind;

namespace {

using numerical_variant = std::variant<std::int64_t, double, std::complex<double>>;

std::vector<numerical_variant> numerical_values_from_matrix(const matrix_expr& self) {
  std::vector<numerical_variant> numerical_values;
  numerical_values.reserve(self.size());
  for (const scalar_expr& expr : self.as_matrix()) {
    if (const auto maybe_num = numerical_cast(expr); maybe_num.has_value()) {
      numerical_values.push_back(*maybe_num);
    } else {
      throw wf::type_error(
          "Expression of type `{}` contains symbolic components that cannot be converted to "
          "numerical types.",
          expr.type_name());
    }
  }
  return numerical_values;
}

}  // namespace

numerical_array_variant numpy_from_matrix(const matrix_expr& self, const py::kwargs&) {
  const auto numerical_values = numerical_values_from_matrix(self);

  // In order to create a single array type, we promote:
  //  int64 -> double -> std::complex<double>
  const bool contains_complex = std::ranges::any_of(numerical_values, [](const auto& x) {
    return std::holds_alternative<std::complex<double>>(x);
  });
  const bool contains_double = std::ranges::any_of(
      numerical_values, [](const auto& x) { return std::holds_alternative<double>(x); });

  // Transform all elements of the matrix with the provided caster,
  // then convert to an Eigen type so that nanobind can return a numpy array for us.
  const auto convert_to_matrix = [&numerical_values, &self](const auto& caster) {
    const auto converted = transform_map<std::vector>(
        numerical_values, [&caster](const auto& x) { return std::visit(caster, x); });

    using value_type = std::decay_t<decltype(converted)>::value_type;
    return Eigen::Map<
               const Eigen::Matrix<value_type, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>(
               converted.data(), static_cast<Eigen::Index>(self.rows()),
               static_cast<Eigen::Index>(self.cols()))
        .eval();
  };

  if (contains_complex) {
    return convert_to_matrix(make_overloaded(
        [](std::int64_t v) { return static_cast<std::complex<double>>(static_cast<double>(v)); },
        [](double v) { return static_cast<std::complex<double>>(v); },
        [](std::complex<double> v) { return v; }));
  } else if (contains_double) {
    return convert_to_matrix(make_overloaded(
        [](std::int64_t v) { return static_cast<double>(v); }, [](double v) { return v; },
        [](std::complex<double>) -> double {
          throw wf::type_error("Cannot coerce std::complex<double> to double.");
        }));
  } else {
    return convert_to_matrix(make_overloaded(
        [](std::int64_t v) { return v; },
        [](auto arg) -> std::int64_t {
          throw wf::type_error("Cannot coerce `{}` to std::int64_t.", typeid(arg).name());
        }));
  }
}

py::object numpy_from_matrix_with_dtype(const matrix_expr& self, const py::object& dtype) {
  const auto numerical_values = numerical_values_from_matrix(self);
  const py::module_ numpy = py::module_::import_("numpy");
  if (self.rows() == 0) {
    // An empty row list cannot convey the column count.
    return numpy.attr("empty")(py::make_tuple(self.rows(), self.cols()), py::arg("dtype") = dtype);
  }

  // Keep each element's numeric type until NumPy applies the requested dtype.
  py::list rows;
  std::size_t element_index = 0;
  for (index_t row = 0; row < self.rows(); ++row) {
    py::list values;
    for (index_t col = 0; col < self.cols(); ++col) {
      std::visit([&](const auto& value) { values.append(py::cast(value)); },
                 numerical_values[element_index++]);
    }
    rows.append(values);
  }
  return numpy.attr("array")(rows, py::arg("dtype") = dtype);
}

}  // namespace wf
