// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/string_view.h>
#include <nanobind/stl/tuple.h>
#include <nanobind/stl/vector.h>
#include <nanobind/typing.h>

#include "wf/code_generation/types.h"
#include "wf/utility/algorithms.h"

#include "wrapper_utils.h"

namespace py = nanobind;
using namespace py::literals;

namespace wf {

// Implement the abstract `erased_pytype::concept` interface.
// We use this to type-erase a `py::type`, and pass it into `custom_type`.
class pytype_wrapper final : public erased_pytype::concept_base {
 public:
  explicit pytype_wrapper(py::type_object type) noexcept : type_(std::move(type)) {}

  bool is_identical_to(const erased_pytype::concept_base& other) const override {
    // Cast is safe because there is only one implementation of `erased_pytype`.
    return type_.equal(static_cast<const pytype_wrapper&>(other).type_);
  }

  constexpr const py::type_object& type() const noexcept { return type_; }

 private:
  py::type_object type_;
};

// Define python constructor for `custom_type`.
void init_custom_type(custom_type* self, std::string name,
                      const std::vector<std::tuple<std::string_view, py::object>>& fields,
                      py::type_object python_type) {
  auto fields_converted = transform_map<std::vector>(fields, [](const auto& tup) {
    // We can't use a variant in the tuple, since it can't be default constructed.
    // Instead, we check for different types manually here.
    const auto& [field_name, type_obj] = tup;
    return struct_field(std::string{field_name}, variant_from_pyobject<type_variant>(type_obj));
  });
  new (self)
      custom_type(std::move(name), std::move(fields_converted),
                  erased_pytype(std::in_place_type_t<pytype_wrapper>{}, std::move(python_type)));
}

void wrap_types(py::module_& m) {
  py::enum_<numeric_primitive_type>(m, "NumericType")
      .value("Bool", numeric_primitive_type::boolean, "Boolean value.")
      .value("Integer", numeric_primitive_type::integral, "Signed integral value.")
      .value("Float", numeric_primitive_type::floating_point, "Floating-point value.");

  wrap_class<scalar_type>(m, "ScalarType")
      .def(py::init<numeric_primitive_type>(), py::arg("numeric_type"),
           "Construct with ``NumericType``.")
      .def_prop_ro("numeric_type", &scalar_type::numeric_type,
                   "Access underlying ``NumericType`` enum.")
      .def("__repr__", [](scalar_type self) { return fmt::format("{}", self); })
      .doc() = "A scalar-valued type.";

  wrap_class<matrix_type>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"),
           "Construct with number of rows and columns.")
      .def_prop_ro("rows", &matrix_type::rows, "First dimension of the matrix.")
      .def_prop_ro("cols", &matrix_type::cols, "Second dimension of the matrix.")
      .def_prop_ro("shape", &matrix_type::dimensions, "Shape as a tuple of ``(rows, cols)``.")
      .def("compute_indices", &matrix_type::compute_indices, "idx"_a,
           "Given a flat index, compute the (row, column) pair it corresponds to.")
      .def("__repr__", [](matrix_type self) { return fmt::format("{}", self); })
      .doc() = "A 2D matrix-valued type.";

  wrap_class<struct_field>(m, "StructField")
      .def_prop_ro("name", &struct_field::name, "Name of the field.")
      .def_prop_ro("type", &struct_field::type, "Type of the field.")
      .doc() =
      "Describes a field on a struct. The :class:`wrenfold.type_info.CustomType` contains a list "
      "of fields.";

  wrap_class<custom_type>(m, "CustomType")
      .def("__init__", &init_custom_type, py::arg("name"), py::arg("fields"),
           py::arg("python_type"), "Construct custom type.")
      .def_prop_ro("name", &custom_type::name, "Name of the struct.")
      .def_prop_ro("fields", &custom_type::fields,
                   "A list of :class:`wrenfold.type_info.StructField` objects.")
      .def_prop_ro(
          "total_size", &custom_type::total_size,
          "Total number of scalar expressions in the custom type **and** all of its children.")
      .def_prop_ro(
          "python_type",
          [](const custom_type& self) -> std::optional<py::type_object> {
            if (const auto pytype = self.underlying_pytype(); pytype.has_value()) {
              return pytype->as<pytype_wrapper>().type();
            }
            return std::nullopt;
          },
          "Retrieve the underlying user-declared python type. May be None.")
      .def("__repr__",
           [](const custom_type& self) {
             py::object python_type = py::none();
             if (const auto pytype = self.underlying_pytype(); pytype.has_value()) {
               python_type = pytype->as<pytype_wrapper>().type();
             }
             const py::str repr = py::repr(python_type);
             return fmt::format("CustomType('{}', {} fields, {})", self.name(), self.size(),
                                py::cast<std::string_view>(repr));
           })
      .doc() =
      "A custom type describes a user-provided struct that exposes members that wrenfold can "
      "retrieve in generated code.";
}

}  // namespace wf
