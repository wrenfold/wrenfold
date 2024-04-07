// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/algorithm_utils.h"
#include "wf/code_generation/types.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Implement the abstract `erased_pytype::concept` interface.
// We use this to type-erase a `py::type`, and pass it into `custom_type`.
class pytype_wrapper final : public erased_pytype::concept {
 public:
  explicit pytype_wrapper(py::type type) noexcept(std::is_nothrow_move_constructible_v<py::type>)
      : type_(std::move(type)) {}

  bool is_identical_to(const erased_pytype::concept& other) const override {
    // Cast is safe because there is only one implementation of `erased_pytype`.
    return type_.is(static_cast<const pytype_wrapper&>(other).type_);
  }

  std::size_t hash() const override { return py::hash(type_); }

  constexpr const py::type& type() const noexcept { return type_; }

 private:
  py::type type_;
};

// Define python constructor for `custom_type`.
custom_type init_custom_type(std::string name,
                             const std::vector<std::tuple<std::string_view, py::object>>& fields,
                             py::type python_type) {
  auto fields_converted = transform_map<std::vector>(fields, [](const auto& tup) {
    // We can't use a variant in the tuple, since it can't be default constructed.
    // Instead, we check for different types manually here.
    const auto& [field_name, type_obj] = tup;
    return struct_field(std::string{field_name}, variant_from_pyobject<type_variant>(type_obj));
  });
  return custom_type(std::move(name), std::move(fields_converted),
                     erased_pytype(std::in_place_type_t<pytype_wrapper>{}, std::move(python_type)));
}

void wrap_types(py::module_& m) {
  py::enum_<code_numeric_type>(m, "NumericType")
      .value("Bool", code_numeric_type::boolean, "Boolean value.")
      .value("Integer", code_numeric_type::integral, "Signed integral value.")
      .value("Float", code_numeric_type::floating_point, "Floating-point value.");

  wrap_class<scalar_type>(m, "ScalarType")
      .def(py::init<code_numeric_type>(), py::arg("numeric_type"),
           "Construct with ``NumericType``.")
      .def_property_readonly("numeric_type", &scalar_type::numeric_type,
                             "Access underlying ``NumericType`` enum.")
      .def("__repr__", [](scalar_type self) { return fmt::format("{}", self); })
      .doc() = "A scalar-valued type.";

  wrap_class<matrix_type>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"),
           "Construct with number of rows and columns.")
      .def_property_readonly("num_rows", &matrix_type::rows, "First dimension of the matrix.")
      .def_property_readonly("num_cols", &matrix_type::cols, "Second dimension of the matrix.")
      .def("compute_indices", &matrix_type::compute_indices, "idx"_a,
           "Given a flat index, compute the (row, column) pair it corresponds to.")
      .def("__repr__", [](matrix_type self) { return fmt::format("{}", self); })
      .doc() = "A 2D matrix-valued type.";

  wrap_class<struct_field>(m, "StructField")
      .def_property_readonly("name", &struct_field::name, "Name of the field.")
      .def_property_readonly("type", &struct_field::type, "Type of the field.")
      .doc() =
      "Describes a field on a struct. The :class:`wrenfold.type_info.CustomType` contains a list "
      "of fields.";

  wrap_class<custom_type>(m, "CustomType")
      .def(py::init(&init_custom_type), py::arg("name"), py::arg("fields"), py::arg("python_type"),
           "Construct custom type.")
      .def_property_readonly("name", &custom_type::name, "Name of the struct.")
      .def_property_readonly("fields", &custom_type::fields,
                             "A list of :class:`wrenfold.type_info.StructField` objects.")
      .def_property_readonly(
          "total_size", &custom_type::total_size,
          "Total number of scalar expressions in the custom type **and** all of its children.")
      .def_property_readonly(
          "python_type",
          [](const custom_type& self) -> std::variant<py::none, py::type> {
            if (const auto pytype = self.underying_pytype(); pytype.has_value()) {
              return pytype->as<pytype_wrapper>().type();
            }
            return py::none();
          },
          "Retrieve the underlying user-declared python type.")
      .def("__repr__",
           [](const custom_type& self) {
             py::object python_type = py::none();
             if (const auto pytype = self.underying_pytype(); pytype.has_value()) {
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
