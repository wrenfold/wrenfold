// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/ir_builder.h"
#include "wf/code_generation/rust_code_generator.h"
#include "wf/code_generation/types.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

namespace py = pybind11;
using namespace py::literals;

// We make this type opaque and wrap it manually below.
// This allows us to avoid problems from variant not being default constructible.
PYBIND11_MAKE_OPAQUE(wf::ast::variant_vector)

namespace wf {

// Accept the mathematical function description, and "transpile" it into AST that can be emitted
// in another language.
ast::function_definition transpile_to_function_definition(const function_description& description) {
  flat_ir ir{description.output_expressions()};
  ir.eliminate_duplicates();
  const output_ir output_ir{std::move(ir)};
  return ast::create_ast(output_ir, description);
}

// Implement the abstract `erased_pytype::concept` interface.
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

// Define pythong constructof for `custom_type`.
custom_type init_custom_type(std::string name,
                             const std::vector<std::tuple<std::string_view, py::object>>& fields,
                             py::type python_type) {
  std::vector<struct_field> fields_converted{};
  fields_converted.reserve(fields.size());
  std::transform(fields.begin(), fields.end(), std::back_inserter(fields_converted),
                 [](const auto& tup) {
                   // We can't use a variant in the tuple, since it can't be default constructed.
                   // Instead, we check for different types manually here.
                   const auto& [field_name, type_obj] = tup;
                   if (py::isinstance<scalar_type>(type_obj)) {
                     return struct_field(std::string{field_name}, py::cast<scalar_type>(type_obj));
                   } else if (py::isinstance<matrix_type>(type_obj)) {
                     return struct_field(std::string{field_name}, py::cast<matrix_type>(type_obj));
                   } else if (py::isinstance<custom_type>(type_obj)) {
                     return struct_field(std::string{field_name}, py::cast<custom_type>(type_obj));
                   } else {
                     throw type_error("Field type must be ScalarType, MatrixType, or CustomType.");
                   }
                 });

  return custom_type(std::move(name), std::move(fields_converted),
                     erased_pytype(std::in_place_type_t<pytype_wrapper>{}, std::move(python_type)));
}

// Construct class_ wrapper for an AST type. Name is derived automatically.
template <typename T>
auto wrap_ast_type(py::module_& m) {
  return py::class_<T>(m, ast::camel_case_name<T>())
      .def("__repr__", [](const T& obj) -> std::string {
        // Handled by the formatters in ast_formatters.h
        static_assert(ast::is_formattable<T>::value,
                      "The specified type is missing a format_ast(...) method");
        return fmt::format("{}", obj);
      });
}

void wrap_codegen_operations(py::module_& m) {
  // Variant vector is wrapped as an opaque type so we don't have to deal
  // with the variant not being default constructible.
  py::class_<ast::variant_vector>(m, "AstVector")
      .def("__repr__",
           [](const ast::variant_vector& vec) {
             return fmt::format("AstVector({} elements)", vec.size());
           })
      .def("__len__", [](const ast::variant_vector& vec) { return vec.size(); })
      .def(
          "__iter__",
          [](const std::vector<ast::variant>& vec) {
            return py::make_iterator(vec.begin(), vec.end());
          },
          py::keep_alive<0, 1>())
      .def(
          "__getitem__",
          [](const std::vector<ast::variant>& vec, const std::size_t index) {
            if (index >= vec.size()) {
              throw dimension_error("Index `{}` exceeds vector length `{}`.", index, vec.size());
            }
            return vec[index];
          },
          py::arg("index"), py::doc("Array access operator."));

  m.def(
      "transpile",
      [](const std::vector<function_description>& descriptions) {
        // TODO: Allow this to run in parallel.
        std::vector<ast::function_definition> outputs;
        outputs.reserve(descriptions.size());
        std::transform(descriptions.begin(), descriptions.end(), std::back_inserter(outputs),
                       &transpile_to_function_definition);
        return outputs;
      },
      py::arg("descriptions"),
      py::doc("Generate function definitions in AST form, given symbolic function descriptions."),
      py::return_value_policy::take_ownership);

  m.def("transpile", &transpile_to_function_definition, py::arg("description"),
        py::doc(
            "Generate a function definition in AST form, given the symbolic function description."),
        py::return_value_policy::take_ownership);

  py::enum_<std_math_function>(m, "StdMathFunction")
      .value("Cos", std_math_function::cos)
      .value("Sin", std_math_function::sin)
      .value("Tan", std_math_function::tan)
      .value("ArcCos", std_math_function::acos)
      .value("ArcSin", std_math_function::asin)
      .value("ArcTan", std_math_function::atan)
      .value("Log", std_math_function::log)
      .value("Sqrt", std_math_function::sqrt)
      .value("Abs", std_math_function::abs)
      .value("Signum", std_math_function::signum)
      .value("Arctan2", std_math_function::atan2)
      .value("Powi", std_math_function::powi)
      .value("Powf", std_math_function::powf)
      .def(
          "to_string",
          [](std_math_function name) { return string_from_standard_library_function(name); },
          py::doc("Convert to string."));

  py::enum_<code_numeric_type>(m, "NumericType")
      .value("Bool", code_numeric_type::boolean)
      .value("Integer", code_numeric_type::integral)
      .value("Real", code_numeric_type::floating_point);

  py::enum_<relational_operation>(m, "RelationalOperation")
      .value("LessThan", relational_operation::less_than)
      .value("LessThanOrEqual", relational_operation::less_than_or_equal)
      .value("Equal", relational_operation::equal);

  py::enum_<argument_direction>(m, "ArgumentDirection")
      .value("Input", argument_direction::input)
      .value("Output", argument_direction::output)
      .value("OptionalOutput", argument_direction::optional_output);

  py::class_<scalar_type>(m, "ScalarType")
      .def(py::init<code_numeric_type>())
      .def_property_readonly("numeric_type", &scalar_type::numeric_type)
      .def("__repr__", [](scalar_type self) { return fmt::format("{}", self); });

  py::class_<matrix_type>(m, "MatrixType")
      .def(py::init<index_t, index_t>(), py::arg("rows"), py::arg("cols"))
      .def_property_readonly("num_rows", &matrix_type::rows)
      .def_property_readonly("num_cols", &matrix_type::cols)
      .def("compute_indices", &matrix_type::compute_indices)
      .def("__repr__", [](matrix_type self) { return fmt::format("{}", self); });

  py::class_<custom_type>(m, "CustomType")
      .def(py::init(&init_custom_type), py::arg("name"), py::arg("fields"), py::arg("python_type"),
           py::doc("Construct custom type from fields."))
      .def_property_readonly("name", &custom_type::name)
      .def_property_readonly("fields", &custom_type::fields, py::doc("Access fields on this type."))
      .def_property_readonly(
          "python_type",
          [](const custom_type& self) -> std::variant<py::none, py::type> {
            if (const auto pytype = self.underying_pytype(); pytype.has_value()) {
              return pytype->as<pytype_wrapper>().type();
            }
            return py::none();
          },
          py::doc("Get the underlying python type."))
      .def("__repr__", [](const custom_type& self) {
        py::object python_type = py::none();
        if (const auto pytype = self.underying_pytype(); pytype.has_value()) {
          python_type = pytype->as<pytype_wrapper>().type();
        }
        const py::str repr = py::repr(python_type);
        return fmt::format("CustomType('{}', {} fields, {})", self.name(), self.size(),
                           py::cast<std::string_view>(repr));
      });

  py::class_<custom_function>(m, "CustomFunction")
      .def_property_readonly("name", &custom_function::name)
      .def_property_readonly("arguments", &custom_function::arguments)
      .def_property_readonly("num_arguments", &custom_function::num_arguments)
      .def_property_readonly("return_type", &custom_function::return_type)
      .def("__eq__", &are_identical<custom_function>, py::is_operator());

  py::class_<function_description>(m, "FunctionDescription")
      .def(py::init<std::string>(), py::arg("name"), py::doc("Construct with string name."))
      .def_property_readonly("name", &function_description::name)
      .def("__repr__",
           [](const function_description& self) {
             return fmt::format("FunctionDescription('{}', {} args)", self.name(),
                                self.arguments().size());
           })
      .def(
          "add_input_argument",
          [](function_description& self, const std::string_view name, scalar_type type) {
            return self.add_input_argument(name, type);
          },
          py::arg("name"), py::arg("type"),
          py::doc("Add a scalar input argument. Returns placeholder value to pass to the python "
                  "function."))
      .def(
          "add_input_argument",
          [](function_description& self, const std::string_view name, matrix_type type) {
            return self.add_input_argument(name, type);
          },
          py::arg("name"), py::arg("type"),
          py::doc("Add a matrix input argument. Returns placeholder value to pass to the python "
                  "function."))
      .def(
          "add_input_argument",
          [](function_description& self, const std::string_view name, const custom_type& type) {
            return self.add_input_argument(name, type);
          },
          py::arg("name"), py::arg("type"),
          py::doc("Add an input argument with a custom user-specified type."))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const Expr& value) {
            self.add_output_argument(name, scalar_type(code_numeric_type::floating_point),
                                     is_optional, {value});
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const MatrixExpr& value) {
            self.add_output_argument(name, matrix_type(value.rows(), value.cols()), is_optional,
                                     value.to_vector());
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const custom_type& custom_type, std::vector<Expr> expressions) {
            self.add_output_argument(name, custom_type, is_optional, std::move(expressions));
          },
          py::arg("name"), py::arg("is_optional"), py::arg("custom_type"), py::arg("expressions"),
          py::doc("Record an output argument of custom type."))
      .def(
          "set_return_value",
          [](function_description& self, const Expr& value) {
            self.set_return_value(scalar_type(code_numeric_type::floating_point), {value});
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const MatrixExpr& value) {
            self.set_return_value(matrix_type(value.rows(), value.cols()), value.to_vector());
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const custom_type& custom_type,
             std::vector<Expr> expressions) {
            self.set_return_value(custom_type, std::move(expressions));
          },
          py::arg("custom_type"), py::arg("expressions"));

  py::class_<argument>(m, "Argument")
      .def_property_readonly("name", &argument::name)
      .def_property_readonly("type", &argument::type)
      .def_property_readonly("direction", &argument::direction)
      .def_property_readonly("is_optional", &argument::is_optional)
      .def("__repr__", [](const argument& self) {
        return fmt::format("Argument({}: {})", self.name(), self.type());
      });

  // AST types are below:
  // --------------------

  wrap_ast_type<ast::add>(m)
      .def_property_readonly("left", [](const ast::add& x) { return *x.left; })
      .def_property_readonly("right", [](const ast::add& x) { return *x.right; });

  wrap_ast_type<ast::assign_temporary>(m)
      .def_property_readonly("left", [](const ast::assign_temporary& x) { return x.left; })
      .def_property_readonly("right", [](const ast::assign_temporary& x) {
        WF_ASSERT(x.right);
        return *x.right;
      });

  wrap_ast_type<ast::assign_output_matrix>(m)
      .def_property_readonly("arg", [](const ast::assign_output_matrix& x) { return x.arg; })
      .def_property_readonly("value", [](const ast::assign_output_matrix& x) { return *x.value; });

  wrap_ast_type<ast::assign_output_scalar>(m)
      .def_property_readonly("arg", [](const ast::assign_output_scalar& x) { return x.arg; })
      .def_property_readonly("value", [](const ast::assign_output_scalar& x) { return *x.value; });

  wrap_ast_type<ast::assign_output_struct>(m)
      .def_property_readonly("arg", [](const ast::assign_output_struct& x) { return x.arg; })
      .def_property_readonly("value", [](const ast::assign_output_struct& x) { return *x.value; });

  wrap_ast_type<ast::branch>(m)
      .def_property_readonly("condition", [](const ast::branch& c) { return *c.condition; })
      .def_property_readonly("if_branch", [](const ast::branch& c) { return c.if_branch; })
      .def_property_readonly("else_branch", [](const ast::branch& c) { return c.else_branch; });

  wrap_ast_type<ast::call_custom_function>(m)
      .def_property_readonly("function",
                             [](const ast::call_custom_function& c) { return c.function; })
      .def_property_readonly("args", [](const ast::call_custom_function& c) { return c.args; });

  wrap_ast_type<ast::call_std_function>(m)
      .def_property_readonly("function", [](const ast::call_std_function& c) { return c.function; })
      .def_property_readonly("args", [](const ast::call_std_function& c) { return c.args; });

  wrap_ast_type<ast::cast>(m)
      .def_property_readonly("destination_type",
                             [](const ast::cast& c) { return c.destination_type; })
      .def_property_readonly("arg", [](const ast::cast& c) {
        WF_ASSERT(c.arg);
        return *c.arg;
      });

  wrap_ast_type<ast::comment>(m)
      .def_property_readonly("content", [](const ast::comment& c) { return c.content; })
      .def("split_lines", &ast::comment::split_lines,
           py::doc("Split comment by newlines and return a list of strings, one per line."));

  wrap_ast_type<ast::compare>(m)
      .def_property_readonly("left",
                             [](const ast::compare& c) {
                               WF_ASSERT(c.left);
                               return *c.left;
                             })
      .def_property_readonly("right",
                             [](const ast::compare& c) {
                               WF_ASSERT(c.right);
                               return *c.right;
                             })
      .def_property_readonly("operation", [](const ast::compare& c) { return c.operation; });

  wrap_ast_type<ast::construct_matrix>(m)
      .def_property_readonly("type", [](const ast::construct_matrix& c) { return c.type; })
      .def_property_readonly("args", [](const ast::construct_matrix& c) { return c.args; });

  wrap_ast_type<ast::construct_custom_type>(m)
      .def_property_readonly("type",
                             [](const ast::construct_custom_type& self) { return self.type; })
      .def_property_readonly(
          "field_values",
          [](const ast::construct_custom_type& self) -> const auto& { return self.field_values; })
      .def(
          "get_field_value",
          [](const ast::construct_custom_type& self,
             const std::string_view name) -> std::optional<ast::variant> {
            if (const auto v = self.get_field_by_name(name); v.has_value()) {
              return *v;
            }
            return std::nullopt;
          },
          py::arg("name"), py::doc("Lookup field value by name"));

  wrap_ast_type<ast::declaration>(m)
      .def_property_readonly("name", [](const ast::declaration& d) { return d.name; })
      .def_property_readonly("type", [](const ast::declaration& d) { return d.type; })
      .def_property_readonly("value", [](const ast::declaration& d) -> std::optional<ast::variant> {
        if (d.value) {
          return *d.value;
        } else {
          return std::nullopt;
        }
      });

  wrap_ast_type<ast::divide>(m)
      .def_property_readonly("left",
                             [](const ast::divide& x) {
                               WF_ASSERT(x.left);
                               return *x.left;
                             })
      .def_property_readonly("right", [](const ast::divide& x) {
        WF_ASSERT(x.right);
        return *x.right;
      });

  wrap_ast_type<ast::float_literal>(m).def_property_readonly(
      "value", [](const ast::float_literal& f) { return f.value; });

  wrap_ast_type<ast::get_argument>(m).def_property_readonly(
      "argument", [](const ast::get_argument& self) { return self.arg; });

  wrap_ast_type<ast::get_field>(m)
      .def_property_readonly("arg", [](const ast::get_field& self) { return *self.arg; })
      .def_property_readonly("struct_type", [](const ast::get_field& self) { return self.type; })
      .def_property_readonly("field_name", [](const ast::get_field& self) { return self.field; });

  wrap_ast_type<ast::get_matrix_element>(m)
      .def_property_readonly("arg", [](const ast::get_matrix_element& self) { return *self.arg; })
      .def_property_readonly("row", [](const ast::get_matrix_element& self) { return self.row; })
      .def_property_readonly("col", [](const ast::get_matrix_element& self) { return self.col; });

  wrap_ast_type<ast::integer_literal>(m).def_property_readonly(
      "value", [](const ast::integer_literal& i) { return i.value; });

  wrap_ast_type<ast::multiply>(m)
      .def_property_readonly("left",
                             [](const ast::multiply& x) {
                               WF_ASSERT(x.left);
                               return *x.left;
                             })
      .def_property_readonly("right", [](const ast::multiply& x) {
        WF_ASSERT(x.right);
        return *x.right;
      });

  wrap_ast_type<ast::negate>(m).def_property_readonly("arg", [](const ast::negate& x) {
    WF_ASSERT(x.arg);
    return *x.arg;
  });

  wrap_ast_type<ast::optional_output_branch>(m)
      .def_property_readonly("argument",
                             [](const ast::optional_output_branch& self) { return self.arg; })
      .def_property_readonly(
          "statements", [](const ast::optional_output_branch& self) { return self.statements; });

  wrap_ast_type<ast::return_object>(m).def_property_readonly(
      "value", [](const ast::return_object& c) { return c.value; },
      py::doc("Value or object being returned."));

  wrap_ast_type<ast::special_constant>(m).def_property_readonly(
      "value", [](const ast::special_constant& c) { return c.value; },
      py::doc("Enum indicating the value of the constant"));

  wrap_ast_type<ast::variable_ref>(m).def_property_readonly(
      "name", [](const ast::variable_ref& v) { return v.name; });

  // Types that are not part of the ast::variant
  wrap_ast_type<ast::function_definition>(m)
      .def_property_readonly("signature", &ast::function_definition::signature)
      .def_property_readonly("body",  // TODO: Don't copy this on return.
                             &ast::function_definition::body);

  wrap_ast_type<ast::function_signature>(m)
      .def_property_readonly("return_annotation", &ast::function_signature::return_annotation)
      .def_property_readonly("name", &ast::function_signature::name)
      .def_property_readonly("arguments", &ast::function_signature::arguments);

  wrap_ast_type<ast::return_type_annotation>(m).def_property_readonly(
      "type", [](const ast::return_type_annotation& ret) { return ret.type; });
}

}  // namespace wf
