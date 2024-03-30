// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/rust_code_generator.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Accept the mathematical function description, and "transpile" it into AST that can be emitted
// in another language.
ast::function_definition transpile_to_function_definition(const function_description& description) {
  const control_flow_graph cfg =
      control_flow_graph{description.output_expressions()}.convert_conditionals_to_control_flow();
  return ast::create_ast(cfg, description);
}

// Construct `external_function`. We define this custom constructor to convert py::object to
// type_variant (which is not default constructible).
external_function init_external_function(
    std::string name, const std::vector<std::tuple<std::string_view, py::object>>& arguments,
    const py::object& return_type) {
  auto args = transform_enumerate_map<std::vector<argument>>(
      arguments,
      [](const std::size_t index, const std::tuple<std::string_view, py::object>& name_and_type) {
        return argument(std::get<0>(name_and_type),
                        variant_from_pyobject<type_variant>(std::get<1>(name_and_type)),
                        argument_direction::input, index);
      });
  return external_function(std::move(name), std::move(args),
                           variant_from_pyobject<type_variant>(return_type));
}

// Create expressions that represent the result of invoking an external function.
any_expression call_external_function(const external_function& self, const py::list& args) {
  // Get expressions out of args:
  auto captured_args =
      transform_map<std::vector<any_expression>>(args, &variant_from_pyobject<any_expression>);
  // Now we need to check the types and create expression result:
  return self.create_invocation(std::move(captured_args));
}

void wrap_argument(py::module_& m) {
  py::enum_<argument_direction>(m, "ArgumentDirection")
      .value("Input", argument_direction::input)
      .value("Output", argument_direction::output)
      .value("OptionalOutput", argument_direction::optional_output);

  py::class_<argument>(m, "Argument")
      .def_property_readonly("name", &argument::name)
      .def_property_readonly("type", &argument::type)
      .def_property_readonly("direction", &argument::direction)
      .def_property_readonly("is_optional", &argument::is_optional)
      .def("__repr__", [](const argument& self) {
        return fmt::format("Argument({}: {})", self.name(), self.type());
      });
}

void wrap_codegen_operations(py::module_& m) {
  m.def(
      "transpile",
      [](const std::vector<function_description>& descriptions) {
        // TODO: Allow this to run in parallel.
        return transform_map<std::vector>(descriptions, &transpile_to_function_definition);
      },
      py::arg("descriptions"),
      py::doc("Generate function definitions in AST form, given symbolic function descriptions."),
      py::return_value_policy::take_ownership);

  m.def("transpile", &transpile_to_function_definition, py::arg("description"),
        py::doc(
            "Generate a function definition in AST form, given the symbolic function description."),
        py::return_value_policy::take_ownership);

  wrap_class<external_function>(m, "ExternalFunction")
      .def(py::init(&init_external_function), py::arg("name"), py::arg("arguments"),
           py::arg("return_type"))
      .def_property_readonly("name", &external_function::name)
      .def_property_readonly("arguments", &external_function::arguments)
      .def_property_readonly("num_arguments", &external_function::num_arguments)
      .def("arg_position", &external_function::arg_position, py::arg("arg"),
           py::doc("Find the position of the argument with the specified name."))
      .def_property_readonly("return_type", &external_function::return_type)
      .def("__call__", &call_external_function, py::arg("args"),
           py::doc("Call external function and create return expression."))
      .def("__repr__", [](const external_function& self) {
        const auto args = transform_map<std::vector<std::string>>(
            self.arguments(),
            [](const argument& arg) { return fmt::format("{}: {}", arg.name(), arg.type()); });
        return fmt::format("{}({}) -> {}", self.name(), fmt::join(args, ", "), self.return_type());
      });

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
             const scalar_expr& value) {
            self.add_output_argument(name, scalar_type(code_numeric_type::floating_point),
                                     is_optional, {value});
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const matrix_expr& value) {
            self.add_output_argument(name, matrix_type(value.rows(), value.cols()), is_optional,
                                     value.to_vector());
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"),
          py::doc("Record an output argument of matrix type."))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const custom_type& custom_type, std::vector<scalar_expr> expressions) {
            self.add_output_argument(name, custom_type, is_optional, std::move(expressions));
          },
          py::arg("name"), py::arg("is_optional"), py::arg("custom_type"), py::arg("expressions"),
          py::doc("Record an output argument of custom type."))
      .def(
          "set_return_value",
          [](function_description& self, const scalar_expr& value) {
            self.set_return_value(scalar_type(code_numeric_type::floating_point), {value});
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const matrix_expr& value) {
            self.set_return_value(matrix_type(value.rows(), value.cols()), value.to_vector());
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const custom_type& custom_type,
             std::vector<scalar_expr> expressions) {
            self.set_return_value(custom_type, std::move(expressions));
          },
          py::arg("custom_type"), py::arg("expressions"));
}

}  // namespace wf
