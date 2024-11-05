// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast_conversion.h"
#include "wf/code_generation/control_flow_graph.h"
#include "wf/code_generation/expr_from_ir.h"
#include "wf/code_generation/expression_group.h"
#include "wf/expression.h"
#include "wf/matrix_expression.h"

#include "docs/codegen_wrapper.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Accept the mathematical function description, and "transpile" it into AST that can be emitted
// in another language.
ast::function_definition transpile_to_ast(const function_description& description,
                                          const std::optional<optimization_params>& opt_params,
                                          const bool convert_ternaries) {
  const control_flow_graph cfg =
      control_flow_graph{description, opt_params.value_or(optimization_params{})}
          .convert_conditionals_to_control_flow(convert_ternaries);
  return ast::create_ast(cfg, description);
}

// Convert a function description into IR, then rebuild the simplified expression tree and return
// it.
auto cse_function_description(const function_description& description,
                              const std::optional<optimization_params>& opt_params) {
  const control_flow_graph cfg{description, opt_params.value_or(optimization_params{})};
  rebuilt_expressions rebuilt = rebuild_expression_tree(cfg.first_block(), {}, true);
  // Pybind STL conversion will change the types here for us:
  return std::make_tuple(std::move(rebuilt.output_expressions),
                         std::move(rebuilt.intermediate_values));
}

// Construct `external_function`. We define this custom constructor to convert py::object to
// type_variant (which is not default constructible).
external_function init_external_function(
    std::string name, const std::vector<std::tuple<std::string_view, py::object>>& arguments,
    const py::object& return_type) {
  auto args = transform_enumerate_map<std::vector>(
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
  auto captured_args = transform_map<std::vector>(args, &variant_from_pyobject<any_expression>);
  // Now we need to check the types and create expression result:
  return self.create_invocation(std::move(captured_args));
}

void wrap_argument(py::module_& m) {
  py::enum_<argument_direction>(m, "ArgumentDirection")
      .value("Input", argument_direction::input, "Argument is an input.")
      .value("Output", argument_direction::output, "Argument is an output.")
      .value("OptionalOutput", argument_direction::optional_output,
             "Argument is an optional output.");

  py::class_<argument>(m, "Argument")
      .def_property_readonly("name", &argument::name, "String name of the argument.")
      .def_property_readonly("type", &argument::type, "Type of the argument.")
      .def_property_readonly("direction", &argument::direction,
                             "How the argument is used by the function.")
      .def_property_readonly("is_optional", &argument::is_optional,
                             "True if the argument is optional.")
      .def("__repr__",
           [](const argument& self) {
             return fmt::format("Argument({}: {})", self.name(), self.type());
           })
      .doc() = "Describe an argument to a function.";
}

void wrap_codegen_operations(py::module_& m) {
  // We give this a Py prefix since we subclass it in python with another object.
  wrap_class<external_function>(m, "PyExternalFunction")
      .def(py::init(&init_external_function), py::arg("name"), py::arg("arguments"),
           py::arg("return_type"), "Construct with name, arguments, and return type.")
      .def(py::init<external_function>(), "Copy constructor.")
      .def_property_readonly("name", &external_function::name, "Name of the function.")
      .def_property_readonly("arguments", &external_function::arguments, "List of arguments.")
      .def_property_readonly("num_arguments", &external_function::num_arguments,
                             "Number of arguments the function expects to receive.")
      .def("arg_position", &external_function::arg_position, py::arg("arg"),
           "Find the position of the argument with the specified name.")
      .def_property_readonly("return_type", &external_function::return_type,
                             "Return type of the function. This will determine the type of "
                             "variable we must declare in code-generated functions.")
      .def("call", &call_external_function, py::arg("args"),
           "Call external function and create return expression. OMIT_FROM_SPHINX")
      .def("__repr__", [](const external_function& self) {
        const auto args = transform_map<std::vector<std::string>>(
            self.arguments(),
            [](const argument& arg) { return fmt::format("{}: {}", arg.name(), arg.type()); });
        return fmt::format("{}({}) -> {}", self.name(), fmt::join(args, ", "), self.return_type());
      });

  py::enum_<expression_usage>(m, "ExpressionUsage")
      .value("OptionalOutputArgument", expression_usage::optional_output_argument,
             "Value is an optional output argument.")
      .value("OutputArgument", expression_usage::output_argument,
             "Value is a required output argument.")
      .value("ReturnValue", expression_usage::return_value, "Value is the return value.");

  wrap_class<output_key>(m, "OutputKey")
      .def(py::init<expression_usage, std::string_view>(), py::arg("usage"), py::arg("name"))
      .def_property_readonly(
          "usage", [](const output_key& key) { return key.usage; },
          "Describe how the output is returned by the function.")
      .def_property_readonly(
          "name", [](const output_key& key) -> std::string_view { return key.name; },
          "Name of the output value.", py::keep_alive<0, 1>())
      .def("__repr__", [](const output_key& key) {
        if (key.name.empty()) {
          return fmt::format("OutputKey({})", string_from_expression_usage(key.usage));
        } else {
          return fmt::format("OutputKey({}, '{}')", string_from_expression_usage(key.usage),
                             key.name);
        }
      });

  py::class_<function_description>(m, "FunctionDescription")
      .def(py::init<std::string>(), py::arg("name"), "Construct with function name.")
      .def_property_readonly("name", &function_description::name, "Name of the function.")
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
            self.add_output_argument(name, scalar_type(numeric_primitive_type::floating_point),
                                     is_optional, {value});
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const matrix_expr& value) {
            self.add_output_argument(name, matrix_type(value.rows(), value.cols()), is_optional,
                                     value);
          },
          py::arg("name"), py::arg("is_optional"), py::arg("value"),
          py::doc("Record an output argument of matrix type."))
      .def(
          "add_output_argument",
          [](function_description& self, const std::string_view name, const bool is_optional,
             const custom_type& custom_type, std::vector<scalar_expr> expressions) {
            self.add_output_argument(
                name, custom_type, is_optional,
                create_custom_type_construction(custom_type, std::move(expressions)));
          },
          py::arg("name"), py::arg("is_optional"), py::arg("custom_type"), py::arg("expressions"),
          py::doc("Record an output argument of custom type."))
      .def(
          "set_return_value",
          [](function_description& self, const scalar_expr& value) {
            self.set_return_value(scalar_type(numeric_primitive_type::floating_point), {value});
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const matrix_expr& value) {
            self.set_return_value(matrix_type(value.rows(), value.cols()), value);
          },
          py::arg("value"))
      .def(
          "set_return_value",
          [](function_description& self, const custom_type& custom_type,
             std::vector<scalar_expr> expressions) {
            self.set_return_value(
                custom_type, create_custom_type_construction(custom_type, std::move(expressions)));
          },
          py::arg("custom_type"), py::arg("expressions"))
      .def("output_expressions", &function_description::output_expressions,
           "Retrieve a dict of output expressions computed by this function.")
      .doc() = docstrings::function_description.data();

  wrap_class<optimization_params>(m, "OptimizationParams")
      .def(py::init<>(), "Construct with defaults.")
      .def_readwrite("factorization_passes", &optimization_params::factorization_passes,
                     "Automatically factorize sums of products. This parameter determines the "
                     "number of passes through the expression graph.")
      .def_readwrite("binarize_operations", &optimization_params::binarize_operations,
                     "Convert n-ary additions and multiplications into binary operations.");

  m.def(
      "transpile",
      [](const std::vector<function_description>& descriptions,
         const std::optional<optimization_params>& params, bool convert_ternaries) {
        // TODO: Allow this to run in parallel.
        // Could use std::execution_policy, but it is missing on macosx-13.
        return transform_map<std::vector>(
            descriptions, [&params, convert_ternaries](const function_description& d) {
              return transpile_to_ast(d, params, convert_ternaries);
            });
      },
      py::arg("desc"), py::arg("optimization_params") = py::none(),
      py::arg("convert_ternaries") = true,
      "Overload of :func:`wrenfold.code_generation.transpile` that operates on a sequence "
      "of functions.",
      py::return_value_policy::take_ownership);

  m.def("transpile", &transpile_to_ast, py::arg("desc"),
        py::arg("optimization_params") = py::none(), py::arg("convert_ternaries") = true,
        docstrings::transpile.data(), py::return_value_policy::take_ownership);

  m.def("cse_function_description", &cse_function_description, py::arg("desc"),
        py::arg("params") = py::none(), docstrings::cse_function_description.data(),
        py::return_value_policy::take_ownership);
}

}  // namespace wf
