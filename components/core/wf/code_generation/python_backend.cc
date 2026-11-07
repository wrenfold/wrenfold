// wrenfold symbolic code generator.
// Copyright (c) 2026 wrenfold contributors
// For license information refer to accompanying LICENSE file.
#include "wf/code_generation/python_backend.h"

#include <vector>

namespace wf::detail {
namespace {

// NumPy and JAX share array construction and shape conventions. Scalar conversion and mutation
// remain the responsibility of the concrete adapter.
class array_backend : public python_backend {
 public:
  using python_backend::python_backend;

  std::string convert_matrix(const std::string_view value, const matrix_type& type,
                             const python_generator_float_width width) const override {
    return fmt::format("{}.asarray({}, dtype={}).reshape({}, {})", module_alias(), value,
                       dtype(numeric_primitive_type::floating_point, width), type.rows(),
                       type.cols());
  }

  std::string construct_matrix(const std::span<const std::string> values, const matrix_type& type,
                               python_generator_float_width) const override {
    return fmt::format("{}.array([{}]).reshape({}, {})", module_alias(), fmt::join(values, ", "),
                       type.rows(), type.cols());
  }

  std::string matrix_size(const std::string_view value) const override {
    return fmt::format("{}.size", value);
  }
};

class numpy_backend final : public array_backend {
 public:
  constexpr numpy_backend() : array_backend("np", "import numpy as np", "np.ndarray") {}

  std::string convert_scalar(const std::string_view value, const numeric_primitive_type type,
                             const python_generator_float_width width) const override {
    return fmt::format("{}({})", dtype(type, width), value);
  }

  std::string assign_matrix(const std::string_view destination,
                            const std::string_view value) const override {
    // Reshape the result: reshaping a strided destination can create a copy and lose the write.
    return fmt::format("{}[...] = {}.reshape({}.shape)", destination, value, destination);
  }
};

class jax_backend final : public array_backend {
 public:
  constexpr jax_backend() : array_backend("jnp", "import jax.numpy as jnp", "jnp.ndarray") {}

  std::string convert_scalar(const std::string_view value, const numeric_primitive_type type,
                             const python_generator_float_width width) const override {
    if (type == numeric_primitive_type::integral) {
      // Respect JAX's configured integer width without changing its global x64 setting.
      return fmt::format("jnp.asarray({}, dtype=jnp.result_type(0))", value);
    }
    return fmt::format("{}({})", dtype(type, width), value);
  }

  std::string assign_matrix(std::string_view, std::string_view) const override {
    throw type_error("JAX arrays are immutable. Use use_output_arguments=False.");
  }
};

class pytorch_backend final : public python_backend {
 public:
  constexpr pytorch_backend() : python_backend("th", "import torch as th", "th.Tensor") {}

  std::string convert_scalar(const std::string_view value, const numeric_primitive_type type,
                             const python_generator_float_width width) const override {
    // Unlike torch.asarray on supported older releases, as_tensor preserves autograd history.
    return fmt::format("th.as_tensor({}, dtype={})", value, dtype(type, width));
  }

  std::string convert_matrix(const std::string_view value, const matrix_type& type,
                             const python_generator_float_width width) const override {
    return fmt::format("{}.reshape({}, {})",
                       convert_scalar(value, numeric_primitive_type::floating_point, width),
                       type.rows(), type.cols());
  }

  std::string floating_literal(const std::string_view value,
                               python_generator_float_width) const override {
    // Python numbers work with tensor arithmetic on any device. Tensorize them when constructing
    // a matrix, where stack requires every element to be a tensor on the same device.
    return std::string{value};
  }

  std::string call_math(const std_math_function function, const std::span<const std::string> args,
                        const python_generator_float_width width) const override {
    std::vector<std::string> tensors;
    tensors.reserve(args.size());
    for (std::size_t i = 0; i < args.size(); ++i) {
      if (function == std_math_function::abs || function == std_math_function::signum ||
          function == std_math_function::floor) {
        // These operations preserve the input's numeric type, including int64 inputs.
        tensors.push_back(fmt::format("th.as_tensor({})", args[i]));
      } else {
        const auto type = function == std_math_function::powi && i == 1
                              ? numeric_primitive_type::integral
                              : numeric_primitive_type::floating_point;
        tensors.push_back(convert_scalar(args[i], type, width));
      }
    }
    return python_backend::call_math(function, tensors, width);
  }

  std::string construct_matrix(const std::span<const std::string> values, const matrix_type& type,
                               const python_generator_float_width width) const override {
    // Evaluate operands once, preferring a non-CPU device over CPU scalar constants. Locally bound
    // names cannot shadow caller expressions, which are evaluated outside the lambdas. Device
    // selection uses no Python builtins that could collide with user argument names.
    return fmt::format(
        "(lambda _values: (lambda _device: th.stack([th.as_tensor(_value, dtype={}, "
        "device=_device) for _value in _values]))(([_value.device for _value in _values "
        "if th.is_tensor(_value) and _value.device.type != 'cpu'] + "
        "[_value.device for _value in _values if th.is_tensor(_value)] + [None])[0]))"
        "([{}]).reshape({}, {})",
        dtype(numeric_primitive_type::floating_point, width), fmt::join(values, ", "), type.rows(),
        type.cols());
  }

  std::string assign_matrix(const std::string_view destination,
                            const std::string_view value) const override {
    return fmt::format("{}[...] = {}.reshape({}.shape)", destination, value, destination);
  }

  std::string matrix_size(const std::string_view value) const override {
    return fmt::format("{}.numel()", value);
  }
};

constexpr std::string_view math_function_name(const std_math_function function) {
  switch (function) {
    case std_math_function::cos:
    case std_math_function::sin:
    case std_math_function::tan:
    case std_math_function::cosh:
    case std_math_function::sinh:
    case std_math_function::tanh:
    case std_math_function::sqrt:
    case std_math_function::abs:
    case std_math_function::floor:
    case std_math_function::log:
    case std_math_function::exp:
      return string_from_standard_library_function(function);
    case std_math_function::acos:
      return "arccos";
    case std_math_function::asin:
      return "arcsin";
    case std_math_function::atan:
      return "arctan";
    case std_math_function::acosh:
      return "arccosh";
    case std_math_function::asinh:
      return "arcsinh";
    case std_math_function::atanh:
      return "arctanh";
    case std_math_function::signum:
      return "sign";
    case std_math_function::atan2:
      return "arctan2";
    case std_math_function::powi:
    case std_math_function::powf:
      return "power";
  }
  WF_ASSERT_ALWAYS("Unhandled standard math function: {}",
                   string_from_standard_library_function(function));
}

}  // namespace

std::string python_backend::dtype(const numeric_primitive_type type,
                                  const python_generator_float_width width) const {
  const auto name = [&]() -> std::string_view {
    switch (type) {
      case numeric_primitive_type::boolean:
        return "bool";
      case numeric_primitive_type::integral:
        return "int64";
      case numeric_primitive_type::floating_point:
        return width == python_generator_float_width::float32 ? "float32" : "float64";
    }
    WF_ASSERT_ALWAYS("Invalid numeric type");
  }();
  return fmt::format("{}.{}", module_alias(), name);
}

std::string python_backend::floating_literal(const std::string_view value,
                                             const python_generator_float_width width) const {
  return convert_scalar(value, numeric_primitive_type::floating_point, width);
}

std::string python_backend::symbolic_constant(const symbolic_constant_enum value,
                                              const python_generator_float_width width) const {
  return floating_literal(
      fmt::format("{}.{}", module_alias(), value == symbolic_constant_enum::euler ? "e" : "pi"),
      width);
}

std::string python_backend::call_math(const std_math_function function,
                                      const std::span<const std::string> args,
                                      python_generator_float_width) const {
  return fmt::format("{}.{}({})", module_alias(), math_function_name(function),
                     fmt::join(args, ", "));
}

std::string python_backend::select(const std::string_view condition, const std::string_view left,
                                   const std::string_view right) const {
  return fmt::format("{}.where({}, {}, {})", module_alias(), condition, left, right);
}

const python_backend& get_python_backend(const python_generator_target target) {
  static const numpy_backend numpy;
  static const pytorch_backend pytorch;
  static const jax_backend jax;
  switch (target) {
    case python_generator_target::numpy:
      return numpy;
    case python_generator_target::pytorch:
      return pytorch;
    case python_generator_target::jax:
      return jax;
  }
  WF_ASSERT_ALWAYS("Invalid Python backend");
}

}  // namespace wf::detail
