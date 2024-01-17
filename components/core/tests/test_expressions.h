// Copyright 2023 Gareth Cross
#pragma once
#include "wf/code_generation/declare_custom_function.h"
#include "wf/code_generation/type_registry.h"
#include "wf/expression.h"
#include "wf/functions.h"
#include "wf/geometry/quaternion.h"
#include "wf/matrix_functions.h"
#include "wf/output_annotations.h"
#include "wf/type_annotations.h"

// Some symbolic functions we use in unit tests.
// ReSharper disable CppPassValueParameterByConstReference
namespace wf {

namespace ta = type_annotations;

// A very simple function that combines three input arguments.
inline Expr simple_multiply_add(Expr x, Expr y, Expr z) { return x * y + z; }

// Rotate a 2D vector by an angle. Output the vector, and its derivative with respect
// to the angle.
inline auto vector_rotation_2d(Expr theta, ta::static_matrix<2, 1> v) {
  using namespace matrix_operator_overloads;
  MatrixExpr R = make_matrix(2, 2, cos(theta), -sin(theta), sin(theta), cos(theta));
  ta::static_matrix<2, 1> v_rot{R * v};
  ta::static_matrix<2, 1> v_dot_D_theta{v_rot.inner().diff(theta)};
  return std::make_tuple(output_arg("v_rot", v_rot), optional_output_arg("D_theta", v_dot_D_theta));
}

// Norm of a 3D vector + the 1x3 derivative.
inline auto vector_norm_3d(ta::static_matrix<3, 1> v) {
  Expr len = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  auto D_v = ta::static_matrix<1, 3>{len.diff(v[0]), len.diff(v[1]), len.diff(v[2])};
  return std::make_tuple(return_value(len), output_arg("D_v", D_v));
}

// Heaviside step function - a very simple test of conditionals.
inline Expr heaviside(Expr x) { return where(x >= 0, 1, 0); }

// Exclusive or on the conditions x > 0 and y > 0 (a simple nested conditional).
inline Expr exclusive_or(Expr x, Expr y) {
  Expr cond_x = x > 0;
  Expr cond_y = y > 0;
  return where(cond_x, where(cond_y, 0, 1), where(cond_y, 1, 0));
}

// Test generation of signum and abs.
inline auto signum_and_abs(Expr x) {
  return std::make_tuple(return_value(signum(x)), output_arg("abs", abs(x)));
}

// Arc-tangent w/ derivatives.
inline auto atan2_with_derivatives(Expr y, Expr x) {
  Expr f = atan2(y, x);
  return std::make_tuple(return_value(f), output_arg("D_y", f.diff(y)),
                         output_arg("D_x", f.diff(x)));
}

// Three layers of nested conditionals.
inline auto nested_conditionals_1(Expr x, Expr y) {
  Expr c0 = where(y > 0, cos(x * y), cos(x) + 2);
  Expr c1 = where(x > 0, log(abs(y)), atan2(y, x) * 3);
  Expr c2 = where(abs(x) > abs(y), c0 * 3 - sqrt(abs(c0)), pow(abs(c1), 1.0 / 3.0) / 5);
  return c2;
}

// Nested conditionals, but only on the if-branch side.
inline auto nested_conditionals_2(Expr x, Expr y) {
  Expr c0 = where(y > 0, cos(x * y * constants::pi), sin(22 / y) - 3 * x);
  Expr c1 = where(x > 0, c0, atan2(y * 0.4, x * 0.1) * 19 - y);
  Expr c2 = where(abs(x) > abs(y), c1, sqrt(abs(x * y + 2 * x)));
  return c2;
}

// Create a rotation matrix from a Rodrigues vector, and the 9x3 Jacobian of the rotation matrix
// elements with respect to the vector.
inline auto create_rotation_matrix(ta::static_matrix<3, 1> w) {
  MatrixExpr R = quaternion::from_rotation_vector(w.inner(), 1.0e-16).to_rotation_matrix();
  MatrixExpr R_diff = vectorize_matrix(R).jacobian(w);
  return std::make_tuple(output_arg("R", ta::static_matrix<3, 3>{R}),
                         optional_output_arg("R_D_w", ta::static_matrix<9, 3>{R_diff}));
}

// Recover a Rodrigues rotation vector from a 3x3 rotation matrix.
inline auto rotation_vector_from_matrix(ta::static_matrix<3, 3> R) {
  quaternion q = quaternion::from_rotation_matrix(R);
  ta::static_matrix<3, 1> w = q.to_rotation_vector(1.0e-16);
  return std::make_tuple(output_arg("w", w));
}

// A function with no required outputs (all outputs optional).
inline auto no_required_outputs(Expr x) {
  return std::make_tuple(optional_output_arg("out1", cos(x) + 2),
                         optional_output_arg("out2", abs(x) * 2));
}

namespace symbolic {
// A simple custom type.
struct Point2d : custom_type_base<Point2d> {
  Expr x{0};
  Expr y{0};

  Point2d() = default;
  Point2d(Expr x, Expr y) : x(std::move(x)), y(std::move(y)) {}

  static auto register_type(custom_type_registry& registry) {
    return custom_type_builder<symbolic::Point2d>(registry, "Point2d")
        .add_field("x", &symbolic::Point2d::x)
        .add_field("y", &symbolic::Point2d::y);
  }
};

// Custom type with nested type.
struct Circle : custom_type_base<Circle> {
  Point2d center{};
  Expr radius{0};

  Circle() = default;
  Circle(Point2d center, Expr radius) : center(std::move(center)), radius(std::move(radius)) {}

  static auto register_type(custom_type_registry& registry) {
    return custom_type_builder<symbolic::Circle>(registry, "Circle")
        .add_field("center", &symbolic::Circle::center)
        .add_field("radius", &symbolic::Circle::radius);
  }
};
}  // namespace symbolic

// A method that accepts a custom point.
inline symbolic::Point2d custom_type_1(const symbolic::Point2d& p) {
  const Expr norm = sqrt(p.x * p.x + p.y * p.y);
  return {where(norm > 0, p.x / norm, 0), where(norm > 0, p.y / norm, 0)};
}

// A method that outputs a custom type via optional output argument.
inline auto custom_type_2(Expr theta, Expr radius) {
  symbolic::Point2d p{cos(theta) * radius, sin(theta) * radius};
  ta::static_matrix<2, 2> D_inputs = make_vector(p.x, p.y).jacobian({theta, radius});
  return std::make_tuple(optional_output_arg("out", p), optional_output_arg("D_inputs", D_inputs));
}

// Construct a nested custom type.
inline auto nested_custom_type_1(symbolic::Circle a, symbolic::Point2d b) {
  // Expand circle `a` if required to fit point `b`.
  Expr d2 = make_vector(a.center.x - b.x, a.center.y - b.y).squared_norm();
  Expr d = sqrt(d2);
  symbolic::Circle result{};
  result.radius = where(d <= a.radius, a.radius, d);
  result.center = a.center;
  return result;
}

// Declare an external function.
struct external_function_1
    : declare_custom_function<external_function_1, Expr, type_list<Expr, Expr>> {
  static constexpr std::string_view name() noexcept { return "external_function_1"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

inline auto custom_function_call_1(Expr x, Expr y) {
  const Expr f = external_function_1::call(x * 2, y - 5);
  return f * x;
}

// An external function that accepts a matrix argument.
struct external_function_2
    : declare_custom_function<external_function_2, Expr, type_list<ta::static_matrix<2, 3>>> {
  static constexpr std::string_view name() noexcept { return "external_function_2"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0"); }
};

inline auto custom_function_call_2(ta::static_matrix<2, 1> u, ta::static_matrix<2, 1> v) {
  // clang-format off
  const MatrixExpr m = make_matrix(2, 3,
    u[0] - 2.0, pow(u[1], 2), 1,
    v[0], pow(v[1] + 1.0, 2), 1);
  // clang-format on
  return external_function_2::call(m);
}

// An external function that returns a matrix.
struct external_function_3
    : declare_custom_function<external_function_3, ta::static_matrix<2, 2>,
                              type_list<ta::static_matrix<2, 1>, ta::static_matrix<2, 1>>> {
  static constexpr std::string_view name() noexcept { return "external_function_3"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("arg0", "arg1"); }
};

inline auto custom_function_call_3(Expr x, ta::static_matrix<2, 1> v) {
  return external_function_3::call(make_vector(x, pow(x, 2)), v);
}

// An external function that accepts and returns custom types.
struct external_function_4 : declare_custom_function<external_function_4, symbolic::Point2d,
                                                     type_list<symbolic::Point2d>> {
  static constexpr std::string_view name() noexcept { return "external_function_4"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("p"); }
};

inline auto custom_function_call_4(Expr a, Expr b) {
  const symbolic::Point2d p = external_function_4::call(symbolic::Point2d{a - b, b * 2});
  return where(abs(p.x) > abs(p.y), p.x, p.y);
}

// A external function that accepts a nested custom type.
struct external_function_5
    : declare_custom_function<external_function_5, Expr,
                              type_list<symbolic::Circle, symbolic::Circle>> {
  static constexpr std::string_view name() noexcept { return "external_function_5"; }
  static constexpr auto arg_names() noexcept { return std::make_tuple("c", "p"); }
};

inline auto custom_function_call_5(symbolic::Circle c, Expr x, Expr y) {
  return 2 * external_function_5::call(c, symbolic::Circle{symbolic::Point2d{x, y}, 1.0}) - 1;
}

}  // namespace wf
