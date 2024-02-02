// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/numpy.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/expression.h"
#include "wf/geometry/quaternion.h"
#include "wf/matrix_expression.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Get four elements from an iterable, throw if there are not four.
static auto components_from_iterable(const py::iterable& iterable) {
  absl::InlinedVector<Expr, 4> values;
  cast_to_expr(iterable, values);
  if (values.size() != 4) {
    throw dimension_error("Expected 4 values but {} were provided.", values.size());
  }
  return values;
}

static py::list list_from_quaternion(const quaternion& q) {
  py::list list{};
  list.append(q.w());
  list.append(q.x());
  list.append(q.y());
  list.append(q.z());
  return list;
}

static py::array eval_quaternion(const quaternion& q) {
  py::list list{};  // TODO: Avoid copy into list.
  list.append(try_convert_to_numeric(q.w().eval()));
  list.append(try_convert_to_numeric(q.x().eval()));
  list.append(try_convert_to_numeric(q.y().eval()));
  list.append(try_convert_to_numeric(q.z().eval()));
  return py::array(list);
}

void wrap_geometry_operations(py::module_& m) {
  py::class_<quaternion>(m, "Quaternion")
      .def(py::init<Expr, Expr, Expr, Expr>(), "w"_a, "x"_a, "y"_a, "z"_a,
           py::doc("Construct from wxyz elements."))
      .def(py::init<>(), py::doc("Construct as identity."))
      .def_static(
          "with_name",
          [](const std::string_view name) {
            auto [w, x, y, z] = make_symbols(fmt::format("{}_w", name), fmt::format("{}_x", name),
                                             fmt::format("{}_y", name), fmt::format("{}_z", name));
            return quaternion{std::move(w), std::move(x), std::move(y), std::move(z)};
          },
          "name"_a, "Construct a symbolic quaternion with the given prefix.")
      .def("__repr__",
           [](const quaternion& self) {
             return fmt::format("Quaternion({}, {}, {}, {})", self.w(), self.x(), self.y(),
                                self.z());
           })
      .def(
          "is_identical_to",
          [](const quaternion& self, const quaternion& other) {
            return self.is_identical_to(other);
          },
          "other"_a, "Test if two quaternions have identical expression trees.")
      .def_property_readonly("type_name",
                             [](const quaternion&) -> std::string_view { return "Quaternion"; })
      // Expression operations:
      .def("subs", &quaternion::subs, py::arg("target"), py::arg("replacement"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("eval", &eval_quaternion,
           "Evaluate into a float expression. Return the result as a numpy array.")
      // Storage conversions:
      .def("to_list", &list_from_quaternion, "Convert to list in WXYZ order (scalar first).")
      .def("to_vector_wxyz", &quaternion::to_vector_wxyz,
           "Convert to 4x1 vector in WXYZ order (scalar first).")
      .def("to_vector_xyzw", &quaternion::to_vector_xyzw,
           "Convert to 4x1 vector in XYZW order (scalar last).")
      .def_property_readonly("w", &quaternion::w, "Access scalar element of quaternion.")
      .def_property_readonly("x", &quaternion::x, "Access `i` coefficient of quaternion.")
      .def_property_readonly("y", &quaternion::y, "Access `j` coefficient of quaternion.")
      .def_property_readonly("z", &quaternion::z, "Access `k` coefficient of quaternion.")
      .def_static("from_xyzw", &quaternion::from_vector_xyzw, "xyzw"_a,
                  "Construct from matrix of xyzw elements.")
      .def_static(
          "from_xyzw",
          [](py::iterable iterable) {
            const auto xyzw = components_from_iterable(iterable);
            return quaternion{xyzw[3], xyzw[0], xyzw[1], xyzw[2]};
          },
          "m"_a, "Construct from iterable over xyzw elements.")
      .def_static("from_wxyz", &quaternion::from_vector_wxyz, "m"_a,
                  "Construct from matrix of wxyz elements.")
      .def_static(
          "from_wxyz",
          [](py::iterable iterable) {
            const auto wxyz = components_from_iterable(iterable);
            return quaternion{wxyz[0], wxyz[1], wxyz[2], wxyz[3]};
          },
          "iterable"_a, "Construct from iterable over wxyz elements.")
      // Quaternion operations:
      .def("squared_norm", &quaternion::squared_norm, "Squared norm of four quaternion elements.")
      .def("norm", &quaternion::norm, "Norm of the four quaternion elements.")
      .def("normalized", &quaternion::normalized, "Return a normalized version of the quaternion.")
      .def("conjugate", &quaternion::conjugate, "Return the complex conjugate.")
      .def("inverse", &quaternion::inverse, "Return the normalized complex conjugate.")
      .def("to_rotation_matrix", &quaternion::to_rotation_matrix,
           "Return the equivalent member of SO(3). If this quaternion has non-unit norm, the "
           "result is undefined.")
      .def(py::self * py::self)
      // Conversion from angles:
      // TODO: Stubs are wrong for these, see: https://github.com/python/mypy/pull/14934
      .def_static(
          "from_angle_axis",
          static_cast<quaternion (*)(const Expr&, const Expr&, const Expr&, const Expr&)>(
              &quaternion::from_angle_axis),
          "angle"_a, "vx"_a, "vy"_a, "vz"_a,
          py::doc(
              "Create a quaternion from angle-axis parameters. Parameters [vx, vy, vz] must be a "
              "unit vector, or the result does not represent a rotation. Angle is specified in "
              "radians."))
      .def_static(
          "from_angle_axis",
          static_cast<quaternion (*)(const Expr&, const matrix_expr&)>(
              &quaternion::from_angle_axis),
          "angle"_a, "v"_a,
          py::doc("Create a quaternion from angle-axis parameters. Vector v must be a "
                  "unit vector, or the result does not represent a rotation. Angle is specified in "
                  "radians."))
      .def_static(
          "from_rotation_vector",
          static_cast<quaternion (*)(const Expr&, const Expr&, const Expr&, std::optional<Expr>)>(
              &quaternion::from_rotation_vector),
          "x"_a, "y"_a, "z"_a, py::arg("epsilon"),
          py::doc("Create a quaternion from Rodrigues rotation vector, expressed in units "
                  "of radians."))
      .def_static("from_rotation_vector",
                  static_cast<quaternion (*)(const matrix_expr&, std::optional<Expr>)>(
                      &quaternion::from_rotation_vector),
                  "v"_a, py::arg("epsilon"),
                  py::doc("Create a quaternion from Rodrigues rotation vector, expressed in units "
                          "of radians."))
      .def_static("from_x_angle", &quaternion::from_x_angle, "angle"_a,
                  py::doc("Construct a rotation about the x-axis. Angle is in radians."))
      .def_static("from_y_angle", &quaternion::from_y_angle, "angle"_a,
                  py::doc("Construct a rotation about the y-axis. Angle is in radians."))
      .def_static("from_z_angle", &quaternion::from_z_angle, "angle"_a,
                  py::doc("Construct a rotation about the z-axis. Angle is in radians."))
      .def("to_angle_axis", &quaternion::to_angle_axis, py::arg("epsilon") = constants::zero,
           py::doc("Convert quaternion to angle-axis representation. Returns an angle in the [0, "
                   "pi] interval and a unit-vector."))
      .def("to_rotation_vector", &quaternion::to_rotation_vector,
           py::arg("epsilon") = constants::zero,
           py::doc("Convert quaternion to rotation-vector representation."))
      .def_static("from_rotation_matrix", &quaternion::from_rotation_matrix, py::arg("R"),
                  py::doc("Convert from a rotation matrix via Calley's method."))
      .def(
          "jacobian",
          [](const quaternion& self, const matrix_expr& vars, bool use_abstract) {
            return self.jacobian(vars, use_abstract ? non_differentiable_behavior::abstract
                                                    : non_differentiable_behavior::constant);
          },
          py::arg("vars"), py::arg("use_abstract") = false,
          py::doc("Take jacobian of [w,x,y,z] quaternion elements with respect to variables."))
      .def(
          "jacobian",
          [](const quaternion& self, const std::vector<Expr>& vars, bool use_abstract) {
            return self.jacobian(vars, use_abstract ? non_differentiable_behavior::abstract
                                                    : non_differentiable_behavior::constant);
          },
          py::arg("vars"), py::arg("use_abstract") = false,
          py::doc("Take jacobian of [w,x,y,z] quaternion elements with respect to variables."))
      .def(
          "jacobian",
          [](const quaternion& self, const quaternion& vars, bool use_abstract) {
            return self.jacobian(vars, use_abstract ? non_differentiable_behavior::abstract
                                                    : non_differentiable_behavior::constant);
          },
          py::arg("vars"), py::arg("use_abstract") = false,
          py::doc("Take jacobian of [w,x,y,z] quaternion elements with respect to variables in "
                  "another quaternion."))
      .def("right_retract_derivative", &quaternion::right_retract_derivative,
           py::doc("Jacobian of q * exp(w) wrt w around w = 0."))
      .def("right_local_coordinates_derivative", &quaternion::right_local_coordinates_derivative,
           py::doc("Jacobian of log(q^T * (q + dq)) wrt dq around dq = 0."));

  m.def("left_jacobian_of_so3", &left_jacobian_of_so3, "w"_a, "epsilon"_a,
        py::doc("Evaluate the left jacobian of SO(3)."));
}
}  // namespace wf
