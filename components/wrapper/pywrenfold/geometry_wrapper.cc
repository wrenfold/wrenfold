// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <nanobind/eigen/dense.h>
#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <nanobind/operators.h>
#include <nanobind/stl/complex.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/string_view.h>
#include <nanobind/stl/tuple.h>
#include <nanobind/stl/variant.h>
#include <nanobind/stl/vector.h>

#include "wf/expression.h"
#include "wf/geometry/quaternion.h"
#include "wf/matrix_expression.h"

#include "docs/geometry_wrapper.h"
#include "numpy_conversion.h"
#include "wrapper_utils.h"

namespace py = nanobind;
using namespace py::literals;

namespace wf {

// Get four elements from an iterable. Throw if there are not four.
static std::vector<scalar_expr> components_from_iterable(const py::iterable& iterable) {
  std::vector<scalar_expr> values;
  cast_to_expr(iterable, values);
  if (values.size() != 4) {
    throw dimension_error("Expected 4 values but {} were provided.", values.size());
  }
  return values;
}

static std::vector<scalar_expr> list_from_quaternion(const quaternion& q) {
  return {q.w(), q.x(), q.y(), q.z()};
}

static auto eval_quaternion(const quaternion& q) {
  return numpy_from_matrix(q.to_vector_wxyz(), {});
}

void wrap_geometry_operations(py::module_& m) {
  wrap_class<quaternion>(m, "Quaternion")
      .def(py::init<scalar_expr, scalar_expr, scalar_expr, scalar_expr>(), "w"_a, "x"_a, "y"_a,
           "z"_a, docstrings::quaternion_constructor.data())
      .def(py::init<>(), docstrings::quaternion_identity_constructor.data())
      .def_static(
          "with_name",
          [](const std::string_view name) {
            auto [w, x, y, z] = make_symbols(fmt::format("{}_w", name), fmt::format("{}_x", name),
                                             fmt::format("{}_y", name), fmt::format("{}_z", name));
            return quaternion{std::move(w), std::move(x), std::move(y), std::move(z)};
          },
          "name"_a, docstrings::quaternion_with_name.data())
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
      // Expression operations:
      .def("subs", &quaternion::subs, py::arg("target"), py::arg("replacement"),
           "Invoke :func:`wrenfold.sym.Expr.subs` on every element of the quaternion.")
      .def("eval", &eval_quaternion, docstrings::quaternion_eval.data())
      // Storage conversions:
      .def("to_list", &list_from_quaternion,
           "Convert to list in ``[w, x, y, z]`` (scalar first) order.")
      .def("to_vector_wxyz", &quaternion::to_vector_wxyz,
           "Convert to a 4x1 vector in ``[w, x, y, z]`` (scalar-first) order.")
      .def("to_vector_xyzw", &quaternion::to_vector_xyzw,
           "Convert to a 4x1 vector in ``[x, y, z, w]`` (scalar-last) order.")
      .def_prop_ro("w", &quaternion::w, "Access scalar element of quaternion.")
      .def_prop_ro("x", &quaternion::x, "Access **i** coefficient of quaternion.")
      .def_prop_ro("y", &quaternion::y, "Access **j** coefficient of quaternion.")
      .def_prop_ro("z", &quaternion::z, "Access **k** coefficient of quaternion.")
      .def_static("from_xyzw", &quaternion::from_vector_xyzw, "xyzw"_a,
                  docstrings::quaternion_from_xyzw.data())
      .def_static(
          "from_xyzw",
          [](const py::iterable& iterable) {
            const auto xyzw = components_from_iterable(iterable);
            return quaternion{xyzw[3], xyzw[0], xyzw[1], xyzw[2]};
          },
          "xyzw"_a,
          "Overload of :func:`wrenfold.geometry.Quaternion.from_xyzw` that accepts "
          "Iterable[sym.Expr].")
      .def_static("from_wxyz", &quaternion::from_vector_wxyz, "wxyz"_a,
                  docstrings::quaternion_from_wxyz.data())
      .def_static(
          "from_wxyz",
          [](const py::iterable& iterable) {
            const auto wxyz = components_from_iterable(iterable);
            return quaternion{wxyz[0], wxyz[1], wxyz[2], wxyz[3]};
          },
          "wxyz"_a,
          "Overload of :func:`wrenfold.geometry.Quaternion.from_wxyz` that accepts "
          "Iterable[sym.Expr].")
      // Quaternion operations:
      .def("squared_norm", &quaternion::squared_norm, docstrings::quaternion_squared_norm.data())
      .def("norm", &quaternion::norm, docstrings::quaternion_norm.data())
      .def("normalized", &quaternion::normalized, docstrings::quaternion_normalized.data())
      .def("conjugate", &quaternion::conjugate, docstrings::quaternion_conjugate.data())
      .def("inverse", &quaternion::inverse, docstrings::quaternion_inverse.data())
      .def("to_rotation_matrix", &quaternion::to_rotation_matrix,
           docstrings::quaternion_to_rotation_matrix.data())
      .def(py::self * py::self, docstrings::quaternion_operator_multiply.data())
      .def("rotate", &quaternion::rotate, py::arg("v"), docstrings::quaternion_rotate.data())
      // Conversion from angles:
      // TODO: Stubs are wrong for these, see: https://github.com/python/mypy/pull/14934
      .def_static(
          "from_angle_axis",
          static_cast<quaternion (*)(const scalar_expr&, const scalar_expr&, const scalar_expr&,
                                     const scalar_expr&)>(&quaternion::from_angle_axis),
          "angle"_a, "vx"_a, "vy"_a, "vz"_a, docstrings::quaternion_from_angle_axis.data())
      .def_static("from_angle_axis",
                  static_cast<quaternion (*)(const scalar_expr&, const matrix_expr&)>(
                      &quaternion::from_angle_axis),
                  "angle"_a, "axis"_a,
                  "Overload of ``from_angle_axis`` that accepts ``sym.MatrixExpr`` for the axis.")
      .def_static(
          "from_rotation_vector",
          static_cast<quaternion (*)(const scalar_expr&, const scalar_expr&, const scalar_expr&,
                                     const std::optional<scalar_expr>&)>(
              &quaternion::from_rotation_vector),
          "x"_a, "y"_a, "z"_a, py::arg("epsilon").none(),
          docstrings::quaternion_from_rotation_vector.data())
      .def_static(
          "from_rotation_vector",
          static_cast<quaternion (*)(const matrix_expr&, const std::optional<scalar_expr>&)>(
              &quaternion::from_rotation_vector),
          py::arg("v"), py::arg("epsilon").none(),
          "Overload of ``from_rotation_vector`` that accepts ``sym.MatrixExpr``.")
      .def_static("from_x_angle", &quaternion::from_x_angle, "angle"_a,
                  docstrings::quaternion_from_x_angle.data())
      .def_static("from_y_angle", &quaternion::from_y_angle, "angle"_a,
                  docstrings::quaternion_from_y_angle.data())
      .def_static("from_z_angle", &quaternion::from_z_angle, "angle"_a,
                  docstrings::quaternion_from_z_angle.data())
      .def("to_angle_axis", &quaternion::to_angle_axis, py::arg("epsilon").none() = constants::zero,
           docstrings::quaternion_to_angle_axis.data())
      .def("to_rotation_vector", &quaternion::to_rotation_vector,
           py::arg("epsilon").none() = constants::zero, py::arg("use_atan2") = true,
           docstrings::quaternion_to_rotation_vector.data())
      .def_static("from_rotation_matrix", &quaternion::from_rotation_matrix, py::arg("R"),
                  docstrings::quaternion_from_rotation_matrix.data())
      .def("right_retract_derivative", &quaternion::right_retract_derivative,
           docstrings::quaternion_right_retract_derivative.data())
      .def("right_local_coordinates_derivative", &quaternion::right_local_coordinates_derivative,
           docstrings::quaternion_right_local_coordinates_derivative.data())
      .doc() = "A quaternion class used to represent 3D rotations and orientations.";

  m.def("left_jacobian_of_so3", &left_jacobian_of_so3, py::arg("w"), py::arg("epsilon").none(),
        docstrings::left_jacobian_of_so3.data());

  m.def("inverse_left_jacobian_of_so3", &inverse_left_jacobian_of_so3, py::arg("w"),
        py::arg("epsilon").none(), docstrings::inverse_left_jacobian_of_so3.data());
}
}  // namespace wf
