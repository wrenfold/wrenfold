// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include "type_casters.h"

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
#include <nanobind/typing.h>

#include "wf/expression.h"
#include "wf/geometry/quaternion.h"
#include "wf/geometry/unit_vector.h"
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
  wrap_class<unit_vector>(m, "UnitN")
      .def(py::init<matrix_expr>(), "value"_a,
           "Construct from an Nx1 symbolic vector (N >= 2). The input is not normalized.")
      .def(
          "__init__",
          [](unit_vector* self, const py::typed<py::iterable, scalar_expr>& values) {
            std::vector<scalar_expr> components;
            cast_to_expr(values, components);
            const index_t dimension = static_cast<index_t>(components.size());
            new (self) unit_vector{matrix_expr::create(dimension, 1, std::move(components))};
          },
          "values"_a, "Construct from an iterable of symbolic components.")
      .def(
          "__init__",
          [](unit_vector* self, scalar_expr x, scalar_expr y, scalar_expr z) {
            new (self)
                unit_vector{matrix_expr::create(3, 1, {std::move(x), std::move(y), std::move(z)})};
          },
          "x"_a, "y"_a, "z"_a, "Construct a 3D unit vector from components.")
      .def_static("with_name", &unit_vector::from_name_prefix, "name"_a, "dimension"_a,
                  "Create a symbolic vector with names ``name_0`` through ``name_{N-1}``.")
      .def("__repr__",
           [](const unit_vector& self) {
             return fmt::format("UnitN({})", fmt::join(self.to_vector().to_vector(), ", "));
           })
      .def_prop_ro("dimension", &unit_vector::dimension, "Ambient dimension N.")
      .def("__getitem__",
           [](const unit_vector& self, index_t i) {
             if (i < 0) {
               i += self.dimension();
             }
             if (i < 0 || i >= self.dimension()) {
               throw py::index_error();
             }
             return self[i];
           })
      .def("to_vector", &unit_vector::to_vector, "Return the Nx1 ambient vector.")
      .def(
          "to_list", [](const unit_vector& self) { return self.to_vector().to_vector(); },
          "Return ambient components as a list.")
      .def(
          "eval", [](const unit_vector& self) { return numpy_from_matrix(self.to_vector(), {}); },
          "Evaluate numeric components to a NumPy column vector.")
      .def("subs", &unit_vector::subs, "target"_a, "replacement"_a)
      .def("squared_norm", &unit_vector::squared_norm)
      .def("norm", &unit_vector::norm)
      .def("normalized", &unit_vector::normalized)
      .def(
          "jacobian",
          [](const unit_vector& self, const matrix_expr& vars, const bool use_abstract) {
            return self.jacobian(vars, use_abstract ? non_differentiable_behavior::abstract
                                                    : non_differentiable_behavior::constant);
          },
          "vars"_a, py::arg("use_abstract") = false,
          "Compute the NxM ambient Jacobian with respect to vector variables.")
      .def("retract", &unit_vector::retract, "delta"_a,
           "Apply a (N-1)x1 tangent perturbation using Ceres' SphereManifold convention.")
      .def("local_coordinates", &unit_vector::local_coordinates, "other"_a,
           "Map another point to this point's (N-1)x1 tangent coordinates.")
      .def("retract_derivative", &unit_vector::retract_derivative,
           "Nx(N-1) Jacobian matching Ceres SphereManifold::PlusJacobian.")
      .def("local_coordinates_derivative", &unit_vector::local_coordinates_derivative,
           "(N-1)xN Jacobian matching Ceres SphereManifold::MinusJacobian.")
      .doc() = "A symbolic point on an N-dimensional sphere, with Ceres tangent coordinates.";
  m.attr("Unit3") = m.attr("UnitN");

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
          [](const py::typed<py::iterable, scalar_expr>& iterable) {
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
          [](const py::typed<py::iterable, scalar_expr>& iterable) {
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
