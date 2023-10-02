// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/numpy.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>

#include "expression.h"
#include "geometry/quaternion.h"
#include "matrix_expression.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace math {

static auto ComponentsFromIterable(const py::iterable& iterable) {
  absl::InlinedVector<Expr, 4> values;
  TransformIntoExpr(iterable, values);
  if (values.size() != 4) {
    throw DimensionError("Expected 4 values but {} were provided.", values.size());
  }
  return values;
}

// Check that matrix has correct dims to be converted to quaternion.
static void CheckMatrixDims(const MatrixExpr& m) {
  const bool valid_dims =
      (m.NumRows() == 4 && m.NumCols() == 1) || (m.NumRows() == 1 && m.NumCols() == 4);
  if (!valid_dims) {
    throw TypeError("Matrix must be a 4-element vector. Provided has dimension: [{}, {}]",
                    m.NumRows(), m.NumCols());
  }
}

static py::list ListFromQuaternion(const Quaternion& q) {
  py::list list{};
  list.append(q.w());
  list.append(q.x());
  list.append(q.y());
  list.append(q.z());
  return list;
}

static py::array EvalQuaternion(const Quaternion& q) {
  py::list list{};  // TODO: Avoid copy into list.
  list.append(TryConvertToNumeric(q.w().Eval()));
  list.append(TryConvertToNumeric(q.x().Eval()));
  list.append(TryConvertToNumeric(q.y().Eval()));
  list.append(TryConvertToNumeric(q.z().Eval()));
  return py::array(list);
}

void WrapGeometryOperations(py::module_& m) {
  py::class_<Quaternion>(m, "Quaternion")
      .def(py::init<Expr, Expr, Expr, Expr>(), "w"_a, "x"_a, "y"_a, "z"_a,
           py::doc("Construct from wxyz elements."))
      .def(py::init<>(), py::doc("Construct as identity."))
      .def_static(
          "with_name",
          [](const std::string_view name) {
            auto [w, x, y, z] = Symbols(fmt::format("{}_w", name), fmt::format("{}_x", name),
                                        fmt::format("{}_y", name), fmt::format("{}_z", name));
            return Quaternion{std::move(w), std::move(x), std::move(y), std::move(z)};
          },
          "name"_a, "Construct a symbolic quaternion with the given prefix.")
      .def("__repr__",
           [](const Quaternion& self) {
             return fmt::format("Quaternion({}, {}, {}, {})", self.w(), self.x(), self.y(),
                                self.z());
           })
      .def(
          "is_identical_to",
          [](const Quaternion& self, const Quaternion& other) { return self.is_identical_to(other); },
          "other"_a, "Test if two quaternions have identical expression trees.")
      .def_property_readonly("type_name",
                             [](const Quaternion&) -> std::string_view { return "Quaternion"; })
      // Expression operations:
      .def("subs", &Quaternion::Subs, py::arg("target"), py::arg("replacement"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("eval", &EvalQuaternion,
           "Evaluate into a float expression. Return the result as a numpy array.")
      // Storage conversions:
      .def("to_list", &ListFromQuaternion, "Convert to list in WXYZ order (scalar first).")
      .def("to_vector_wxyz", &Quaternion::ToVectorWXYZ,
           "Convert to 4x1 vector in WXYZ order (scalar first).")
      .def_property_readonly("w", &Quaternion::w, "Access scalar element of quaternion.")
      .def_property_readonly("x", &Quaternion::x, "Access `i` coefficient of quaternion.")
      .def_property_readonly("y", &Quaternion::y, "Access `j` coefficient of quaternion.")
      .def_property_readonly("z", &Quaternion::z, "Access `k` coefficient of quaternion.")
      .def_static(
          "from_xyzw",
          [](const MatrixExpr& xyzw) {
            CheckMatrixDims(xyzw);
            return Quaternion{xyzw[3], xyzw[0], xyzw[1], xyzw[2]};
          },
          "xyzw"_a, "Construct from matrix of xyzw elements.")
      .def_static(
          "from_xyzw",
          [](py::iterable iterable) {
            const auto xyzw = ComponentsFromIterable(iterable);
            return Quaternion{xyzw[3], xyzw[0], xyzw[1], xyzw[2]};
          },
          "m"_a, "Construct from iterable over xyzw elements.")
      .def_static(
          "from_wxyz",
          [](const MatrixExpr& wxyz) {
            CheckMatrixDims(wxyz);
            return Quaternion{wxyz[0], wxyz[1], wxyz[2], wxyz[3]};
          },
          "m"_a, "Construct from matrix of wxyz elements.")
      .def_static(
          "from_wxyz",
          [](py::iterable iterable) {
            const auto wxyz = ComponentsFromIterable(iterable);
            return Quaternion{wxyz[0], wxyz[1], wxyz[2], wxyz[3]};
          },
          "iterable"_a, "Construct from iterable over wxyz elements.")
      // Quaternion operations:
      .def("squared_norm", &Quaternion::SquaredNorm, "Squared norm of four quaternion elements.")
      .def("norm", &Quaternion::Norm, "Norm of the four quaternion elements.")
      .def("normalized", &Quaternion::Normalized, "Return a normalized version of the quaternion.")
      .def("conjugate", &Quaternion::Conjugate, "Return the complex conjugate.")
      .def("inverse", &Quaternion::Inverse, "Return the normalized complex conjugate.")
      .def("to_rotation_matrix", &Quaternion::ToRotationMatrix,
           "Return the equivalent member of SO(3). If this quaternion has non-unit norm, the "
           "result is undefined.")
      .def(py::self * py::self)
      // Conversion from angles:
      // TODO: Stubs are wrong for these, see: https://github.com/python/mypy/pull/14934
      .def_static(
          "from_angle_axis",
          static_cast<Quaternion (*)(const Expr&, const Expr&, const Expr&, const Expr&)>(
              &Quaternion::FromAngleAxis),
          "angle"_a, "vx"_a, "vy"_a, "vz"_a,
          py::doc(
              "Create a quaternion from angle-axis parameters. Parameters [vx, vy, vz] must be a "
              "unit vector, or the result does not represent a rotation. Angle is specified in "
              "radians."))
      .def_static(
          "from_angle_axis",
          static_cast<Quaternion (*)(const Expr&, const MatrixExpr&)>(&Quaternion::FromAngleAxis),
          "angle"_a, "v"_a,
          py::doc("Create a quaternion from angle-axis parameters. Vector v must be a "
                  "unit vector, or the result does not represent a rotation. Angle is specified in "
                  "radians."))
      .def_static("from_rotation_vector",
                  static_cast<Quaternion (*)(const Expr&, const Expr&, const Expr&)>(
                      &Quaternion::FromRotationVector),
                  "x"_a, "y"_a, "z"_a,
                  py::doc("Create a quaternion from Rodrigues rotation vector, expressed in units "
                          "of radians."))
      .def_static("from_rotation_vector",
                  static_cast<Quaternion (*)(const MatrixExpr&)>(&Quaternion::FromRotationVector),
                  "v"_a,
                  py::doc("Create a quaternion from Rodrigues rotation vector, expressed in units "
                          "of radians."))
      .def_static("from_x_angle", &Quaternion::FromXAngle, "angle"_a,
                  py::doc("Construct a rotation about the x-axis. Angle is in radians."))
      .def_static("from_y_angle", &Quaternion::FromYAngle, "angle"_a,
                  py::doc("Construct a rotation about the y-axis. Angle is in radians."))
      .def_static("from_z_angle", &Quaternion::FromZAngle, "angle"_a,
                  py::doc("Construct a rotation about the z-axis. Angle is in radians."))
      .def("to_angle_axis", &Quaternion::ToAngleAxis, py::arg("zero_epsilon") = Constants::Zero,
           py::doc("Convert quaternion to angle-axis representation. Returns an angle in the [0, "
                   "pi] interval and a unit-vector."))
      .def_static("from_rotation_matrix", &Quaternion::FromRotationMatrix, py::arg("R"),
                  py::doc("Convert from a rotation matrix via Calley's method."));
}
}  // namespace math
