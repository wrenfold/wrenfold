"""Representative type-check-only tests for the public Python API."""

import pywrenfold
from pywrenfold import gen, geometry, sym, type_info

x = sym.symbol("x")
y = sym.symbol("y")

expr_comparisons: tuple[sym.BooleanExpr, ...] = (
    x < y,
    x > y,
    x <= y,
    x >= y,
)

numeric_comparisons: tuple[sym.BooleanExpr, ...] = (
    x < 1,
    x > 1,
    x <= 1,
    x >= 1,
    x > 1,
    x < 1,
    x >= 1,
    x <= 1,
    x < 1.5,
    x > 1.5,
    x <= 1.5,
    x >= 1.5,
    x > 1.5,
    x < 1.5,
    x >= 1.5,
    x <= 1.5,
)

quaternion_from_iterables: tuple[geometry.Quaternion, ...] = (
    geometry.Quaternion.from_xyzw([x, x, x, x]),
    geometry.Quaternion.from_wxyz((x, x, x, x)),
)

rotation_vector = quaternion_from_iterables[0].to_rotation_vector(epsilon=1.0e-16, use_atan2=False)
rotation_jacobians: tuple[sym.MatrixExpr, ...] = (
    geometry.left_jacobian_of_so3(rotation_vector, epsilon=1.0e-16),
    geometry.inverse_left_jacobian_of_so3(rotation_vector, epsilon=1.0e-16),
)

constructed_vectors: tuple[sym.MatrixExpr, ...] = (
    sym.vector(x, y),
    sym.vector(1, x, 2.5),
    sym.row_vector(x, y),
    sym.row_vector(1, x, 2.5),
)

matrix_shape: tuple[int, int] = constructed_vectors[0].shape
matrix_rows: list[list[sym.Expr]] = constructed_vectors[0].to_list()
matrix_from_rows = sym.matrix([[x, 1], [2.5, y]])

expr_args: tuple[sym.Expr | sym.BooleanExpr, ...] = (x + y).args
boolean_args: tuple[sym.Expr | sym.BooleanExpr, ...] = (x > y).args

package_version: str = pywrenfold.__version__

symbols_from_names: list[sym.Expr] = sym.make_symbols("a", "b", "c")

bounded_expressions: tuple[sym.Expr, ...] = (
    sym.min(x, 1),
    sym.min(1.5, x),
    sym.max(x, 1),
    sym.max(1.5, x),
)

relational_expressions: tuple[sym.BooleanExpr, ...] = (
    sym.lt(x, 1),
    sym.le(1.5, x),
    sym.gt(x, 1),
    sym.ge(1.5, x),
    sym.eq(x, 1),
)

scalar_functions_with_literals: tuple[sym.Expr, ...] = (
    sym.log(2),
    sym.exp(1.5),
    sym.pow(x, 2),
    sym.cos(0),
    sym.sin(0.5),
    sym.atan2(y, 1),
    sym.floor(1.5),
    sym.where(x > 0, x, 0),
    sym.unevaluated(1),
    sym.stop_derivative(1.5),
    sym.addition([x, 1, 2.5]),
    sym.multiplication([x, 2]),
    sym.substitution(x, x, 0),
    sym.Function("f")(x, 1, 2.5),
)

substituted_expressions: tuple[sym.Expr | sym.MatrixExpr | sym.BooleanExpr, ...] = (
    x.subs(x, 0),
    (x > y).subs(x, 0),
    sym.vector(x, y).subs(x, 0),
    x.subs([(x, 0)]),
)

quaternions_with_literals: tuple[geometry.Quaternion, ...] = (
    geometry.Quaternion(1, 0, 0, 0),
    geometry.Quaternion.from_xyzw([0, 0, 0, 1]),
    geometry.Quaternion.from_wxyz([1, 0, 0, 0]),
    geometry.Quaternion.from_angle_axis(0, 1, 0, 0),
    geometry.Quaternion.from_x_angle(0.5),
)

custom_type = type_info.CustomType(
    "point", [("x", type_info.ScalarType(type_info.NumericType.Float))], object
)
external_function = gen.PyExternalFunction(
    "external", [("x", type_info.ScalarType(type_info.NumericType.Float))], custom_type
)
external_result: sym.Expr | sym.MatrixExpr | sym.CompoundExpr | sym.BooleanExpr = (
    external_function.call([x])
)

distributed_expr: sym.Expr = sym.distribute(x * (x + y))
distributed_matrix: sym.MatrixExpr = sym.distribute(sym.vector(x, y))
substituted_expr: sym.Expr = sym.subs(x + y, x, 0)
substituted_matrix: sym.MatrixExpr = sym.subs(sym.vector(x, y), x, 0)
