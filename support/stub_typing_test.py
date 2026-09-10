"""Representative type-check-only tests for the generated extension stubs."""

from pywrenfold import geometry, sym

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
