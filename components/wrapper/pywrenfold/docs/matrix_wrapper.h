// Copyright 2024 Gareth Cross
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view matrix_expr_diff = R"doc(
Differentiate every element of the matrix with respect to the specified variable.

See :func:`wrenfold.sym.Expr.diff` for meaning of the arguments.

Returns:
  A new matrix (with the same shape as ``self``) where each element is the derivative of the
  corresponding input element, taken ``order`` times with respect to ``var``.

Examples:
  >>> x = sym.symbols('x')
  >>> m = sym.matrix([[x, sym.sin(x)], [22, -x**2]])
  >>> m.diff(x)
  [[1, cos(x)], [0, -2*x]]
)doc";

inline constexpr std::string_view matrix_expr_unary_map = R"doc(
Perform a unary map operation on ``self``. The provided callable ``func`` is invoked on every
element of the matrix (traversing in row-major order). The returned values are used to create a new
matrix with the same shape as the input.

Args:
  func: A callable that accepts and returns ``sym.Expr``.

Returns:
  The mapped matrix.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> m = sym.matrix([[x, y], [2, sym.pi]])
  >>> m.unary_map(lambda v: v * 2)
  [[2*x, 2*y], [4, 2*pi]]
)doc";

inline constexpr std::string_view matrix_expr_reshape = R"doc(
Reshape a matrix and return a copy with the new shape. The underlying order (row-major) of elements
in ``self`` is unchanged. The expressions are simply copied into a new matrix with the requested
number of rows and columns. The total number of elements must remain unchanged.

Args:
  rows: Number of rows in the output.
  cols: Number of columns in the output.

Returns:
  A new matrix with shape ``(rows, cols)``.

Raises:
  wrenfold.sym.DimensionError: If the specified dimensions are invalid.

Examples:
  >>> sym.matrix([1, 2, 3, 4]).reshape(2, 2)
  [[1, 2], [3, 4]]
)doc";

inline constexpr std::string_view matrix_expr_transpose = R"doc(
Transpose the matrix by swapping rows and columns. If ``self`` has dimensions ``(N, M)``, the result
will be an ``(M, N)`` matrix where element ``[i, j]`` is ``self[j, i]``.

Returns:
  The transposed matrix.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> m = sym.matrix([[x, y*2], [sym.cos(x), 0]])
  >>> m
  [[x, 2*y], [cos(x), 0]]
  >>> m.transpose()
  [[x, cos(x)], [2*y, 0]]
)doc";

inline constexpr std::string_view matrix_expr_squared_norm = R"doc(
The sum of squared elements of ``self``, or the squared L2 norm.

Returns:
  A scalar-valued expression.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> m = sym.matrix([[x, y], [2, 0]])
  >>> m.squared_norm()
  4 + x**2 + y**2
)doc";

inline constexpr std::string_view det = R"doc(
Compute the determinant of the matrix. For 2x2 and 3x3 matrices, a hardcoded formula is employed.
For sizes 4x4 and above, the matrix is first decomposed via full-pivoting LU decomposition.

Caution:
  When computing the LU decomposition, the pivot selection step cannot know apriori which symbolic
  expressions *might* evaluate to zero at runtime. As a result, the decomposition ordering could
  produce ``NaN`` values at runtime when numerical values are substituted into this expression.

Returns:
  The matrix determinant as a scalar-valued expression.

Raises:
  wrenfold.sym.DimensionError: If the matrix is not square.

Examples:
  >>> x = sym.symbols('x')
  >>> m = sym.matrix([[sym.cos(x), -sym.sin(x)], [sym.sin(x), sym.cos(x)]])
  >>> m.det()
  cos(x)**2 + sin(x)**2

References:
  * `Matrix determinant <https://en.wikipedia.org/wiki/Determinant>`_.
)doc";

inline constexpr std::string_view identity = R"doc(
Create an identity matrix.

Args:
  rows: Number of rows and columns in the resulting matrix.

Raises:
  wrenfold.sym.DimensionError: If the dimensions are non-positive.
)doc";

inline constexpr std::string_view zeroes = R"doc(
Create a matrix of zeros with the specified shape.

Args:
  rows: Number of rows. Must be positive.
  cols: Number of cols. Must be positive.

Returns:
  A ``(rows, cols)`` matrix of zeroes.

Raises:
  wrenfold.sym.DimensionError: If the dimensions are non-positive.

Examples:
  >>> sym.zeros(2, 3)
  [[0, 0, 0], [0, 0, 0]]
)doc";

inline constexpr std::string_view vector = R"doc(
Create a column-vector from the provided arguments.

Args:
  args: The elements of the vector.

Returns:
  A ``(N, 1)`` column vector, where ``N`` is the number of args.

Raises:
  wrenfold.sym.DimensionError: If no arguments are provided.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> sym.vector(x * 2, 0, y + 3)
  [[2*x], [0], [3 + y]]
)doc";

inline constexpr std::string_view row_vector = R"doc(
Create a row-vector from the provided arguments.

Args:
  args: The elements of the vector.

Returns:
  A ``(1, M)`` row vector, where ``M`` is the number of args.

Raises:
  wrenfold.sym.DimensionError: If no arguments are provided.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> sym.row_vector(x * 2, 0, y + 3)
  [[2*x, 0, 3 + y]]
)doc";

inline constexpr std::string_view matrix = R"doc(
Construct a matrix from an iterator over rows. Inputs are vertically concatenated to create a matrix
expression.

Args:
  rows: Either ``Iterable[sym.Expr]`` or ``Iterable[Iterable[sym.Expr]]``.

Tip:
  If the input is an iterable of expressions, the result will have column-vector shape.

Returns:
  * A matrix of expressions.

Raises:
  wrenfold.sym.DimensionError: If the input is empty, or the number of columns is inconsistent
    between rows.

Examples:
  >>> sym.matrix([1,2,3])
  [[1], [2], [3]]
  >>> sym.matrix([(1,2,3), (x, y, z)])
  [[1, 2, 3], [x, y, z]]
  >>> def yield_rows():
  >>>   yield sym.matrix([sym.pi, x, 0]).T
  >>>   yield sym.matrix([-2, y**2, sym.cos(x)]).T
  >>> sym.matrix(yield-rows())
  [[pi, x, 0], [-2, y**2, cos(x)]]
)doc";

inline constexpr std::string_view matrix_of_symbols = R"doc(
Construct a matrix of ``variable`` expressions with the provided prefix.

Args:
  prefix: String prefix for variable names. Variables will be named ``{prefix}_{row}_{col}``.
  rows: Number of rows.
  cols: Number of cols.

Returns:
  A ``(rows, cols)`` shaped matrix filled with ``variable`` expressions.

Raises:
  wrenfold.sym.DimensionError: If the dimensions are non-positive.
)doc";

inline constexpr std::string_view hstack = R"doc(
Horizontally (joining rows) stack the provided matrices. Number of rows must be the same for every
input.

Raises:
  wrenfold.sym.DimensionError: If there is a dimension mismatch, or the input is empty.
)doc";

inline constexpr std::string_view vstack = R"doc(
Vertically (joining columns) stack the provided matrices. Number of columns must be the same for
every input.

Raises:
  wrenfold.sym.DimensionError: If there is a dimension mismatch, or the input is empty.
)doc";

inline constexpr std::string_view diag = R"doc(
Diagonally stack the provided matrices. Off-diagonal blocks are filled with zeroes.

Raises:
  wrenfold.sym.DimensionError: If the input is empty.

Examples:
  >>> x, y, z = sym.symbols('x, y, z')
  >>> m = sym.matrix([[x, 0], [y*z, 5]])
  >>> sym.diagonal([m, sym.eye(2)])
  [[x, 0, 0, 0], [y*z, 5, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
)doc";

inline constexpr std::string_view vec = R"doc(
Vectorize matrix in column-major order.

Args:
  m: Matrix to flatten.

Returns:
  New column-vector matrix formed by vertically stacking the columns of ``m``.
)doc";

inline constexpr std::string_view matrix_where = R"doc(
Overload of :func:`wrenfold.sym.where` for matrix arguments. Accepts two identically-shaped matrices
``a, b`` and a boolean-valued expression ``c``.

Args:
  c: Boolean-valued expression.
  a: Matrix whose elements are selected when ``c`` is True.
  b: Matrix whose elements are selected when ``c`` is False.

Returns:
  A new matrix where element ``[i, j]`` evaluates to ``sym.where(c, a[i, j], b[i, j])``.

Raises:
  wrenfold.sym.DimensionError: If the dimensions of ``a`` and ``b`` do not match.
)doc";

inline constexpr std::string_view jacobian = R"doc(
Compute the Jacobian of a vector-valued function with respect to multiple variables.

Given ``functions`` of length ``N`` and ``vars`` of length ``M``, this method produces an ``(N, M)``
shaped matrix where element ``[i, j]`` is the partial derivative of ``functions[i]`` taken with
respect to ``vars[j]``.

Args:
  functions: An iterable of scalar-valued expressions to differentiate.
  vars: An iterable of ``variable`` or ``compound_expression_element`` expressions.
  use_abstract: See :func:`wrenfold.sym.Expr.diff`.

Returns:
  The Jacobian matrix of shape ``(N, M)``.

Raises:
  sym.DimensionError: If the dimensions of ``functions`` or ``vars`` are invalid.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> J = sym.jacobian([x + 1, sym.sin(y*x), x*y**2], [x, y])
  >>> J.shape
  (3, 2)
  >>> J
  [[1, 0], [y*cos(x*y), x*cos(x*y)], [y**2, 2*x*y]]

References:
  * `Jacobian matrix <https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant>`_
)doc";

}  // namespace wf::docstrings
