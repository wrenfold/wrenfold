// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/complex.h>
#include <pybind11/functional.h>
#include <pybind11/numpy.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/expression.h"
#include "wf/expressions/matrix.h"
#include "wf/expressions/numeric_expressions.h"
#include "wf/functions.h"
#include "wf/matrix_functions.h"
#include "wf/numerical_casts.h"

#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace wf {

// Stores slice indices and provides a method for mapping from flat indices to sparse ones.
struct slice {
 public:
  explicit slice(const index_t len, const py::slice& slice) {
    if (!slice.compute(static_cast<py::ssize_t>(len), &start_, &stop_, &step_, &length_)) {
      throw py::error_already_set();
    }
  }

  // Number of iterations in the slice.
  constexpr index_t length() const noexcept { return static_cast<index_t>(length_); }

  // Convert flat index to modified index.
  constexpr index_t map_index(index_t i) const noexcept {
    return static_cast<index_t>(start_ + i * step_);
  }

 private:
  py::ssize_t start_{0};
  py::ssize_t stop_{0};
  py::ssize_t step_{0};
  py::ssize_t length_{0};
};

// Iterator over rows of a matrix expression.
struct row_iterator {
  static row_iterator begin(const matrix_expr& m) { return row_iterator(m, 0); }
  static row_iterator end(const matrix_expr& m) { return row_iterator(m, m.rows()); }

  // Iterators only match if the underlying matrix expression is the same instance.
  bool operator==(const row_iterator& other) const {
    return parent_.has_same_address(other.parent_) && row_ == other.row_;
  }

  // Pre-increment. We don't need post-increment for pybind.
  row_iterator& operator++() {
    row_++;
    return *this;
  }

  // De-reference. Behaves like doing `m[row]`.
  std::variant<scalar_expr, matrix_expr> operator*() const {
    if (parent_.cols() == 1) {
      return parent_[row_];
    } else {
      return parent_.get_block(row_, 0, 1, parent_.cols());
    }
  }

 private:
  // We store a strong reference to the parent, thereby ensuring this iterator is always valid.
  matrix_expr parent_;
  index_t row_;

  row_iterator(matrix_expr parent, const index_t row) : parent_(std::move(parent)), row_(row) {}
};

// Access a particular row and column (with support for negative indexing).
scalar_expr matrix_get_row_and_col(const matrix_expr& self,
                                   const std::tuple<index_t, index_t>& row_col) {
  auto [row, col] = row_col;
  return self(row < 0 ? (self.rows() + row) : row, col < 0 ? (self.cols() + col) : col);
}

// Return variant because this could be a single expression, or a matrix expression.
std::variant<scalar_expr, matrix_expr> matrix_get_row(const matrix_expr& self, const index_t row) {
  if (self.cols() == 1) {
    // Vectors convert to scalar automatically (don't form 1x1 matrix).
    return self[row < 0 ? (self.rows() + row) : row];
  } else {
    return self.get_block(row < 0 ? (self.rows() + row) : row, 0, 1, self.cols());
  }
}

// Return a sub-block by slicing just rows.
matrix_expr matrix_get_row_slice(const matrix_expr& self, py::slice slc) {
  const slice slice_index{self.rows(), slc};

  std::vector<scalar_expr> elements;
  elements.reserve(static_cast<std::size_t>(slice_index.length() * self.cols()));

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < slice_index.length(); ++i) {
    for (index_t j = 0; j < self.cols(); ++j) {
      elements.push_back(self(slice_index.map_index(i), j));
    }
  }
  return matrix_expr::create(slice_index.length(), self.cols(), std::move(elements));
}

// Return a sub-block by slicing both rows and cols.
matrix_expr matrix_get_row_and_col_slice(const matrix_expr& self,
                                         const std::tuple<py::slice, py::slice>& slices) {
  const auto [row_slice, col_slice] = slices;
  const slice row_index{self.rows(), row_slice};
  const slice col_index{self.cols(), col_slice};
  const std::size_t num_elements =
      static_cast<std::size_t>(row_index.length()) * static_cast<std::size_t>(col_index.length());

  std::vector<scalar_expr> elements;
  elements.reserve(num_elements);

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < row_index.length(); ++i) {
    for (index_t j = 0; j < col_index.length(); ++j) {
      elements.push_back(self(row_index.map_index(i), col_index.map_index(j)));
    }
  }
  return matrix_expr::create(row_index.length(), col_index.length(), std::move(elements));
}

// Return a given row index, and slice along columns.
matrix_expr matrix_get_row_index_and_col_slice(
    const matrix_expr& self, const std::tuple<index_t, py::slice>& row_and_col_slice) {
  const auto [row, col_slice] = row_and_col_slice;
  const index_t wrapped_row = row < 0 ? self.rows() - row : row;
  const slice col_index{self.cols(), col_slice};

  std::vector<scalar_expr> elements;
  elements.reserve(static_cast<std::size_t>(col_index.length()));
  for (index_t j = 0; j < col_index.length(); ++j) {
    elements.push_back(self(wrapped_row, col_index.map_index(j)));
  }
  return matrix_expr::create(1, col_index.length(), std::move(elements));
}

// Return a given row slice, and pick a particular index of column.
matrix_expr matrix_get_row_slice_and_col_index(
    const matrix_expr& self, const std::tuple<py::slice, index_t>& row_slice_and_col) {
  const auto [row_slice, col] = row_slice_and_col;
  const index_t wrapped_col = col < 0 ? self.cols() - col : col;
  const slice row_index{self.rows(), row_slice};

  std::vector<scalar_expr> elements;
  elements.reserve(static_cast<std::size_t>(row_index.length()));
  for (index_t i = 0; i < row_index.length(); ++i) {
    elements.push_back(self(row_index.map_index(i), wrapped_col));
  }
  return matrix_expr::create(row_index.length(), 1, std::move(elements));
}

// Set a particular row and column (with support for negative indexing).
void matrix_set_row_and_col(matrix_expr& self, const std::tuple<index_t, index_t>& row_col,
                            const scalar_expr& other) {
  auto [row, col] = row_col;
  self.set(row < 0 ? (self.rows() + row) : row, col < 0 ? (self.cols() + col) : col, other);
}

// Set an entire row.
void matrix_set_row(matrix_expr& self, const index_t row, const matrix_expr& other) {
  self.set_block(row < 0 ? (self.rows() + row) : row, 0, 1, self.cols(), other);
}

// Set a sub-block by slicing just rows.
void matrix_set_row_slice(matrix_expr& self, py::slice slc, const matrix_expr& other) {
  const slice slice_index{self.rows(), slc};

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < slice_index.length(); ++i) {
    for (index_t j = 0; j < self.cols(); ++j) {
      self.set(slice_index.map_index(i), j, other(i, j));
    }
  }
}

// Set a sub-block by slicing both rows and cols.
void matrix_set_row_and_col_slice(matrix_expr& self, const std::tuple<py::slice, py::slice>& slices,
                                  const matrix_expr& other) {
  const auto [row_slice, col_slice] = slices;
  const slice row_index{self.rows(), row_slice};
  const slice col_index{self.cols(), col_slice};

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < row_index.length(); ++i) {
    for (index_t j = 0; j < col_index.length(); ++j) {
      self.set(row_index.map_index(i), col_index.map_index(j), other(i, j));
    }
  }
}

// Set a given row index, and slice along columns.
void matrix_set_row_index_and_col_slice(matrix_expr& self,
                                        const std::tuple<index_t, py::slice>& row_and_col_slice,
                                        const matrix_expr& other) {
  const auto [row, col_slice] = row_and_col_slice;
  const index_t wrapped_row = row < 0 ? self.rows() - row : row;
  const slice col_index{self.cols(), col_slice};

  for (index_t j = 0; j < col_index.length(); ++j) {
    self.set(wrapped_row, col_index.map_index(j), other(0, j));
  }
}

// Set a given row slice, and pick a particular index of column.
void matrix_set_row_slice_and_col_index(matrix_expr& self,
                                        const std::tuple<py::slice, index_t>& row_slice_and_col,
                                        const matrix_expr& other) {
  const auto [row_slice, col] = row_slice_and_col;
  const index_t wrapped_col = col < 0 ? self.cols() - col : col;
  const slice row_index{self.rows(), row_slice};

  for (index_t i = 0; i < row_index.length(); ++i) {
    self.set(row_index.map_index(i), wrapped_col, other(i, 0));
  }
}

// Convert a container of scalar_expr objects to a column vector.
template <typename Container>
matrix_expr column_vector_from_container(const Container& inputs) {
  std::vector<scalar_expr> converted;
  cast_to_expr(inputs, converted);
  if (converted.empty()) {
    throw dimension_error("Cannot construct empty vector.");
  }
  const index_t rows = static_cast<index_t>(converted.size());
  return matrix_expr::create(rows, 1, std::move(converted));
}

// Convert a container of scalar_expr objects to a row vector.
template <typename Container>
matrix_expr row_vector_from_container(const Container& inputs) {
  std::vector<scalar_expr> converted;
  cast_to_expr(inputs, converted);
  if (converted.empty()) {
    throw dimension_error("Cannot construct empty row vector.");
  }
  const index_t cols = static_cast<index_t>(converted.size());
  return matrix_expr::create(1, cols, std::move(converted));
}

inline std::size_t extract_iterable_rows(const py::handle& row, std::vector<scalar_expr>& output) {
  if (py::isinstance<matrix_expr>(row)) {
    const matrix_expr as_matrix = py::cast<matrix_expr>(row);
    // If the "row" is a matrix, we stack them vertically:
    const auto num_elements = as_matrix.cols() * as_matrix.rows();
    output.reserve(output.size() + static_cast<std::size_t>(num_elements));
    std::copy(as_matrix.as_matrix().begin(), as_matrix.as_matrix().end(),
              std::back_inserter(output));
    return as_matrix.cols();
  }
  return cast_to_expr(py::iter(row), output);
}

// Vertically stack a bunch of iterable objects into one big matrix.
inline matrix_expr stack_iterables(const std::vector<py::object>& rows) {
  std::vector<scalar_expr> converted{};

  // Transform the first row to get the # of columns.
  auto it = rows.begin();
  const std::size_t expected_num_cols = extract_iterable_rows(*it, converted);
  converted.reserve(rows.size() * expected_num_cols);

  // All other elements must match.
  for (++it; it != rows.end(); ++it) {
    const std::size_t num_cols = extract_iterable_rows(*it, converted);
    if (num_cols != expected_num_cols) {
      throw dimension_error(
          "Mismatch in number of provided columns. First input had {} columns, but input [{}] has "
          "{} elements.",
          expected_num_cols, std::distance(rows.begin(), it), num_cols);
    }
  }

  // Figure out how many rows we extracted:
  WF_ASSERT_EQUAL(0, converted.size() % expected_num_cols);
  const auto total_rows = static_cast<index_t>(converted.size() / expected_num_cols);
  return matrix_expr::create(total_rows, static_cast<index_t>(expected_num_cols),
                             std::move(converted));
}

// Create matrix from iterable. When the input is an iterable over iterables, we stack
// them as rows.
matrix_expr matrix_from_iterable(py::iterable rows) {
  // The input could be a generator, in which case we can only iterate over it once.
  // Calling `begin` more than once does not yield the same position. We have to create
  // py::object here so that the reference count is properly incremented, otherwise the
  // results yielded from the generator will be discarded.
  std::vector<py::object> extracted;
  extracted.reserve(py::len_hint(rows));
  std::transform(rows.begin(), rows.end(), std::back_inserter(extracted),
                 [](const py::handle& handle) { return handle.cast<py::object>(); });

  if (extracted.empty()) {
    throw dimension_error("Cannot construct matrix from empty iterator.");
  }

  // Try to cast the first row to an iterable. If it works, interpret this as an iterable over
  // iterables (rows and columns).
  if (py::isinstance<py::iterable>(extracted[0])) {
    return stack_iterables(extracted);
  }
  // If the argument is not iterable, interpret it as a list of expressions.
  return column_vector_from_container(extracted);
}

// Perform element-wise map operation on a matrix.
matrix_expr unary_map_matrix(const matrix_expr& self,
                             const std::function<scalar_expr(scalar_expr)>& func) {
  return matrix_expr::create(self.rows(), self.cols(),
                             transform_map<std::vector<scalar_expr>>(self.as_matrix(), func));
}

// Convert `matrix_expr` to a nested list.
py::list list_from_matrix(const matrix_expr& self) {
  py::list rows{};
  for (index_t i = 0; i < self.rows(); ++i) {
    py::list cols{};
    for (index_t j = 0; j < self.cols(); ++j) {
      cols.append(self(i, j));
    }
    rows.append(std::move(cols));
  }
  return rows;
}

py::list flat_list_from_matrix(const matrix_expr& self) {
  py::list output{};
  for (const scalar_expr& element : self.as_matrix()) {
    output.append(element);
  }
  return output;
}

// Convert matrix to numpy array.
py::array numpy_from_matrix(const matrix_expr& self) {
  auto list = py::list();  // TODO: Don't copy into list.
  for (const scalar_expr& expr : self.as_matrix()) {
    list.append(maybe_numerical_cast(expr));
  }
  auto array = py::array(list);
  const std::array<std::size_t, 2> new_shape{static_cast<std::size_t>(self.rows()),
                                             static_cast<std::size_t>(self.cols())};
  array.resize(new_shape);
  return array;
}

void wrap_matrix_operations(py::module_& m) {
  // Matrix expression type.
  wrap_class<matrix_expr>(m, "MatrixExpr")
      // scalar_expr inherited properties:
      .def("__repr__", &matrix_expr::to_string)
      .def("expression_tree_str", &matrix_expr::to_expression_tree_string,
           "Retrieve the expression tree as a pretty-printed string.")
      .def_property_readonly("type_name", [](const matrix_expr& self) { return self.type_name(); })
      // Operations:
      .def(
          "diff",
          [](const matrix_expr& self, const scalar_expr& var, int order, bool use_abstract) {
            return self.diff(var, order,
                             use_abstract ? non_differentiable_behavior::abstract
                                          : non_differentiable_behavior::constant);
          },
          "var"_a, py::arg("order") = 1, py::arg("use_abstract") = false,
          "Differentiate the expression with respect to the specified variable.")
      .def(
          "jacobian",
          [](const matrix_expr& self, const matrix_expr& vars, bool use_abstract) {
            return self.jacobian(vars, use_abstract ? non_differentiable_behavior::abstract
                                                    : non_differentiable_behavior::constant);
          },
          "vars"_a, py::arg("use_abstract") = false,
          "Compute the jacobian of a vector-valued function with respect to vector of arguments.")
      .def(
          "jacobian",
          [](const matrix_expr& self, const std::vector<scalar_expr>& vars, bool use_abstract) {
            return self.jacobian(vars, use_abstract ? non_differentiable_behavior::abstract
                                                    : non_differentiable_behavior::constant);
          },
          "vars"_a, py::arg("use_abstract") = false,
          "Compute the jacobian of a vector-valued function with respect to a list of arguments.")
      .def("distribute", &matrix_expr::distribute, "Expand products of additions and subtractions.")
      .def("subs", &matrix_expr::subs, py::arg("target"), py::arg("substitute"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def(
          "eval",
          [](const matrix_expr& self) {
            matrix_expr eval = self.eval();
            return numpy_from_matrix(eval);
          },
          "Evaluate into float expression.")
      .def(
          "collect",
          [](const matrix_expr& self, const scalar_expr& var) { return self.collect({var}); },
          "var"_a, "Collect powers of the provided expression.")
      .def(
          "collect",
          [](const matrix_expr& self, const std::vector<scalar_expr>& vars) {
            return self.collect(vars);
          },
          "var"_a, "Collect powers of the provided expressions.")
      // Matrix specific properties:
      .def_property_readonly(
          "shape", [](const matrix_expr& m) { return py::make_tuple(m.rows(), m.cols()); },
          "Shape of the matrix in (row, col) format.")
      .def_property_readonly("size", &matrix_expr::size, "Total number of elements.")
      .def_property_readonly(
          "is_empty", [](const matrix_expr& m) { return m.rows() == 0 || m.cols() == 0; },
          "Is the matrix empty (either zero rows or cols).")
      // Slicing:
      .def("__getitem__", &matrix_get_row, py::arg("row"), "Retrieve a row from the matrix.")
      .def("__getitem__", &matrix_get_row_and_col, py::arg("row_col"),
           "Retrieve a row and column from the matrix.")
      .def("__getitem__", &matrix_get_row_slice, py::arg("slice"), "Slice along rows.")
      .def("__getitem__", &matrix_get_row_and_col_slice, py::arg("slices"),
           "Slice along rows and cols.")
      .def("__getitem__", &matrix_get_row_index_and_col_slice, py::arg("row_and_col_slice"),
           "Slice a specific row.")
      .def("__getitem__", &matrix_get_row_slice_and_col_index, py::arg("row_slice_and_col"),
           "Slice a specific column.")
      .def("__setitem__", &matrix_set_row, py::arg("row"), py::arg("other"),
           "Set a row from the matrix.")
      .def("__setitem__", &matrix_set_row_and_col, py::arg("row_col"), py::arg("other"),
           "Set a row and column from the matrix.")
      .def("__setitem__", &matrix_set_row_slice, py::arg("slice"), py::arg("other"),
           "Set a slice along rows.")
      .def("__setitem__", &matrix_set_row_and_col_slice, py::arg("slices"), py::arg("other"),
           "Set a slice along rows and cols.")
      .def("__setitem__", &matrix_set_row_index_and_col_slice, py::arg("row_and_col_slice"),
           py::arg("other"), "Set a slice on a specific row.")
      .def("__setitem__", &matrix_set_row_slice_and_col_index, py::arg("row_slice_and_col"),
           py::arg("other"), "Set a slice on a specific column.")
      // Support conversion to numpy.
      .def("__array__", &numpy_from_matrix, "Convert to numpy array.")
      // Iterable:
      .def("__len__", &matrix_expr::rows, "Number of rows in the matrix.")
      .def("__iter__",  //  We don't need keep_alive since matrix_expr does that for us.
           [](const matrix_expr& expr) {
             return py::make_iterator(row_iterator::begin(expr), row_iterator::end(expr));
           })
      .def("unary_map", &unary_map_matrix, py::arg("func"), "Perform element-wise map operation.")
      .def("reshape", &matrix_expr::reshape, py::arg("rows"), py::arg("cols"),
           "Reshape a matrix while preserving the number of elements. Returns a copy.")
      .def(
          "reshape",
          [](const matrix_expr& self, std::tuple<index_t, index_t> row_and_col) {
            return self.reshape(std::get<0>(row_and_col), std::get<1>(row_and_col));
          },
          py::arg("shape"),
          py::doc("Reshape a matrix while preserving the number of elements. Returns a copy."))
      // Convert to list
      .def("to_list", &list_from_matrix, "Convert to list of lists.")
      .def("to_flat_list", &flat_list_from_matrix,
           py::doc("Convert to a flat list assembled in row major order."))
      .def("transpose", &matrix_expr::transposed, "Transpose the matrix.")
      .def_property_readonly("T", &matrix_expr::transposed, "Transpose the matrix.")
      .def("squared_norm", &matrix_expr::squared_norm, "Get the squared L2 norm of the matrix.")
      .def("norm", &matrix_expr::norm, "Get the L2 norm of the matrix.")
      .def("det", &determinant, "Compute determinant of the matrix.")
      // Operators:
      .def("__add__",
           static_cast<matrix_expr (*)(const matrix_expr&, const matrix_expr&)>(&operator+),
           py::is_operator())
      .def("__sub__",
           static_cast<matrix_expr (*)(const matrix_expr&, const matrix_expr&)>(&operator-),
           py::is_operator())
      .def("__mul__",
           static_cast<matrix_expr (*)(const matrix_expr&, const matrix_expr&)>(&operator*),
           py::is_operator())
      // Right-multiply by scalar:
      .def("__mul__",
           static_cast<matrix_expr (*)(const matrix_expr&, const scalar_expr&)>(&operator*),
           py::is_operator())
      // Left multiply by scalar:
      .def("__rmul__",
           static_cast<matrix_expr (*)(const scalar_expr&, const matrix_expr&)>(&operator*),
           py::is_operator())
      .def("__neg__", &matrix_expr::operator-, "Element-wise negation of the matrix.")
      // Prohibit conversion to bool.
      .def("__bool__", [](const matrix_expr&) {
        throw type_error("matrix_expr cannot be coerced to boolean.");
      });

  // Matrix constructors:
  m.def("identity", &make_identity, "rows"_a, "Create identity matrix.");
  m.def("eye", &make_identity, "rows"_a, "Create identity matrix (alias for identity).");
  m.def("zeros", &make_zeros, "rows"_a, "cols"_a, "Create a matrix of zeros.");
  m.def("vector", &column_vector_from_container<py::args>,
        "Construct a column vector from the arguments.");
  m.def("row_vector", &row_vector_from_container<py::args>,
        "Construct a row vector from the arguments.");
  m.def("matrix", &matrix_from_iterable, py::arg("rows"),
        "Construct a matrix from an iterator over rows.");
  m.def("matrix_of_symbols", &make_matrix_of_symbols, py::arg("prefix"), py::arg("rows"),
        py::arg("cols"), "Construct a matrix of symbols.");

  m.def(
      "hstack", [](const std::vector<matrix_expr>& values) { return hstack(values); },
      py::arg("values"), py::doc("Horizontally stack matrices."));
  m.def(
      "vstack", [](const std::vector<matrix_expr>& values) { return vstack(values); },
      py::arg("values"), py::doc("Vertically stack matrices."));
  m.def(
      "diagonal", [](const std::vector<matrix_expr>& values) { return diagonal_stack(values); },
      py::arg("values"),
      py::doc("Diagonally stack matrix blocks. Fill off-diagonal blocks with zeros"));

  m.def("vec", &vectorize_matrix, py::arg("m"), "Vectorize matrix in column-major order.");
  m.def("det", &determinant, py::arg("m"), "Compute determinant of a matrix.");
  m.def("full_piv_lu", &factorize_full_piv_lu, py::arg("m"),
        "Factorize a matrix using fully-pivoting LU decomposition.");

  // Version of where() for matrices
  m.def("where",
        static_cast<matrix_expr (*)(const boolean_expr&, const matrix_expr&, const matrix_expr&)>(
            &wf::where),
        "condition"_a, "if_true"_a, "if_false"_a, "If-else statement with matrix operands.");

  // Jacobian of a list of expressions wrt another list of expressions.
  m.def(
      "jacobian",
      [](const std::vector<scalar_expr>& functions, const std::vector<scalar_expr>& arguments,
         const bool use_abstract) {
        return jacobian({functions}, {arguments},
                        use_abstract ? non_differentiable_behavior::abstract
                                     : non_differentiable_behavior::constant);
      },
      "functions"_a, "arguments"_a, py::arg("use_abstract") = false,
      py::doc("Compute the NxM jacobian of `N` functions with respect to `M` arguments."));
}

}  // namespace wf
