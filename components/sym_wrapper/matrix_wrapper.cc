// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/functional.h>
#include <pybind11/numpy.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "expression.h"
#include "expressions/matrix.h"
#include "expressions/numeric_expressions.h"
#include "functions.h"
#include "matrix_functions.h"
#include "plain_formatter.h"
#include "wrapper_utils.h"

namespace py = pybind11;
using namespace py::literals;

namespace math {

// Stores slice indices and provides a method for mapping from flat indices to sparse ones.
struct Slice {
 public:
  explicit Slice(const index_t len, const py::slice& slice) {
    if (!slice.compute(static_cast<py::ssize_t>(len), &start_, &stop_, &step_, &length_)) {
      throw py::error_already_set();
    }
  }

  // Number of iterations in the slice.
  index_t length() const { return static_cast<index_t>(length_); }

  // Convert flat index to modified index.
  index_t map_index(index_t i) const { return static_cast<index_t>(start_ + i * step_); }

 private:
  py::ssize_t start_{0};
  py::ssize_t stop_{0};
  py::ssize_t step_{0};
  py::ssize_t length_{0};
};

// Iterator over rows of a matrix expression.
struct RowIterator {
  static RowIterator Begin(const MatrixExpr& m) { return RowIterator(m, 0); }
  static RowIterator End(const MatrixExpr& m) { return RowIterator(m, m.rows()); }

  // Iterators only match if the underlying matrix expression is the same instance.
  bool operator==(const RowIterator& other) const {
    return parent_.has_same_address(other.parent_) && row_ == other.row_;
  }

  // Pre-increment. We don't need post-increment for pybind.
  RowIterator& operator++() {
    row_++;
    return *this;
  }

  // De-reference. Behaves like doing `m[row]`.
  std::variant<Expr, MatrixExpr> operator*() const {
    if (parent_.cols() == 1) {
      return parent_[row_];
    } else {
      return parent_.get_block(row_, 0, 1, parent_.cols());
    }
  }

 private:
  // We store a strong reference to the parent, thereby ensuring this iterator is always valid.
  math::MatrixExpr parent_;
  math::index_t row_;

  RowIterator(MatrixExpr parent, index_t row) : parent_(std::move(parent)), row_(row) {}
};

Expr MatrixGetRowCol(const MatrixExpr& self, const std::tuple<index_t, index_t>& row_col) {
  auto [row, col] = row_col;
  return self(row < 0 ? (self.rows() + row) : row, col < 0 ? (self.cols() + col) : col);
}

// Return variant because this could be a single expression, or a matrix expression.
std::variant<Expr, MatrixExpr> MatrixGetRow(const MatrixExpr& self, const index_t row) {
  if (self.cols() == 1) {
    // Vectors convert to scalar automatically (don't form 1x1 matrix).
    return self[row < 0 ? (self.rows() + row) : row];
  } else {
    return self.get_block(row < 0 ? (self.rows() + row) : row, 0, 1, self.cols());
  }
}

MatrixExpr MatrixGetRowSlice(const MatrixExpr& self, py::slice slice) {
  const Slice slice_index{self.rows(), slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(slice_index.length() * self.cols()));

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < slice_index.length(); ++i) {
    for (index_t j = 0; j < self.cols(); ++j) {
      elements.push_back(self(slice_index.map_index(i), j));
    }
  }
  return MatrixExpr::create(slice_index.length(), self.cols(), std::move(elements));
}

MatrixExpr MatrixGetRowColSlice(const MatrixExpr& self,
                                const std::tuple<py::slice, py::slice>& slices) {
  const auto [row_slice, col_slice] = slices;
  const Slice row_index{self.rows(), row_slice};
  const Slice col_index{self.cols(), col_slice};
  const std::size_t num_elements =
      static_cast<std::size_t>(row_index.length()) * static_cast<std::size_t>(col_index.length());

  std::vector<Expr> elements;
  elements.reserve(num_elements);

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < row_index.length(); ++i) {
    for (index_t j = 0; j < col_index.length(); ++j) {
      elements.push_back(self(row_index.map_index(i), col_index.map_index(j)));
    }
  }
  return MatrixExpr::create(row_index.length(), col_index.length(), std::move(elements));
}

MatrixExpr MatrixGetRowIndexColSlice(const MatrixExpr& self,
                                     const std::tuple<index_t, py::slice>& row_and_col_slice) {
  const auto [row, col_slice] = row_and_col_slice;
  const index_t wrapped_row = row < 0 ? self.rows() - row : row;
  const Slice col_index{self.cols(), col_slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(col_index.length()));
  for (index_t j = 0; j < col_index.length(); ++j) {
    elements.push_back(self(wrapped_row, col_index.map_index(j)));
  }
  return MatrixExpr::create(1, col_index.length(), std::move(elements));
}

MatrixExpr MatrixGetRowSliceColIndex(const MatrixExpr& self,
                                     const std::tuple<py::slice, index_t>& row_slice_and_col) {
  const auto [row_slice, col] = row_slice_and_col;
  const index_t wrapped_col = col < 0 ? self.cols() - col : col;
  const Slice row_index{self.rows(), row_slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(row_index.length()));
  for (index_t i = 0; i < row_index.length(); ++i) {
    elements.push_back(self(row_index.map_index(i), wrapped_col));
  }
  return MatrixExpr::create(row_index.length(), 1, std::move(elements));
}

template <typename Container>
MatrixExpr ColumnVectorFromContainer(const Container& inputs) {
  std::vector<Expr> converted;
  TransformIntoExpr(inputs, converted);
  if (converted.empty()) {
    throw DimensionError("Cannot construct empty vector.");
  }
  const index_t rows = static_cast<index_t>(converted.size());
  return MatrixExpr::create(rows, 1, std::move(converted));
}

template <typename Container>
MatrixExpr RowVectorFromContainer(const Container& inputs) {
  std::vector<Expr> converted;
  TransformIntoExpr(inputs, converted);
  if (converted.empty()) {
    throw DimensionError("Cannot construct empty row vector.");
  }
  const index_t cols = static_cast<index_t>(converted.size());
  return MatrixExpr::create(1, cols, std::move(converted));
}

inline std::size_t ExtractIterableRows(const py::handle& row, std::vector<Expr>& output) {
  if (py::isinstance<MatrixExpr>(row)) {
    const MatrixExpr as_matrix = py::cast<MatrixExpr>(row);
    // If the "row" is a matrix, we stack them vertically:
    const auto num_elements = as_matrix.cols() * as_matrix.rows();
    output.reserve(output.size() + static_cast<std::size_t>(num_elements));
    std::copy(as_matrix.as_matrix().begin(), as_matrix.as_matrix().end(), std::back_inserter(output));
    return as_matrix.cols();
  }
  return TransformIntoExpr(py::iter(row), output);
}

// Vertically stack a bunch of iterable objects into one big matrix.
inline MatrixExpr StackIterables(const std::vector<py::object>& rows) {
  std::vector<Expr> converted{};

  // Transform the first row to get the # of columns.
  auto it = rows.begin();
  const std::size_t expected_num_cols = ExtractIterableRows(*it, converted);
  converted.reserve(rows.size() * expected_num_cols);

  // All other elements must match.
  for (++it; it != rows.end(); ++it) {
    const std::size_t num_cols = ExtractIterableRows(*it, converted);
    if (num_cols != expected_num_cols) {
      throw DimensionError(
          "Mismatch in number of provided columns. First input had {} columns, but input [{}] has "
          "{} elements.",
          expected_num_cols, std::distance(rows.begin(), it), num_cols);
    }
  }

  // Figure out how many rows we extracted:
  ASSERT_EQUAL(0, converted.size() % expected_num_cols);
  const auto total_rows = static_cast<index_t>(converted.size() / expected_num_cols);
  return MatrixExpr::create(total_rows, static_cast<index_t>(expected_num_cols),
                            std::move(converted));
}

// Create matrix from iterable.
MatrixExpr MatrixFromIterable(py::iterable rows) {
  // The input could be a generator, in which case we can only iterate over it once.
  // Calling `begin` more than once does not yield the same position. We have to create
  // py::object here so that the reference count is properly incremented, otherwise the
  // results yielded from the generator will be discarded.
  std::vector<py::object> extracted;
  extracted.reserve(py::len_hint(rows));
  std::transform(rows.begin(), rows.end(), std::back_inserter(extracted),
                 [](const py::handle& handle) { return handle.cast<py::object>(); });

  if (extracted.empty()) {
    throw DimensionError("Cannot construct matrix from empty iterator.");
  }

  // Try to cast the first row to an iterable. If it works, interpret this as an iterable over
  // iterables (rows and columns).
  if (py::isinstance<py::iterable>(extracted[0])) {
    return StackIterables(extracted);
  }
  // If the argument is not iterable, interpret it as a list of expressions.
  return ColumnVectorFromContainer(extracted);
}

// Perform element-wise map operation on a matrix.
MatrixExpr MapMatrix(const MatrixExpr& self, const std::function<Expr(Expr)>& func) {
  std::vector<Expr> result{};
  result.reserve(self.size());
  const auto& m = self.as_matrix();
  std::transform(m.begin(), m.end(), std::back_inserter(result), func);
  return MatrixExpr::create(self.rows(), self.cols(), std::move(result));
}

py::list ListFromMatrix(const MatrixExpr& self) {
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

// Convert matrix to numpy array.
py::array NumpyFromMatrix(const MatrixExpr& self) {
  auto list = py::list();  // TODO: Don't copy into list.
  for (const Expr& expr : self.as_matrix()) {
    list.append(TryConvertToNumeric(expr));
  }
  auto array = py::array(list);
  const std::array<std::size_t, 2> new_shape{static_cast<std::size_t>(self.rows()),
                                             static_cast<std::size_t>(self.cols())};
  array.resize(new_shape);
  return array;
}

py::array EvalToNumeric(const MatrixExpr& self) {
  MatrixExpr eval = self.eval();
  return NumpyFromMatrix(eval);
}

void WrapMatrixOperations(py::module_& m) {
  // Matrix expression type.
  py::class_<MatrixExpr>(m, "MatrixExpr")
      // Expr inherited properties:
      .def("__repr__", &MatrixExpr::to_string)
      .def("expression_tree_str", &MatrixExpr::to_expression_tree_string,
           "Retrieve the expression tree as a pretty-printed string.")
      .def(
          "is_identical_to",
          [](const MatrixExpr& self, const MatrixExpr& other) { return self.is_identical_to(other); },
          "other"_a, "Test if two matrix expressions have identical expression trees.")
      .def_property_readonly("type_name", &MatrixExpr::type_name)
      // Operations:
      .def("diff", &MatrixExpr::diff, "var"_a, py::arg("order") = 1,
           "Differentiate the expression with respect to the specified variable.")
      .def("distribute", &MatrixExpr::distribute, "Expand products of additions and subtractions.")
      .def("subs", &MatrixExpr::subs, py::arg("target"), py::arg("substitute"),
           "Replace the `target` expression with `substitute` in the expression tree.")
      .def("eval", &EvalToNumeric, "Evaluate into float expression.")
      // Matrix specific properties:
      .def_property_readonly(
          "shape", [](const MatrixExpr& m) { return py::make_tuple(m.rows(), m.cols()); },
          "Shape of the matrix in (row, col) format.")
      .def_property_readonly("size", &MatrixExpr::size, "Total number of elements.")
      .def_property_readonly(
          "is_empty", [](const MatrixExpr& m) { return m.rows() == 0 || m.cols() == 0; },
          "Is the matrix empty (either zero rows or cols).")
      // Slicing:
      .def("__getitem__", &MatrixGetRow, py::arg("row"), "Retrieve a row from the matrix.")
      .def("__getitem__", &MatrixGetRowCol, py::arg("row_col"),
           "Retrieve a row and column from the matrix.")
      .def("__getitem__", &MatrixGetRowSlice, py::arg("slice"), "Slice along rows.")
      .def("__getitem__", &MatrixGetRowColSlice, py::arg("slices"), "Slice along rows and cols.")
      .def("__getitem__", &MatrixGetRowIndexColSlice, py::arg("row_and_col_slice"),
           "Slice a specific row.")
      .def("__getitem__", &MatrixGetRowSliceColIndex, py::arg("row_slice_and_col"),
           "Slice a specific column.")
      // Support conversion to numpy.
      .def("__array__", &NumpyFromMatrix, "Convert to numpy array.")
      // Iterable:
      .def("__len__", &MatrixExpr::rows, "Number of rows in the matrix.")
      .def("__iter__",  //  We don't need keep_alive since MatrixExpr does that for us.
           [](const MatrixExpr& expr) {
             return py::make_iterator(RowIterator::Begin(expr), RowIterator::End(expr));
           })
      .def("unary_map", &MapMatrix, py::arg("func"), "Perform element-wise map operation.")
      // Convert to list
      .def("to_list", &ListFromMatrix, "Convert to list of lists.")
      .def("transpose", &MatrixExpr::transposed, "Transpose the matrix.")
      .def_property_readonly("T", &MatrixExpr::transposed, "Transpose the matrix.")
      .def("det", &determinant, "Compute determinant of the matrix.")
      // Operators:
      .def("__add__",
           static_cast<MatrixExpr (*)(const MatrixExpr&, const MatrixExpr&)>(
               &matrix_operator_overloads::operator+),
           py::is_operator())
      .def("__sub__",
           static_cast<MatrixExpr (*)(const MatrixExpr&, const MatrixExpr&)>(
               &matrix_operator_overloads::operator-),
           py::is_operator())
      .def("__mul__",
           static_cast<MatrixExpr (*)(const MatrixExpr&, const MatrixExpr&)>(
               &matrix_operator_overloads::operator*),
           py::is_operator())
      // Right-multiply by scalar:
      .def("__mul__",
           static_cast<MatrixExpr (*)(const MatrixExpr&, const Expr&)>(
               &matrix_operator_overloads::operator*),
           py::is_operator())
      // Left multiply by scalar:
      .def("__rmul__",
           static_cast<MatrixExpr (*)(const Expr&, const MatrixExpr&)>(
               &matrix_operator_overloads::operator*),
           py::is_operator())
      .def("__neg__", &MatrixExpr::operator-, "Element-wise negation of the matrix.");

  // Matrix constructors:
  m.def("identity", &make_identity, "rows"_a, "Create identity matrix.");
  m.def("eye", &make_identity, "rows"_a, "Create identity matrix (alias for identity).");
  m.def("zeros", &make_zeros, "rows"_a, "cols"_a, "Create a matrix of zeros.");
  m.def("vector", &ColumnVectorFromContainer<py::args>,
        "Construct a column vector from the arguments.");
  m.def("row_vector", &RowVectorFromContainer<py::args>,
        "Construct a row vector from the arguments.");
  m.def("matrix", &MatrixFromIterable, py::arg("rows"),
        "Construct a matrix from an iterator over rows.");
  m.def("matrix_of_symbols", &make_matrix_of_symbols, py::arg("prefix"), py::arg("rows"), py::arg("cols"),
        "Construct a matrix of symbols.");

  m.def("vec", &vectorize_matrix, py::arg("m"), "Vectorize matrix in column-major order.");
  m.def("det", &determinant, py::arg("m"), "Compute determinant of a matrix.");
  m.def("full_piv_lu", &factorize_full_piv_lu, py::arg("m"),
        "Factorize a matrix using fully-pivoting LU decomposition.");

  // Version of where() for matrices
  m.def(
      "where",
      static_cast<MatrixExpr (*)(const Expr&, const MatrixExpr&, const MatrixExpr&)>(&math::where),
      "condition"_a, "if_true"_a, "if_false"_a, "If-else statement with matrix operands.");
}

}  // namespace math
