// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/functional.h>
#include <pybind11/numpy.h>
#include <pybind11/operators.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "expression.h"
#include "expressions/matrix.h"
#include "matrix_functions.h"

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
  index_t Length() const { return static_cast<index_t>(length_); }

  // Convert flat index to modified index.
  index_t Map(index_t i) const { return static_cast<index_t>(start_ + i * step_); }

 private:
  py::ssize_t start_;
  py::ssize_t stop_;
  py::ssize_t step_;
  py::ssize_t length_;
};

// Iterator over rows of a matrix expression.
struct RowIterator {
  static RowIterator Begin(const MatrixExpr& m) { return RowIterator(m, 0); }
  static RowIterator End(const MatrixExpr& m) { return RowIterator(m, m.NumRows()); }

  // Iterators only match if the underlying matrix expression is the same instance.
  bool operator==(const RowIterator& other) const {
    return parent_.HasSameAddress(other.parent_) && row_ == other.row_;
  }

  // Pre-increment. We don't need post-increment for pybind.
  RowIterator& operator++() {
    row_++;
    return *this;
  }

  // De-reference. Behaves like doing `m[row]`.
  Expr operator*() const {
    if (parent_.NumCols() == 1) {
      return parent_[row_];
    } else {
      return parent_.GetBlock(row_, 0, 1, parent_.NumCols());
    }
  }

 private:
  // We store a strong reference to the parent, thereby ensuring this iterator is always valid.
  math::MatrixExpr parent_;
  math::index_t row_;

  RowIterator(const MatrixExpr& parent, index_t row) : parent_(parent), row_(row) {}
};

Expr MatrixGetRowCol(const MatrixExpr& self, const std::tuple<index_t, index_t> row_col) {
  auto [row, col] = row_col;
  return self(row < 0 ? (self.NumRows() + row) : row, col < 0 ? (self.NumCols() + col) : col);
}

// Return variant because this could be a single expression, or a matrix expression.
std::variant<Expr, MatrixExpr> MatrixGetRow(const MatrixExpr& self, const index_t row) {
  if (self.NumCols() == 1) {
    // Vectors convert to scalar automatically (don't form 1x1 matrix).
    return self[row < 0 ? (self.NumRows() + row) : row];
  } else {
    return self.GetBlock(row < 0 ? (self.NumRows() + row) : row, 0, 1, self.NumCols());
  }
}

MatrixExpr MatrixGetRowSlice(const MatrixExpr& self, py::slice slice) {
  const Slice slice_index{self.NumRows(), slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(slice_index.Length() * self.NumCols()));

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < slice_index.Length(); ++i) {
    for (index_t j = 0; j < self.NumCols(); ++j) {
      elements.push_back(self(slice_index.Map(i), j));
    }
  }
  return MatrixExpr::Create(slice_index.Length(), self.NumCols(), std::move(elements));
}

MatrixExpr MatrixGetRowColSlice(const MatrixExpr& self,
                                const std::tuple<py::slice, py::slice>& slices) {
  const auto [row_slice, col_slice] = slices;
  const Slice row_index{self.NumRows(), row_slice};
  const Slice col_index{self.NumCols(), col_slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(row_index.Length() * col_index.Length()));

  // Step over sliced rows and pull out all columns:
  for (index_t i = 0; i < row_index.Length(); ++i) {
    for (index_t j = 0; j < col_index.Length(); ++j) {
      elements.push_back(self(row_index.Map(i), col_index.Map(j)));
    }
  }
  return MatrixExpr::Create(row_index.Length(), col_index.Length(), std::move(elements));
}

MatrixExpr MatrixGetRowIndexColSlice(const MatrixExpr& self,
                                     const std::tuple<index_t, py::slice>& row_and_col_slice) {
  const auto [row, col_slice] = row_and_col_slice;
  const index_t wrapped_row = row < 0 ? self.NumRows() - row : row;
  const Slice col_index{self.NumCols(), col_slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(col_index.Length()));
  for (index_t j = 0; j < col_index.Length(); ++j) {
    elements.push_back(self(wrapped_row, col_index.Map(j)));
  }
  return MatrixExpr::Create(1, col_index.Length(), std::move(elements));
}

MatrixExpr MatrixGetRowSliceColIndex(const MatrixExpr& self,
                                     const std::tuple<py::slice, index_t>& row_slice_and_col) {
  const auto [row_slice, col] = row_slice_and_col;
  const index_t wrapped_col = col < 0 ? self.NumCols() - col : col;
  const Slice row_index{self.NumRows(), row_slice};

  std::vector<Expr> elements;
  elements.reserve(static_cast<std::size_t>(row_index.Length()));
  for (index_t i = 0; i < row_index.Length(); ++i) {
    elements.push_back(self(row_index.Map(i), wrapped_col));
  }
  return MatrixExpr::Create(row_index.Length(), 1, std::move(elements));
}

// Iterate over a container and transform every element into `Expr`.
template <typename Container>
std::size_t TransformIntoExpr(const Container& inputs, std::vector<Expr>& output) {
  // len_hint will get the length if possible, otherwise return 0.
  if constexpr (std::is_base_of_v<py::handle, std::decay_t<Container>>) {
    output.reserve(output.size() + py::len_hint(inputs));
  } else {
    output.reserve(output.size() + inputs.size());
  }
  // Count so we only traverse the input iterators once (to handle generators).
  std::size_t count = 0;
  std::transform(inputs.begin(), inputs.end(), std::back_inserter(output),
                 [&](const py::handle& handle) {
                   ++count;
                   return py::cast<Expr>(handle);
                 });
  return count;
}

template <typename Container>
MatrixExpr ColumnVectorFromContainer(const Container& inputs) {
  std::vector<Expr> converted;
  TransformIntoExpr(inputs, converted);
  if (converted.empty()) {
    throw DimensionError("Cannot construct empty vector.");
  }
  return MatrixExpr::Create(converted.size(), 1, std::move(converted));
}

template <typename Container>
MatrixExpr RowVectorFromContainer(const Container& inputs) {
  std::vector<Expr> converted;
  TransformIntoExpr(inputs, converted);
  if (converted.empty()) {
    throw DimensionError("Cannot construct empty row vector.");
  }
  return MatrixExpr::Create(1, converted.size(), std::move(converted));
}

inline std::size_t ExtractIterableRows(const py::handle& row, std::vector<Expr>& output) {
  if (py::isinstance<MatrixExpr>(row)) {
    const MatrixExpr as_matrix = py::cast<MatrixExpr>(row);
    // If the "row" is a matrix, we stack them vertically:
    const auto num_elements = as_matrix.NumCols() * as_matrix.NumRows();
    output.reserve(output.size() + static_cast<std::size_t>(num_elements));
    std::copy(as_matrix.AsMatrix().begin(), as_matrix.AsMatrix().end(), std::back_inserter(output));
    return as_matrix.NumCols();
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
  return MatrixExpr{MakeExpr<Matrix>(total_rows, expected_num_cols, std::move(converted))};
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
  result.reserve(self.Size());
  const auto& m = self.AsMatrix();
  std::transform(m.begin(), m.end(), std::back_inserter(result), func);
  return MatrixExpr::Create(self.NumRows(), self.NumCols(), std::move(result));
}

py::list ListFromMatrix(const MatrixExpr& self) {
  py::list rows{};
  for (index_t i = 0; i < self.NumRows(); ++i) {
    py::list cols{};
    for (index_t j = 0; j < self.NumCols(); ++j) {
      cols.append(self(i, j));
    }
    rows.append(std::move(cols));
  }
  return rows;
}

// Convert matrix to numpy array.
py::array NumpyFromMatrix(const MatrixExpr& self) {
  auto list = py::list();  // TODO: Don't copy into list.
  for (const Expr& expr : self.AsMatrix()) {
    list.append(expr);
  }
  auto array = py::array(list);
  const std::array<std::size_t, 2> new_shape{static_cast<std::size_t>(self.NumRows()),
                                             static_cast<std::size_t>(self.NumCols())};
  array.resize(new_shape);
  return array;
}

// For the benefit of python types, we need to re-define these with MatrixExpr as the type.
// TODO: Gross that this adds a bunch more casting/calling overhead.
MatrixExpr operator+(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{static_cast<const Expr&>(a) + static_cast<const Expr&>(b)};
}

MatrixExpr operator-(const MatrixExpr& a, const MatrixExpr& b) {
  return MatrixExpr{static_cast<const Expr&>(a) - static_cast<const Expr&>(b)};
}

// Handle matrix * matrix, which may produce a scalar.
std::variant<Expr, MatrixExpr> operator*(const MatrixExpr& a, const MatrixExpr& b) {
  Expr result = static_cast<const Expr&>(a) * static_cast<const Expr&>(b);
  if (TryCast<Matrix>(result) != nullptr) {
    return static_cast<const MatrixExpr&>(result);
  }
  return result;
}
MatrixExpr operator*(const MatrixExpr& a, const Expr& b) {
  return MatrixExpr{static_cast<const Expr&>(a) * static_cast<const Expr&>(b)};
}
MatrixExpr operator*(const Expr& a, const MatrixExpr& b) {
  return MatrixExpr{static_cast<const Expr&>(a) * static_cast<const Expr&>(b)};
}

void WrapMatrixOperations(py::module_& m) {
  // Matrix expression type.
  py::class_<MatrixExpr, Expr>(m, "MatrixExpr")
      .def_property_readonly(
          "shape", [](const MatrixExpr& m) { return py::make_tuple(m.NumRows(), m.NumCols()); },
          "Shape of the matrix in (row, col) format.")
      .def_property_readonly("size", &MatrixExpr::Size, "Total number of elements.")
      .def_property_readonly(
          "is_empty", [](const MatrixExpr& m) { return m.NumRows() == 0 || m.NumCols() == 0; },
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
      .def("__len__", &MatrixExpr::NumRows, "Number of rows in the matrix.")
      .def("__iter__",  //  We don't need keep_alive since MatrixExpr does that for us.
           [](const MatrixExpr& expr) {
             return py::make_iterator(RowIterator::Begin(expr), RowIterator::End(expr));
           })
      .def("unary_map", &MapMatrix, py::arg("func"), "Perform element-wise map operation.")
      // Convert to list
      .def("to_list", &ListFromMatrix, "Convert to list of lists.")
      .def("transpose", &MatrixExpr::Transpose, "Transpose the matrix.")
      .def_property_readonly("T", &MatrixExpr::Transpose, "Transpose the matrix.")
      // Operators:
      // We need to override these again so that we get `MatrixExpr` return type.
      .def(py::self + py::self)
      .def(py::self - py::self)
      .def(py::self * py::self)
      .def(
          "__mul__", [](const MatrixExpr& a, const Expr& b) { return a * b; }, py::is_operator())
      .def(
          "__rmul__", [](const Expr& a, const MatrixExpr& b) { return a * b; }, py::is_operator())
      .def(
          "__neg__", [](const MatrixExpr& x) -> MatrixExpr { return x * -1; },
          "Element-wise negation of the matrix.");

  // Matrix constructors:
  m.def("identity", &Identity, "rows"_a, "Create identity matrix.");
  m.def("eye", &Identity, "rows"_a, "Create identity matrix (alias for identity).");
  m.def("zeros", &Zeros, "rows"_a, "cols"_a, "Create a matrix of zeros");
  m.def(
      "vector", [](py::args args) { return ColumnVectorFromContainer(args); },
      "Construct a column vector from the arguments.");
  m.def(
      "row_vector", [](py::args args) { return RowVectorFromContainer(args); },
      "Construct a row vector from the arguments.");
  m.def("matrix", &MatrixFromIterable, py::arg("rows"),
        "Construct a matrix from an iterator over rows.");
  m.def("matrix_of_symbols", &MatrixOfSymbols, py::arg("prefix"), py::arg("rows"), py::arg("cols"),
        "Construct a matrix of symbols.");

  m.def("vec", &Vec, py::arg("m"), "Vectorize matrix in column-major order");
}

}  // namespace math
