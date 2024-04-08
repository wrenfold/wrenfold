/// Define traits used to pass data to code-generation methods.

#[cfg(feature = "nalgebra")]
use nalgebra as na;

/// A one-dimensional immutable input span with length `D0`.
pub trait Span1D<const D0: usize> {
    /// The spanned scalar type.
    type ValueType;

    /// Access element `i`
    fn get(&self, i: usize) -> Self::ValueType;
}

/// A one-dimensional mutable output span with length `D0`.
pub trait OutputSpan1D<const D0: usize> {
    /// The spanned scalar type.
    type ValueType;

    /// Set element `i` to `val`.
    fn set(&mut self, i: usize, val: Self::ValueType);
}

/// A two-dimensional immutable input span with shape `(D0, D1)`.
pub trait Span2D<const D0: usize, const D1: usize> {
    /// The spanned scalar type.
    type ValueType;

    /// Access element `(i, j)` where `i` is the row and `j` is the column.
    fn get(&self, i: usize, j: usize) -> Self::ValueType;
}

/// A two-dimensional mutable output span with shape `(D0, D1)`.
pub trait OutputSpan2D<const D0: usize, const D1: usize> {
    /// The spanned scalar type.
    type ValueType;

    /// Set element `(i, j)` to `val`.
    fn set(&mut self, i: usize, j: usize, val: Self::ValueType);
}

/// Implementation of `Span1D` for nalgebra column vectors.
#[cfg(feature = "nalgebra")]
impl<T, S, const D0: usize> Span1D<D0> for na::Matrix<T, na::Const<D0>, na::Const<1>, S>
where
    T: na::Scalar + Copy,
    S: na::RawStorage<T, na::Const<D0>, na::Const<1>>,
{
    type ValueType = T;

    #[inline(always)]
    fn get(&self, i: usize) -> Self::ValueType {
        self[i]
    }
}

/// Implementation of `OutputSpan1D` for nalgebra column vectors.
#[cfg(feature = "nalgebra")]
impl<T, S, const D0: usize> OutputSpan1D<D0> for na::Matrix<T, na::Const<D0>, na::Const<1>, S>
where
    T: na::Scalar + Copy,
    S: na::RawStorageMut<T, na::Const<D0>, na::Const<1>>,
{
    type ValueType = T;

    #[inline(always)]
    fn set(&mut self, i: usize, val: Self::ValueType) {
        self[i] = val;
    }
}

/// Implementation of `Span2D` for statically size nalgebra matrices.
#[cfg(feature = "nalgebra")]
impl<T, S, const D0: usize, const D1: usize> Span2D<D0, D1>
    for na::Matrix<T, na::Const<D0>, na::Const<D1>, S>
where
    T: na::Scalar + Copy,
    S: na::RawStorage<T, na::Const<D0>, na::Const<D1>>,
{
    type ValueType = T;

    #[inline(always)]
    fn get(&self, i: usize, j: usize) -> Self::ValueType {
        self[(i, j)]
    }
}

/// Implementation of `OutputSpan2D` for statically size nalgebra matrices.
#[cfg(feature = "nalgebra")]
impl<T, S, const D0: usize, const D1: usize> OutputSpan2D<D0, D1>
    for na::Matrix<T, na::Const<D0>, na::Const<D1>, S>
where
    T: na::Scalar + Copy,
    S: na::RawStorageMut<T, na::Const<D0>, na::Const<D1>>,
{
    type ValueType = T;

    #[inline(always)]
    fn set(&mut self, i: usize, j: usize, val: Self::ValueType) {
        self[(i, j)] = val;
    }
}
