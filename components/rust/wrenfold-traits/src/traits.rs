/// Define traits used to pass data to code-generation methods.

#[cfg(feature = "nalgebra")]
use nalgebra as na;

pub trait Span1D<const D0: usize> {
    type ValueType;

    fn get(&self, i: usize) -> Self::ValueType;
}

pub trait OutputSpan1D<const D0: usize> {
    type ValueType;

    fn set(&mut self, i: usize, val: Self::ValueType);
}

pub trait Span2D<const D0: usize, const D1: usize> {
    type ValueType;

    fn get(&self, i: usize, j: usize) -> Self::ValueType;
}

pub trait OutputSpan2D<const D0: usize, const D1: usize> {
    type ValueType;

    fn set(&mut self, i: usize, j: usize, val: Self::ValueType);
}

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
