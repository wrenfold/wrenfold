//! Utilities for computing numerical jacobians.

use nalgebra as na;

pub trait Manifold<T, const DIM: usize> {
    /// Apply a tangent-space perturbation to an element of the manifold.
    /// Sometimes written in the literature as `self [+] dx` or `self \oplus dx`.
    fn retract(&self, dx: &na::SVector<T, DIM>) -> Self;

    /// Compute the tangent-space difference between `other` and `self` around `self`.
    /// Sometimes written in the literature as `other [-] self` or `other \ominus self`.
    fn local_coordinates(&self, other: &Self) -> na::SVector<T, DIM>;
}

/// Implement `Manifold` for all nalgebra vectors.
impl<T, const D: usize> Manifold<T, D> for na::SVector<T, D>
where
    T: na::RealField,
{
    fn retract(&self, dx: &na::SVector<T, D>) -> Self {
        self + dx
    }

    fn local_coordinates(&self, other: &Self) -> na::SVector<T, D> {
        other - self
    }
}

/// Compute central difference derivative, with error in O(step^6).
fn central_diff<T, F, const Y_DIM: usize>(dx: T, f: &F) -> na::SVector<T, Y_DIM>
where
    F: Fn(T) -> na::SVector<T, Y_DIM>,
    T: na::RealField + Copy,
{
    let dx_times_2 = dx * T::from_i32(2).unwrap();
    let dx_times_3 = dx * T::from_i32(3).unwrap();
    let c1 = f(dx) - f(-dx);
    let c2 = f(dx_times_2) - f(-dx_times_2);
    let c3 = f(dx_times_3) - f(-dx_times_3);
    (c1 * T::from_i32(45).unwrap() - c2 * T::from_i32(9).unwrap() + c3)
        / (T::from_i32(60).unwrap() * dx)
}

/// Given a function `f` that maps from a an `M` dimensional manifold to an `N` dimensional manifold,
/// compute the NxM jacobian about `x`.
///
/// Based on C++ implementation in `numerical_jacobian.h`
/// TODO: Generalize to support dynamic dimensions.
#[allow(non_snake_case)]
pub fn numerical_jacobian<X, Y, F, T, const X_DIM: usize, const Y_DIM: usize>(
    x: &X,
    f: &F,
    step: T,
) -> na::SMatrix<T, Y_DIM, X_DIM>
where
    X: Manifold<T, X_DIM>,
    Y: Manifold<T, Y_DIM>,
    F: Fn(&X) -> Y,
    T: na::RealField + Copy,
{
    // Evaluate function at the linearization point `x`.
    let y_at_x = f(x);

    // Iterate over variables and perturb each dimension of `X`:
    let mut J = na::SMatrix::<T, Y_DIM, X_DIM>::zeros();
    for var in 0..X_DIM {
        let dy = central_diff(step, &|dx: T| -> na::SVector<T, Y_DIM> {
            let mut delta = na::SVector::<T, X_DIM>::zeros();
            delta[var] = dx;
            let y_perturbed = f(&x.retract(&delta));
            y_at_x.local_coordinates(&y_perturbed)
        });
        J.column_mut(var).copy_from(&dy);
    }
    J
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Define a custom type so we can test manifold trait.
    #[derive(Debug, Copy, Clone)]
    struct Vars<T> {
        pub x: T,
        pub y: T,
        pub z: T,
    }

    impl<T> Manifold<T, 3> for Vars<T>
    where
        T: na::RealField + Copy,
    {
        fn retract(&self, dx: &na::SVector<T, 3>) -> Self {
            Self {
                x: self.x + dx[0],
                y: self.y + dx[1],
                z: self.z + dx[2],
            }
        }

        fn local_coordinates(&self, other: &Self) -> na::SVector<T, 3> {
            na::SVector::<T, 3>::new(other.x - self.x, other.y - self.y, other.z - self.z)
        }
    }

    #[test]
    fn test_numerical_jacobian() {
        // Test on a function mapping from R(3) --> R(2)
        let f = |v: &Vars<f64>| -> na::Vector2<f64> {
            na::Vector2::new(v.x * v.y.powi(3) - v.z.sin(), v.y * v.x.cos() - v.x.powi(2))
        };

        let v = Vars {
            x: -0.5,
            y: 0.1,
            z: 1.2,
        };
        let df = numerical_jacobian(&v, &f, 0.01);

        let df_expected = na::Matrix2x3::<f64>::new(
            v.y.powi(3),
            3.0 * v.x * v.y.powi(2),
            -v.z.cos(),
            -v.y * v.x.sin() - 2.0 * v.x,
            v.x.cos(),
            0.0,
        );
        println!("expected:\n{}\nactual:\n{}\n", df_expected, df);
        approx::assert_abs_diff_eq!(df_expected, df, epsilon = 1.0e-12);
    }
}
