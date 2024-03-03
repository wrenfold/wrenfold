//! Copyright 2024 Gareth Cross

use nalgebra as na;

#[allow(unused)]
mod generated;
mod numerical;

use generated::*;

/// Declare a method named `func_name` that evaluates a b-spline of order `order`.
/// The caller is expected to pass a series of tuples indicating the generated methods to call
/// when evaluating basis polynomials [0, 1, ...]. For example, for a order 3 spline:
///
///  declare_bspline_method!(
///     bspline3,           //  <-- Name of the function instantiated by this macro.
///     3,                  //  <-- Order of the spline.
///     (0, poly_0),        //  <-- The five piecewise-polynomial bases of order 3.
///     (1, poly_1),
///     ...
///  );
#[macro_export]
macro_rules! declare_bspline_method {
    ($func_name:ident, $order:expr, $(($index:expr, $interval_name:ident)),* $(,)? ) => {
        pub fn $func_name(
            x: f64,
            num_knots: usize,
            coefficients: &mut na::SMatrix<f64, { $order }, { $order - 1 }>,
        ) -> usize {
            // Compute the interval (ie. between which knots does x fall?)
            let interval = compute_basis_index(x, num_knots);
            let num_intervals = num_knots - 1;

            let knot_spacing = 1.0 / num_intervals as f64;
            let base_knot_spacing  = 1.0 / $order as f64;
            let scale_factor = base_knot_spacing / knot_spacing;

            for i in 0..$order {
                // shifted_i is the index of the basis polynomial we need to compute.
                let shifted_i = i + interval;
                let polynomial_index = determine_poly_index(shifted_i, num_intervals, $order);

                // Scale and shift `x` to be in the domain of the basis polynomial `polynomial_index`.
                let source_offset = compute_polynomial_origin(shifted_i, $order, knot_spacing);
                let dest_offset = compute_polynomial_origin(polynomial_index, $order, base_knot_spacing);
                let scaled_x = (x - source_offset) * scale_factor + dest_offset;

                // Call the appropriate method:
                match polynomial_index {
                    $(
                        $index => $interval_name(scaled_x, scale_factor, &mut coefficients.row_mut(i)),
                    )*
                    _ => { panic!("Invalid polynomial index: {}", polynomial_index) }
                }

            }
            interval
        }
    };
}

/// Map argument x from [0, 1] to the index of the 0'th order basis interval.
/// If there are `n` knots, there are n - 1 intervals.
#[inline(always)]
fn compute_basis_index(x: f64, num_knots: usize) -> usize {
    assert!(x >= 0.0);
    assert!(x <= 1.0);
    let num_intervals = num_knots - 1;

    // min() here because the last interval is inclusive on the right side.
    let x_interval = f64::floor(x * num_intervals as f64);
    (x_interval as usize).min(num_intervals - 1)
}

/// Given an index `interval` (between [0, num_intervals]), convert it the index into the basis
/// functions of an order `order` bspline.
#[inline(always)]
fn determine_poly_index(interval: usize, num_intervals: usize, order: usize) -> usize {
    if interval < order - 1 {
        interval
    } else if interval < num_intervals {
        order - 1
    } else {
        order - (num_intervals - interval)
    }
}

/// Determine where basis polynomial `poly_index` begins on the x-axis.
#[inline(always)]
fn compute_polynomial_origin(poly_index: usize, order: usize, knot_spacing: f64) -> f64 {
    (poly_index as i32 - order as i32 + 1).max(0) as f64 * knot_spacing
}

declare_bspline_method!(
    eval_bspline_coefficients_order3,
    3,
    (0, bspline_order3_poly_0),
    (1, bspline_order3_poly_1),
    (2, bspline_order3_poly_2),
    (3, bspline_order3_poly_3),
    (4, bspline_order3_poly_4),
);

declare_bspline_method!(
    eval_bspline_coefficients_order4,
    4,
    (0, bspline_order4_poly_0),
    (1, bspline_order4_poly_1),
    (2, bspline_order4_poly_2),
    (3, bspline_order4_poly_3),
    (4, bspline_order4_poly_4),
    (5, bspline_order4_poly_5),
    (6, bspline_order4_poly_6),
);

declare_bspline_method!(
    eval_bspline_coefficients_order5,
    5,
    (0, bspline_order5_poly_0),
    (1, bspline_order5_poly_1),
    (2, bspline_order5_poly_2),
    (3, bspline_order5_poly_3),
    (4, bspline_order5_poly_4),
    (5, bspline_order5_poly_5),
    (6, bspline_order5_poly_6),
    (7, bspline_order5_poly_7),
    (8, bspline_order5_poly_8),
);

declare_bspline_method!(
    eval_bspline_coefficients_order6,
    6,
    (0, bspline_order6_poly_0),
    (1, bspline_order6_poly_1),
    (2, bspline_order6_poly_2),
    (3, bspline_order6_poly_3),
    (4, bspline_order6_poly_4),
    (5, bspline_order6_poly_5),
    (6, bspline_order6_poly_6),
    (7, bspline_order6_poly_7),
    (8, bspline_order6_poly_8),
    (9, bspline_order6_poly_9),
    (10, bspline_order6_poly_10),
);

#[cfg(test)]
fn test_bspline_generic<const ORDER: usize, const ORDER_MINUS_ONE: usize, F>(
    num_knots: usize,
    func: F,
    tol: f64,
    deriv_tol: f64,
) where
    F: Fn(f64, usize, &mut na::SMatrix<f64, ORDER, ORDER_MINUS_ONE>) -> usize,
{
    const NUM_SAMPLES: usize = 1000;
    for x in (0..NUM_SAMPLES).map(|i| i as f64 / (NUM_SAMPLES as f64 - 1.0)) {
        let mut coefficients = na::SMatrix::zeros();
        let index = func(x, num_knots, &mut coefficients);

        // Check that b-spline coefficients sum to one.
        approx::assert_abs_diff_eq!(1.0, coefficients.column(0).sum(), epsilon = tol);

        // Test against numerical implementation:
        for j in 0..ORDER {
            approx::assert_abs_diff_eq!(
                numerical::BSplineNumerical::new(ORDER, num_knots).eval(x, index + j),
                coefficients[(j, 0)],
                epsilon = tol,
            );
        }
    }

    // Check derivatives:
    for interval in 0..(num_knots - 1) {
        let x = (interval as f64 + 0.5) / (num_knots - 1) as f64;

        let mut coefficients = na::SMatrix::zeros();
        func(x, num_knots, &mut coefficients);

        for diff in 1..ORDER_MINUS_ONE {
            let numerical_diff = wrenfold_test_utils::numerical_jacobian(
                &x,
                &|x| {
                    let mut coeffs = na::SMatrix::zeros();
                    func(*x, num_knots, &mut coeffs);
                    coeffs.column(diff - 1).into_owned()
                },
                0.001,
            );
            approx::assert_abs_diff_eq!(
                numerical_diff,
                coefficients.column(diff).into_owned(),
                epsilon = deriv_tol
            );
        }
    }
}

#[test]
fn test_bspline_order3() {
    test_bspline_generic(4, eval_bspline_coefficients_order3, 1.0e-12, 1.0e-9);
    test_bspline_generic(7, eval_bspline_coefficients_order3, 1.0e-12, 1.0e-9);
}

#[test]
fn test_bspline_order4() {
    test_bspline_generic(5, eval_bspline_coefficients_order4, 1.0e-12, 1.0e-9);
    test_bspline_generic(8, eval_bspline_coefficients_order4, 1.0e-12, 1.0e-9);
}

#[test]
fn test_bspline_order5() {
    test_bspline_generic(6, eval_bspline_coefficients_order5, 1.0e-9, 1.0e-6);
    test_bspline_generic(13, eval_bspline_coefficients_order5, 1.0e-9, 1.0e-6);
}

#[test]
fn test_bspline_order6() {
    test_bspline_generic(7, eval_bspline_coefficients_order6, 1.0e-9, 1.0e-6);
    test_bspline_generic(16, eval_bspline_coefficients_order6, 1.0e-9, 1.0e-6);
}

/// Do a simple curve fitting problem with a 4th-order b-spline.
#[test]
#[allow(non_snake_case)]
fn test_bspline_fit_order_4() {
    let num_knots = 5;

    // Some sample data we will fit our spline to.
    let sample_pts = [
        [0.05, 0.4],
        [0.1, 0.8],
        [0.31, -0.1],
        [0.49, 0.4],
        [0.7, 0.0],
        [0.95, 0.3],
        [0.98, 0.56],
    ];

    // Build linear system Ax = b
    let mut A = na::DMatrix::<f64>::zeros(sample_pts.len(), num_knots + 2);
    let mut b = na::DVector::<f64>::zeros(sample_pts.len());

    for (row, [x, y]) in sample_pts.iter().enumerate() {
        let mut coeffs = na::SMatrix::zeros();
        let col_offset = eval_bspline_coefficients_order4(*x, num_knots, &mut coeffs);

        A.row_mut(row)
            .fixed_columns_mut::<4>(col_offset)
            .copy_from(&coeffs.column(0).transpose());
        b[row] = *y;
    }

    // Solve:
    let params = A.clone().try_inverse().unwrap() * &b;
    println!("A: {}", A);
    println!("b: {}", b);
    println!("params: {}", params.transpose());

    // Validate that we passed through our target points.
    for [x, y] in sample_pts.into_iter() {
        let mut coeffs = na::SMatrix::zeros();
        let index = eval_bspline_coefficients_order4(x, num_knots, &mut coeffs);
        approx::assert_abs_diff_eq!(
            y,
            coeffs.column(0).dot(&params.rows(index, 4)),
            epsilon = 1.0e-14
        );
    }
}
