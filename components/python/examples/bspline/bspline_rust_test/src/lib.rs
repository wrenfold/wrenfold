use nalgebra as na;

mod numerical;

mod gen {
    // CMake exports `CODE_GENERATION_FILE` when building this target.
    include!(concat!(env!("CODE_GENERATION_FILE")));
}

use gen::{
    bspline_order4_interval_0, bspline_order4_interval_1, bspline_order4_interval_2,
    bspline_order4_interval_3, bspline_order7_interval_0, bspline_order7_interval_1,
    bspline_order7_interval_2, bspline_order7_interval_3, bspline_order7_interval_4,
    bspline_order7_interval_5, bspline_order7_interval_6,
};

/// Declare a method named `func_name` that evaluates a b-spline of order `order`.
/// The caller is expected to pass a series of tuples indicating the generated methods to call
/// when evaluating intervals [0, 1, ..., order - 1]. For example, for a order 3 spline:
///
///  declare_bspline_method!(
///     bspline3,           //  <-- Name of the function instantiated by this macro.
///     3,                  //  <-- Order of the spline.
///     (0, interval_0),    //  <-- Handles interval [0, t_1]
///     (1, interval_1),    //  <-- Handles interval [t_1, t_2]
///     interval_2,         //  <-- Handles all other intervals.
///  );
#[macro_export]
macro_rules! declare_bspline_method {
    ($func_name:ident, $order:expr, $(($index:expr, $interval_name:ident)),*, $general_interval_name:ident $(,)? ) => {
        pub fn $func_name(
            x: f64,
            num_knots: usize,
            coefficients: &mut na::SMatrix<f64, { $order }, { $order - 1 }>,
        ) -> usize {
            let num_intervals = num_knots - 1;
            let (i, x_unit, scale_factor) = compute_basis_index(x, num_knots);
            $(
                if i == $index {
                    $interval_name(x_unit, scale_factor, coefficients);
                } else if i == num_intervals - $index - 1 {
                    $interval_name(1.0 - x_unit, -scale_factor, coefficients);
                    swap_rows(coefficients);
                } else
            )* {
                $general_interval_name(x_unit, scale_factor, coefficients);
            }
            i
        }
    };
}

/// Map argument x from [0, 1] to the index of the 0'th order basis interval.
/// If there are `n` knots, there are n - 1 intervals.
#[inline(always)]
fn compute_basis_index(x: f64, num_knots: usize) -> (usize, f64, f64) {
    assert!(x >= 0.0);
    assert!(x <= 1.0);
    let num_intervals = num_knots - 1;

    // min() here because the last interval is inclusive on the right side.
    let x_interval = f64::floor(x * num_intervals as f64);
    let i = (x_interval as usize).min(num_intervals - 1);

    // Compute the start of the interval that `x` falls in:
    let knot_spacing = 1.0 / num_intervals as f64;
    let interval_start = i as f64 * knot_spacing;

    // Shift and scale into [0, 1].
    let scale_factor = 1.0 / knot_spacing;
    let x_unit = (x - interval_start) * scale_factor;

    (i, x_unit, scale_factor)
}

/// Swap the column order of a fixed-size nalgebra matrix.
#[inline(always)]
fn swap_rows<const ROWS: usize, const COLS: usize>(m: &mut na::SMatrix<f64, ROWS, COLS>) {
    for row in 0..(ROWS / 2) {
        m.swap_rows(row, ROWS - row - 1);
    }
}

// Define evaluator for 4-th order b-spline.
declare_bspline_method!(
    eval_bspline_coefficients_order4,
    4,
    (0, bspline_order4_interval_0),
    (1, bspline_order4_interval_1),
    (2, bspline_order4_interval_2),
    bspline_order4_interval_3,
);

// Define evaluator for 7-th order b-spline.
declare_bspline_method!(
    eval_bspline_coefficients_order7,
    7,
    (0, bspline_order7_interval_0),
    (1, bspline_order7_interval_1),
    (2, bspline_order7_interval_2),
    (3, bspline_order7_interval_3),
    (4, bspline_order7_interval_4),
    (5, bspline_order7_interval_5),
    bspline_order7_interval_6,
);

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

#[test]
fn test_bspline_order4() {
    let num_knots = 8;
    let interval_scale = 1.0 / (num_knots as f64 - 1.0);

    let mut coeffs_prev = na::SMatrix::zeros();
    let mut coeffs_next = na::SMatrix::zeros();

    // Check that intervals are continuous at the intersection points.
    bspline_order4_interval_0(1.0, interval_scale, &mut coeffs_prev);
    bspline_order4_interval_1(0.0, interval_scale, &mut coeffs_next);
    approx::assert_abs_diff_eq!(
        coeffs_prev.fixed_rows::<3>(1),
        coeffs_next.fixed_rows::<3>(0),
        epsilon = 1.0e-12
    );

    bspline_order4_interval_1(1.0, interval_scale, &mut coeffs_prev);
    bspline_order4_interval_2(0.0, interval_scale, &mut coeffs_next);
    approx::assert_abs_diff_eq!(
        coeffs_prev.fixed_rows::<3>(1),
        coeffs_next.fixed_rows::<3>(0),
        epsilon = 1.0e-12
    );

    bspline_order4_interval_2(1.0, interval_scale, &mut coeffs_prev);
    bspline_order4_interval_3(0.0, interval_scale, &mut coeffs_next);
    approx::assert_abs_diff_eq!(
        coeffs_prev.fixed_rows::<3>(1),
        coeffs_next.fixed_rows::<3>(0),
        epsilon = 1.0e-12
    );

    const NUM_SAMPLES: usize = 1000;
    for x in (0..NUM_SAMPLES).map(|i| i as f64 / (NUM_SAMPLES as f64 - 1.0)) {
        let mut coefficients = na::SMatrix::zeros();
        let index = eval_bspline_coefficients_order4(x, num_knots, &mut coefficients);

        // Check that b-spline coefficients sum to one.
        approx::assert_abs_diff_eq!(1.0, coefficients.column(0).sum(), epsilon = 1.0e-12);

        // Test against numerical implementation:
        for j in 0..4 {
            approx::assert_abs_diff_eq!(
                numerical::BSplineNumerical::new(4, num_knots).eval(x, index + j),
                coefficients[(j, 0)],
                epsilon = 1.0e-12,
            );
        }
    }
}

#[test]
fn test_bspline_order7() {
    let num_knots = 13;

    const NUM_SAMPLES: usize = 1000;
    for x in (0..NUM_SAMPLES).map(|i| i as f64 / (NUM_SAMPLES as f64 - 1.0)) {
        let mut coefficients = na::SMatrix::zeros();
        let index = eval_bspline_coefficients_order7(x, num_knots, &mut coefficients);
        approx::assert_abs_diff_eq!(1.0, coefficients.column(0).sum(), epsilon = 1.0e-10);

        for j in 0..7 {
            approx::assert_abs_diff_eq!(
                numerical::BSplineNumerical::new(7, num_knots).eval(x, index + j),
                coefficients[(j, 0)],
                epsilon = 1.0e-10,
            );
        }
    }
}
