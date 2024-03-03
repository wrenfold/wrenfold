//! Copyright 2024 Gareth Cross
#![cfg(test)]

/// Numerical implementation we compare the generated polynomials to.
/// Written by copying the test C++ implementation in bspline_numerical.h
#[derive(Debug, Copy, Clone)]
pub struct BSplineNumerical {
    order: usize,
    num_knots: usize,
}

impl BSplineNumerical {
    pub fn new(order: usize, num_knots: usize) -> Self {
        Self { order, num_knots }
    }

    fn num_extended_knots(&self) -> usize {
        self.num_knots + 2 * (self.order - 1)
    }

    fn knot_spacing(&self) -> f64 {
        1.0 / (self.num_knots - 1) as f64
    }

    fn knot_value(&self, i: usize) -> f64 {
        if i < self.order - 1 {
            0.0
        } else if i > self.num_extended_knots() - self.order - 1 {
            1.0
        } else {
            (i - (self.order - 1)) as f64 * self.knot_spacing()
        }
    }

    fn weight(&self, x: f64, i: usize, k: usize) -> f64 {
        if self.knot_value(i) == self.knot_value(i + k) {
            0.0
        } else {
            (x - self.knot_value(i)) / (self.knot_value(i + k) - self.knot_value(i))
        }
    }

    fn cox_de_boor(&self, x: f64, i: usize, k: usize) -> f64 {
        if k == 0 {
            let last_non_repeated_knot = self.num_knots + (self.order - 1) - 1;
            let upper_bound = if i + 1 == last_non_repeated_knot {
                x <= self.knot_value(i + 1)
            } else {
                x < self.knot_value(i + 1)
            };
            if self.knot_value(i) <= x && upper_bound {
                1.0
            } else {
                0.0
            }
        } else {
            self.weight(x, i, k) * self.cox_de_boor(x, i, k - 1)
                + (1.0 - self.weight(x, i + 1, k)) * self.cox_de_boor(x, i + 1, k - 1)
        }
    }

    pub fn eval(&self, x: f64, i: usize) -> f64 {
        self.cox_de_boor(x, i, self.order - 1)
    }
}
