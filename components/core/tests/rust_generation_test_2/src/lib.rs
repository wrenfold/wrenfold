//! wrenfold symbolic code generator.
//! Copyright (c) 2024 Gareth Cross
#![allow(non_snake_case)]

#[cfg(test)]
mod generated;

#[cfg(test)]
pub mod types {
    #[derive(Debug)]
    pub struct StructType {
        pub x: f64,
        pub y: f64,
    }

    /// Support methods so we can compare to the generated code.
    impl StructType {
        pub fn dot(&self, other: &StructType) -> f64 {
            self.x * other.x + self.y * other.y
        }

        pub fn lerp(&self, other: &StructType, alpha: f64) -> StructType {
            Self {
                x: other.x * alpha + self.x * (1.0 - alpha),
                y: other.y * alpha + self.y * (1.0 - alpha),
            }
        }
    }
}

#[cfg(test)]
pub mod external {
    use crate::types::StructType;

    /// An external function we call from generated code.
    pub fn interpolate_access(vec: &Vec<StructType>, x: f64) -> StructType {
        let x_floor = x.floor();
        let x_ceil = x.ceil();
        let alpha = x - x_floor;
        let i0 = x_floor as usize;
        let i1 = x_ceil as usize;
        vec.get(i0).unwrap().lerp(vec.get(i1).unwrap(), alpha)
    }
}

/// Test that we can call our external function that takes an opaque types (vector of custom structs).
#[test]
fn test_lookup_and_compute_inner_product() {
    use approx::assert_abs_diff_eq;

    const NUM_SAMPLES: usize = 20;
    let test_vector = (0..NUM_SAMPLES)
        .map(|i| (i as f64 + 0.5) / NUM_SAMPLES as f64)
        .map(|alpha| types::StructType {
            x: alpha.cos(),
            y: alpha.sin(),
        })
        .collect::<Vec<_>>();

    assert_abs_diff_eq!(
        external::interpolate_access(&test_vector, 0.0)
            .dot(&external::interpolate_access(&test_vector, 19.0)),
        generated::lookup_and_compute_inner_product(&test_vector, 0.0, 19.0),
        epsilon = 1.0e-15
    );
    assert_abs_diff_eq!(
        external::interpolate_access(&test_vector, 7.2)
            .dot(&external::interpolate_access(&test_vector, 0.23)),
        generated::lookup_and_compute_inner_product(&test_vector, 7.2, 0.23),
        epsilon = 1.0e-15
    );
    assert_abs_diff_eq!(
        external::interpolate_access(&test_vector, 16.0)
            .dot(&external::interpolate_access(&test_vector, 5.98)),
        generated::lookup_and_compute_inner_product(&test_vector, 16.0, 5.98),
        epsilon = 1.0e-15
    );
}
