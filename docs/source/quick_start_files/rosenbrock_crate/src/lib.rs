#[cfg(test)]
mod generated; //  We assume the generated output was written to `generated.rs`.

#[cfg(test)]
mod tests {
    pub use super::*;
    use nalgebra as na;

    #[test]
    fn test_rosenbrock() {
        let a = 2.0;
        let b = 10.0;
        let xy_min = na::Vector2::new(a, a * a);

        let mut f_D_xy = na::SMatrix::<f64, 1, 2>::zeros();
        let f = generated::rosenbrock(&xy_min, a, b, &mut f_D_xy);

        assert_eq!(0.0, f);
        assert_eq!(0.0, f_D_xy[0]);
        assert_eq!(0.0, f_D_xy[1]);
    }
}
