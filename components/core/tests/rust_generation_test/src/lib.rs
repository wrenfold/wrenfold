//! Tests on generated rust code. We validate that it compiles and yields reasonable
//! numerical results.
#![allow(non_snake_case)]

#[allow(dead_code)]
mod gen;

#[cfg(test)]
use approx::assert_abs_diff_eq;
#[cfg(test)]
use nalgebra as na;

#[test]
fn test_simple_multiply_add() {
    assert_abs_diff_eq!(
        0.0,
        gen::simple_multiply_add(1.0, 0.0, 0.0),
        epsilon = 1.0e-14
    );
    assert_abs_diff_eq!(
        4.0,
        gen::simple_multiply_add(0.0, 2.0, 4.0),
        epsilon = 1.0e-14
    );
    assert_abs_diff_eq!(
        -1.6,
        gen::simple_multiply_add(2.0, -1.2, 0.8),
        epsilon = 1.0e-15
    );
    assert_abs_diff_eq!(
        -6.13,
        gen::simple_multiply_add(-1.3, 4.1, -0.8),
        epsilon = 1.0e-15
    );
}

#[test]
fn test_vector_rotation_2d() {
    let v = na::Vector2::new(0.8, 0.34);
    let mut v_rot = na::Vector2::zeros();
    let mut v_rot_D_theta = na::Vector2::zeros();

    gen::vector_rotation_2d(-0.4, &v, &mut v_rot, Some(&mut v_rot_D_theta));
    assert_abs_diff_eq!(
        na::Rotation2::new(-0.4).transform_vector(&v),
        v_rot,
        epsilon = 1.0e-15
    );
    assert_abs_diff_eq!(0.0, v_rot.dot(&v_rot_D_theta), epsilon = 1.0e-15);

    let v = na::Vector2::new(1.0, 0.0);
    gen::vector_rotation_2d(0.1146, &v, &mut v_rot, None::<&mut na::Vector2<f64>>);
    assert_abs_diff_eq!(
        na::Rotation2::new(0.1146).transform_vector(&v),
        v_rot,
        epsilon = 1.0e-15
    );

    let v = na::Vector2::new(-2.51, 1.45);
    gen::vector_rotation_2d(-1.9, &v, &mut v_rot, None::<&mut na::Vector2<f64>>);
    assert_abs_diff_eq!(
        na::Rotation2::new(-1.9).transform_vector(&v),
        v_rot,
        epsilon = 1.0e-15
    );
}

#[test]
fn test_heaviside() {
    assert_eq!(0.0, gen::heaviside(-1.0e-32));
    assert_eq!(0.0, gen::heaviside(-1.0e-6));
    assert_eq!(1.0, gen::heaviside(-0.0));
    assert_eq!(1.0, gen::heaviside(0.0));
    assert_eq!(1.0, gen::heaviside(1.0e-9));
    assert_eq!(1.0, gen::heaviside(1.0));
}

#[test]
fn test_exclusive_or() {
    assert_eq!(0.0, gen::exclusive_or(0.0, 0.0));
    assert_eq!(0.0, gen::exclusive_or(-1.2, -0.8));
    assert_eq!(0.0, gen::exclusive_or(1.8, 2.0));
    assert_eq!(1.0, gen::exclusive_or(0.02, -9.0));
    assert_eq!(1.0, gen::exclusive_or(-10.0, 12.0));
    assert_eq!(1.0, gen::exclusive_or(-1.0e-32, 1.0e-32));
}

#[test]
fn test_signum_and_abs() {
    let mut abs = 0.0;
    assert_eq!(0.0, gen::signum_and_abs(0.0, &mut abs));
    assert_eq!(0.0, abs);

    assert_eq!(0.0, gen::signum_and_abs(-0.0, &mut abs));
    assert_eq!(0.0, abs);

    assert_eq!(1.0, gen::signum_and_abs(1.0e-16, &mut abs));
    assert_eq!(1.0e-16, abs);

    assert_eq!(-1.0, gen::signum_and_abs(-1.0e-16, &mut abs));
    assert_eq!(1.0e-16, abs);

    assert_eq!(1.0, gen::signum_and_abs(9.8, &mut abs));
    assert_eq!(9.8, abs);

    assert_eq!(-1.0, gen::signum_and_abs(-12.1, &mut abs));
    assert_eq!(12.1, abs);
}

#[test]
fn test_nested_conditionals_1() {
    assert_eq!(1.9875135408080455, gen::nested_conditionals_1(0.5, 0.2));
    assert_eq!(0.2127659962913584, gen::nested_conditionals_1(0.1, 0.3));
    assert_eq!(2.664161305696031, gen::nested_conditionals_1(2.4, -0.11));
    assert_eq!(0.20636916796204946, gen::nested_conditionals_1(1.3, -3.0));
    assert_eq!(1.6620319900924367, gen::nested_conditionals_1(-0.8, 0.66));
    assert_eq!(
        0.35166057729244216,
        gen::nested_conditionals_1(-0.123, -0.5)
    );
}

#[test]
fn test_nested_conditionals_2() {
    assert_eq!(0.998948281966456, gen::nested_conditionals_2(0.73, 0.02));
    assert_eq!(2.093418257300724, gen::nested_conditionals_2(1.32, 1.32));
    assert_eq!(-21.598735511069624, gen::nested_conditionals_2(7.2, -7.0));
    assert_eq!(4.907137658554118, gen::nested_conditionals_2(5.6, -6.3));
    assert_eq!(33.78428079355372, gen::nested_conditionals_2(-1.0, 0.95));
    assert_eq!(0.0, gen::nested_conditionals_2(-1.7, -2.0));
}