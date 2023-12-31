//! Test generated custom types.

#[cfg(test)]
mod gen;

#[cfg(test)]
pub mod geo;

#[cfg(test)]
mod tests {
    use nalgebra as na;

    use wrenfold_test_utils::{numerical_jacobian, Manifold};

    use crate::gen;
    use crate::geo::{Point3d, Pose3d};

    impl Manifold<f64, 3> for Point3d {
        fn retract(&self, dx: &na::SVector<f64, 3>) -> Self {
            Self {
                x: self.x + dx.x,
                y: self.y + dx.y,
                z: self.z + dx.z,
            }
        }

        fn local_coordinates(&self, other: &Self) -> na::SVector<f64, 3> {
            na::vector![other.x - self.x, other.y - self.y, other.z - self.z]
        }
    }

    impl Manifold<f64, 6> for Pose3d {
        fn retract(&self, dx: &na::SVector<f64, 6>) -> Self {
            Self::new(
                self.rotation() * na::UnitQuaternion::new(dx.fixed_rows::<3>(0).into_owned()),
                self.translation() + dx.fixed_rows::<3>(3),
            )
        }

        fn local_coordinates(&self, other: &Self) -> na::SVector<f64, 6> {
            let rodrigues = (self.rotation().inverse() * other.rotation())
                .axis_angle()
                .map(|(axis, angle)| axis.into_inner() * angle)
                .unwrap_or_else(na::Vector3::zeros); //  Zero rotation case.
            let mut vec = na::Vector6::zeros();
            vec.fixed_rows_mut::<3>(0).copy_from(&rodrigues);
            vec.fixed_rows_mut::<3>(3)
                .copy_from(&(other.translation() - self.translation()));
            vec
        }
    }

    fn get_test_poses() -> Vec<Pose3d> {
        vec![
            Pose3d::new_rotation_vector(na::Vector3::zeros(), na::Vector3::zeros()),
            Pose3d::new_rotation_vector(na::vector![0.04, -0.02, 0.03], na::Vector3::zeros()),
            Pose3d::new_rotation_vector(
                na::vector![1.2, -0.821, 0.321],
                na::vector![-0.73, -4.3, 0.33],
            ),
            Pose3d::new_rotation_vector(
                na::vector![-0.331, 0.8521, -0.011],
                na::vector![5.1, -3.3, 0.55],
            ),
        ]
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_transform_point() {
        let p_body = Point3d {
            x: 0.813,
            y: -0.556,
            z: 0.77,
        };

        for world_T_body in get_test_poses() {
            let mut D_pose_gen = na::SMatrix::<f64, 3, 6>::zeros();
            let mut D_pt_gen = na::SMatrix::<f64, 3, 3>::zeros();

            let p_world = gen::transform_point(
                &world_T_body,
                &p_body,
                Some(&mut D_pose_gen),
                Some(&mut D_pt_gen),
            );

            approx::assert_abs_diff_eq!(
                world_T_body.rotation().to_rotation_matrix() * p_body.to_vector()
                    + world_T_body.translation(),
                p_world.to_vector(),
                epsilon = 1.0e-14
            );

            let D_pose_num = numerical_jacobian(
                &world_T_body,
                &|world_T_body: &Pose3d| {
                    gen::transform_point(
                        world_T_body,
                        &p_body,
                        None::<&mut na::SMatrix<f64, 3, 6>>,
                        None::<&mut na::SMatrix<f64, 3, 3>>,
                    )
                },
                0.01,
            );

            let D_pt_num = numerical_jacobian(
                &p_body,
                &|p_body: &Point3d| {
                    gen::transform_point(
                        &world_T_body,
                        p_body,
                        None::<&mut na::SMatrix<f64, 3, 6>>,
                        None::<&mut na::SMatrix<f64, 3, 3>>,
                    )
                },
                0.01,
            );

            approx::assert_abs_diff_eq!(D_pose_num, D_pose_gen, epsilon = 1.0e-12);
            approx::assert_abs_diff_eq!(D_pt_num, D_pt_gen, epsilon = 1.0e-12);
        }
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_compose_poses() {
        let poses = get_test_poses();
        for a_T_b in &poses {
            for b_T_c in &poses {
                let mut D_first = na::SMatrix::<f64, 6, 6>::zeros();
                let mut D_second = na::SMatrix::<f64, 6, 6>::zeros();

                let a_T_c =
                    gen::compose_poses(a_T_b, b_T_c, Some(&mut D_first), Some(&mut D_second));

                approx::assert_abs_diff_eq!(
                    a_T_c.rotation().to_rotation_matrix(),
                    (a_T_b.rotation() * b_T_c.rotation()).to_rotation_matrix(),
                    epsilon = 1.0e-12
                );
                approx::assert_abs_diff_eq!(
                    a_T_b.rotation().to_rotation_matrix() * b_T_c.translation()
                        + a_T_b.translation(),
                    a_T_c.translation(),
                    epsilon = 1.0e-12
                );

                let D_first_num = numerical_jacobian(
                    a_T_b,
                    &|a_T_b| {
                        gen::compose_poses(
                            a_T_b,
                            b_T_c,
                            None::<&mut na::SMatrix<f64, 6, 6>>,
                            None::<&mut na::SMatrix<f64, 6, 6>>,
                        )
                    },
                    0.01,
                );
                approx::assert_abs_diff_eq!(D_first_num, D_first, epsilon = 1.0e-12);

                let D_second_num = numerical_jacobian(
                    b_T_c,
                    &|b_T_c| {
                        gen::compose_poses(
                            a_T_b,
                            b_T_c,
                            None::<&mut na::SMatrix<f64, 6, 6>>,
                            None::<&mut na::SMatrix<f64, 6, 6>>,
                        )
                    },
                    0.01,
                );
                approx::assert_abs_diff_eq!(D_second_num, D_second, epsilon = 1.0e-12);
            }
        }
    }
}
