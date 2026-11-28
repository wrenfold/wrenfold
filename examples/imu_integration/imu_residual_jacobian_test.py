"""Check the generated IMU residual against perturbations of the preintegrated measurement."""

import unittest

import numpy as np
import wrenfold as wf
from wrenfold.testing import check_jacobian

from .imu_integration import unweighted_imu_preintegration_error


def retract_measurement(measurement, delta):
    rotation, position, velocity = measurement
    # Right-multiply the scalar-last quaternion by exp(delta_rotation).
    half_angle = np.linalg.norm(delta[:3]) / 2
    increment_xyz = 0.5 * np.sinc(half_angle / np.pi) * delta[:3]
    increment_w = np.cos(half_angle)
    xyz, w = rotation[:3], rotation[3]
    rotated = np.r_[
        increment_w * xyz + w * increment_xyz + np.cross(xyz, increment_xyz),
        w * increment_w - xyz @ increment_xyz,
    ]
    return rotated, position + delta[3:6], velocity + delta[6:9]


class ImuResidualJacobianTest(unittest.TestCase):
    def test_error_D_measurements(self):
        residual, _ = wf.generate_python(unweighted_imu_preintegration_error)
        measurement = (
            np.array([0.5, 0.5, -0.5, 0.5]),
            np.array([0.2, 0.5, 0.1]),
            np.array([0.7, -0.4, 0.1]),
        )

        def evaluate(sample, *, with_jacobian):
            rotation, position, velocity = sample
            return residual(
                world_R_i_xyzw=np.array([0.0, 0.0, 0.6, 0.8]),
                world_t_i=np.array([1.0, 2.0, -3.0]),
                world_v_i=np.array([0.4, 0.2, 0.5]),
                world_R_k_xyzw=np.array([0.0, 0.8, 0.0, 0.6]),
                world_t_k=np.array([3.0, 1.0, -2.0]),
                world_v_k=np.array([-0.1, 0.4, 0.2]),
                i_R_k_measured_xyzw=rotation,
                i_t_k_measured=position,
                i_v_k_measured=velocity,
                duration=0.3,
                gravity_world=np.array([0.0, 0.0, -9.81]),
                compute_error_D_rotation_i=False,
                compute_error_D_translation_i=False,
                compute_error_D_velocity_i=False,
                compute_error_D_rotation_k=False,
                compute_error_D_translation_k=False,
                compute_error_D_velocity_k=False,
                compute_error_D_measurements=with_jacobian,
            )

        check_jacobian(
            lambda sample: evaluate(sample, with_jacobian=False)[0],
            lambda sample: evaluate(sample, with_jacobian=True)[-1],
            at=measurement,
            retract=retract_measurement,
            tangent_dim=9,
            atol=1e-11,
            rtol=1e-10,
        )


if __name__ == "__main__":
    unittest.main()
