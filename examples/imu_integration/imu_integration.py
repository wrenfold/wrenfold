"""
Example: IMU integration, as it might appear in pre-integrated estimation scheme.

This is one particular implementation of preintegration, included here as an example
and for the purpose of testing the code-generation framework.

In practice, there is a larger space of design choices you could make. For instance:
- Placing the tangent space in world frame instead of body.
- Incorporating rotational effects of the earth.
- Higher-order numerical integration schemes.
"""

import argparse
import typing as T

from wrenfold import code_generation
from wrenfold import sym
from wrenfold.geometry import Quaternion, left_jacobian_of_so3
from wrenfold.type_annotations import FloatScalar, Vector4, Vector3


def blockwise_jacobians(
    output_states: T.Iterable[T.Union[sym.MatrixExpr, Quaternion]],
    input_states: T.Iterable[T.Union[sym.MatrixExpr, Quaternion]],
) -> sym.MatrixExpr:
    jacobians = []
    for out_state in output_states:
        jacobians_row = []
        for in_state in input_states:
            out_expressions = (
                out_state.to_vector_wxyz() if isinstance(out_state, Quaternion) else out_state)
            in_expressions = (
                in_state.to_vector_wxyz() if isinstance(in_state, Quaternion) else in_state)

            out_D_in = sym.jacobian(out_expressions, in_expressions)
            if isinstance(out_state, Quaternion):
                out_D_in = out_state.right_local_coordinates_derivative() * out_D_in
            if isinstance(in_state, Quaternion):
                out_D_in = out_D_in * in_state.right_retract_derivative()
            jacobians_row.append(out_D_in)

        jacobians.append(sym.hstack(jacobians_row))

    return sym.vstack(jacobians)


def integrate_imu(
    i_R_j_xyzw: Vector4,
    i_p_j: Vector3,
    i_v_j: Vector3,
    gyro_bias: Vector3,
    accelerometer_bias: Vector3,
    angular_velocity: Vector3,
    linear_acceleration: Vector3,
    dt: FloatScalar,
):
    """
    We take an incremental navigation state represented by the 9DOF product of:
    - Rotation from frame i to j: i_R_j_xyzw (a scalar last quaternion)
    - Translation from frame i to j: i_p_j
    - Velocity delta from frame i to j: i_v_j

    Given a gyro and accelerometer measurement (plus their respective biases), we integrate
    the state forward.

    The updated state is returned, as well as the 9x9 Jacobian of the new state with respect
    to the previous one. In addition, we compute 9x6 jacobians of the new state with respect
    to the input measurements and sensor biases. These would typically be used to propagate
    forward your filter uncertainty.
    """

    # This operation does not produce a math operation - it can be thought of as a cast
    # from vector storage to a Quaternion.
    i_R_j: Quaternion = Quaternion.from_xyzw(i_R_j_xyzw)

    # Subtract biases from the input measurements:
    angular_velocity_unbiased = angular_velocity - gyro_bias
    linear_acceleration_unbiased = linear_acceleration - accelerometer_bias

    # Rotation from frame `j` (start of integration) to frame `k` (end of integration)
    angular_vel_times_dt = angular_velocity_unbiased * dt
    j_R_k: Quaternion = Quaternion.from_rotation_vector(angular_vel_times_dt, epsilon=1.0e-16)

    # Rotate the acceleration back into frame `i`:
    # Optionally, we can account for the integral of rotational motion. This adds operations, but
    # produces a more numerically correct result.
    account_for_rotation_over_interval = True
    if account_for_rotation_over_interval:
        j_R_k_integral = left_jacobian_of_so3(w=angular_vel_times_dt, epsilon=1.0e-16)
        accel_in_i = (i_R_j.to_rotation_matrix() * j_R_k_integral * linear_acceleration_unbiased)
    else:
        accel_in_i = i_R_j.to_rotation_matrix() * linear_acceleration_unbiased

    # Integrate position and velocity:
    i_p_k = i_p_j + i_v_j * dt + accel_in_i * (sym.abs(dt) * dt / 2)
    i_v_k = i_v_j + accel_in_i * dt

    # Integrate rotation:
    i_R_k = i_R_j * j_R_k

    # Determine jacobians:
    k_D_j = blockwise_jacobians(
        output_states=(i_R_k, i_p_k, i_v_k), input_states=(i_R_j, i_p_j, i_v_j))

    k_D_measurements = blockwise_jacobians(
        output_states=(i_R_k, i_p_k, i_v_k),
        input_states=(angular_velocity, linear_acceleration),
    )

    # Debatable whether you want to do this here, or just do it numerically after invoking the
    # generated code. I'll put it here for the sake of completeness in this example.
    k_D_bias = -k_D_measurements

    return [
        code_generation.OutputArg(i_R_k.normalized().to_vector_xyzw(), name="i_R_k"),
        code_generation.OutputArg(i_p_k, name="i_p_k"),
        code_generation.OutputArg(i_v_k, name="i_v_k"),
        code_generation.OutputArg(k_D_j, name="k_D_j", is_optional=True),
        code_generation.OutputArg(k_D_measurements, name="k_D_measurements", is_optional=True),
        code_generation.OutputArg(k_D_bias, name="k_D_bias", is_optional=True),
    ]


def compute_pim_delta_from_endpoints(
    world_R_i_xyzw: Vector4,
    world_t_i: Vector3,
    world_v_i: Vector3,
    world_R_k_xyzw: Vector4,
    world_t_k: Vector3,
    world_v_k: Vector3,
    duration: FloatScalar,
    gravity_world: Vector3,
) -> T.Tuple[Quaternion, Vector3, Vector3]:
    """
    Given two 9-DOF navigation states of the form (world_R_imu, world_t_imu, world_v_imu), compute what
    the preintegrated IMU measurements should be, assuming no noise or integration error.

    The first state (t0) is denoted as frame `i`, while the second (t1) is frame `k`.

    Returns the triplet: (i_R_k, i_t_k, i_v_k)
    """
    world_R_i: Quaternion = Quaternion.from_xyzw(world_R_i_xyzw)
    world_R_k: Quaternion = Quaternion.from_xyzw(world_R_k_xyzw)
    i_R_world = world_R_i.conjugate()
    i_R_world_mat = i_R_world.to_rotation_matrix()

    # Compute the estimated rotation delta from t0 -> t1:
    # Normalize for numerical stability.
    i_R_k_predicted = (i_R_world * world_R_k).normalized()

    # Compute the estimated velocity change between t0 and t1:
    # This expression can be derived from:
    #
    #  world_v_t1 = world_v_t0 + ∫ a_world(t) dt    (1)
    #
    # Where `∫ a_world(t) dt` is the integral of world-frame acceleration, not including
    # gravity (over the interval t0 --> t1). It can be written as:
    #
    #  ∫ a_world(t) dt = ∫ w_R_imu(t) * a_imu(t) dt
    #
    #                  = world_R_t0 * ∫ t0_R_imu(t) * a_imu(t) dt    (2)
    #
    # Where `a_imu(t)` is the acceleration in IMU frame, again not incorporating gravity.
    #
    # Meanwhile, the PIM integral of velocity can be written (ignoring biases):
    #
    #  t0_v_t1 = ∫ t0_R_imu(t) * world_R_imu(t)^T * (a_world(t) - g_world) dt
    #
    #          = -(world_R_t0^T * g_world) Δt + ∫ t0_R_imu(t) * a_imu(t) dt    (3)
    #
    # Where Δt is the duration t1 - t0, and g_world is approximately [0, 0, -9.81m/s^2].
    #
    # Then we can combine (1) and (2) to obtain:
    #
    #  world_v_t1 = world_v_t0 + world_R_t0 * ∫ t0_R_imu(t) * a_imu(t) dt   (4)
    #
    # Then we combine (3) and (4):
    #
    #  world_v_t1 = world_v_t0 + world_R_t0 * (t0_v_t1 + world_R_t0^T * g_world * Δt)
    #
    #             = world_v_t0 + (world_R_t0 * t0_v_t1) + g_world * Δt
    #
    # Or, after rearranging for `t0_v_t1`:
    #
    #  t0_v_t1 = world_R_t0^T * (world_v_t1 - world_R_t0 - g_world * Δt)
    #
    i_v_k = i_R_world_mat * (world_v_k - world_v_i - gravity_world * duration)

    # A similar derivation may be performed for position, albeit involving the double
    # integral of acceleration.
    half_dt2 = duration * sym.abs(duration) / 2
    i_t_k = i_R_world_mat * (
        world_t_k - world_t_i - world_v_i * duration - gravity_world * half_dt2)

    return i_R_k_predicted, i_t_k, i_v_k


def unweighted_imu_preintegration_error(
    world_R_i_xyzw: Vector4,
    world_t_i: Vector3,
    world_v_i: Vector3,
    world_R_k_xyzw: Vector4,
    world_t_k: Vector3,
    world_v_k: Vector3,
    i_R_k_measured_xyzw: Vector4,
    i_t_k_measured: Vector3,
    i_v_k_measured: Vector3,
    duration: FloatScalar,
    gravity_world: Vector3,
):
    """
    Given two 9-DOF navigation states of the form (world_R_imu, world_t_imu, world_v_imu), we implement
    a residual between the predicted delta between them, and the measured preintegrated delta.

    The first state is denoted as frame `i`, while the second is denoted as frame `k`. We first compute
    the idealized PIM delta between them, then subtract the actual preintegrated IMU measurements.

    A couple of important notes:
    1. This is the unweighted error. Typically, you would follow up this step by weighting the
       errors by the sqrt information of the IMU measurements themselves.
    2. This example does not account for the bias. It is assumed that the input preintegrated measurements
       have already been corrected, and the caller will propagate the jacobian onto the bias using
       `error_D_measurements` (an output of this function). As an alternative, you could choose to include
       that step here symbolically.
    """
    world_R_i: Quaternion = Quaternion.from_xyzw(world_R_i_xyzw)
    world_R_k: Quaternion = Quaternion.from_xyzw(world_R_k_xyzw)
    i_R_k_measured: Quaternion = Quaternion.from_xyzw(i_R_k_measured_xyzw)

    # Compute the predicted delta from the estimated end-point states:
    i_R_k_predicted, i_t_k_predicted, i_v_k_predicted = (
        compute_pim_delta_from_endpoints(
            world_R_i_xyzw,
            world_t_i,
            world_v_i,
            world_R_k_xyzw,
            world_t_k,
            world_v_k,
            duration,
            gravity_world,
        ))

    # Compute the difference and stack into a 9-DOF error vector:
    error = sym.vstack([
        (i_R_k_measured.conjugate() * i_R_k_predicted).to_rotation_vector(epsilon=1.0e-16),
        i_t_k_predicted - i_t_k_measured,
        i_v_k_predicted - i_v_k_measured,
    ])

    error_D_rotation_i = (
        error.jacobian(world_R_i.to_vector_wxyz()) * world_R_i.right_retract_derivative())
    error_D_rotation_k = (
        error.jacobian(world_R_k.to_vector_wxyz()) * world_R_k.right_retract_derivative())

    # Compute the jacobian of the error wrt the PIM itself. In practice, you need this term
    # for the preintegrated bias correction:
    stacked_measurements = sym.vstack(
        [i_R_k_measured.to_vector_wxyz(), i_t_k_measured, i_v_k_measured])
    error_D_measurements = error.jacobian(stacked_measurements)

    # `error_D_rotation_delta` is specified in terms of the [w,x,y,z] quaternion elements.
    # Convert it to the tangent space of `i_R_k_measured`:
    diag_blocks = [i_R_k_measured.right_retract_derivative(), sym.eye(3), sym.eye(3)]
    error_D_measurements = error_D_measurements * sym.diag(diag_blocks)

    # Compute the errors, and jacobians wrt estimated states:
    return [
        code_generation.OutputArg(error, name="error"),
        code_generation.OutputArg(error_D_rotation_i, name="error_D_rotation_i", is_optional=True),
        code_generation.OutputArg(
            error.jacobian(world_t_i), name="error_D_translation_i", is_optional=True),
        code_generation.OutputArg(
            error.jacobian(world_v_i), name="error_D_velocity_i", is_optional=True),
        code_generation.OutputArg(error_D_rotation_k, name="error_D_rotation_k", is_optional=True),
        code_generation.OutputArg(
            error.jacobian(world_t_k), name="error_D_translation_k", is_optional=True),
        code_generation.OutputArg(
            error.jacobian(world_v_k), name="error_D_velocity_k", is_optional=True),
        code_generation.OutputArg(
            error_D_measurements, name="error_D_measurements", is_optional=True),
    ]


def integration_test_sequence(t: FloatScalar):
    """
    A motion sequence parameterized as a function of time `t`. We use this as a test sequence
    for evaluating our IMU integration code.

    This trajectory is not particularly exciting.
    """
    position = sym.vector(3 * t * t + 2, sym.sin(2 * sym.pi * t * 0.5), 0)
    velocity = position.diff(t)
    acceleration = velocity.diff(t)

    # Constant angular velocity about each rotational axis:
    world_R_body: Quaternion = (
        Quaternion.from_x_angle(0.5 * t) * Quaternion.from_y_angle(-0.2 * t) *
        Quaternion.from_z_angle(0.3 * t))
    world_R_body_mat = world_R_body.to_rotation_matrix()

    # Recover the body-frame angular velocity as a function of time:
    # dR/dt = R * skew(w) --> skew(w) = R^T * (dR/dt)
    skew_w = world_R_body_mat.transpose() * world_R_body_mat.diff(t)
    angular_velocity_body = sym.vector(-skew_w[1, 2], skew_w[0, 2], -skew_w[0, 1])

    # Compute body-frame measured acceleration, including force from gravity:
    g = 9.80665
    linear_acceleration_body = world_R_body_mat.transpose() * (acceleration + sym.vector(0, 0, g))

    return [
        code_generation.OutputArg(world_R_body.to_vector_xyzw(), name="world_R_body"),
        code_generation.OutputArg(position, name="world_t_body"),
        code_generation.OutputArg(velocity, name="world_v_body"),
        code_generation.OutputArg(angular_velocity_body, name="angular_velocity_body"),
        code_generation.OutputArg(linear_acceleration_body, name="linear_acceleration_body"),
    ]


def main(args: argparse.Namespace):
    code = str()
    for function in [
            integrate_imu,
            unweighted_imu_preintegration_error,
            integration_test_sequence,
    ]:
        code += code_generation.generate_function(
            function, generator=code_generation.CppGenerator())
        code += "\n\n"

    code = code_generation.CppGenerator.apply_preamble(code, namespace="gen")
    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
