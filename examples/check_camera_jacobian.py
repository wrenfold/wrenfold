"""Check the projection Jacobian of a pinhole camera using right pose perturbations."""

import numpy as np
import wrenfold as wf
from wrenfold import sym
from wrenfold.testing import check_jacobian


def pinhole(camera_T_world: wf.Matrix4, point: wf.Vector3):
    """Project a world point, with pose tangent coordinates ordered rotation then translation."""
    p = camera_T_world[:3, :3] * point + camera_T_world[:3, 3]
    pixel = sym.vector(400 * p[0] / p[2], 400 * p[1] / p[2])

    # A right pose increment acts on the world point before camera_T_world.
    # The first-order displacement is omega x point + translation.
    x, y, z = point
    rotation_block = sym.matrix([[0, z, -y], [-z, 0, x], [y, -x, 0]])
    J_pose = pixel.jacobian(point) * sym.hstack([rotation_block, sym.eye(3)])
    return wf.ReturnValue(pixel), wf.OutputArg(J_pose, "J_pose", is_optional=True)


def pose_increment(delta):
    """Cayley retraction, with the same first derivative at zero as the SE(3) exponential."""
    wx, wy, wz = delta[:3]
    twist = np.zeros((4, 4))
    twist[:3, :3] = [[0, -wz, wy], [wz, 0, -wx], [-wy, wx, 0]]
    twist[:3, 3] = delta[3:]
    return np.linalg.solve(np.eye(4) - twist / 2, np.eye(4) + twist / 2)


def camera_sample():
    project, _ = wf.generate_python(pinhole)
    point = np.array([0.4, -0.3, 3.0])
    pose = np.array([[0, -1, 0, 0.2], [1, 0, 0, -0.1], [0, 0, 1, 0.5], [0, 0, 0, 1.0]])

    def pixel(T):
        return project(T, point, compute_J_pose=False)[0]

    def jacobian(T):
        return project(T, point, compute_J_pose=True)[1]

    return pose, pixel, jacobian


if __name__ == "__main__":
    pose, pixel, jacobian = camera_sample()
    check_jacobian(
        pixel,
        jacobian,
        at=pose,
        retract=lambda T, delta: T @ pose_increment(delta),
        tangent_dim=6,
    )
