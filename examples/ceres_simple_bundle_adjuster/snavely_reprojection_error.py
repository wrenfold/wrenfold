"""
Generate the Snavely reprojection error via wrenfold.
"""

import argparse
from pathlib import Path

import wrenfold as wf
from wrenfold import sym
from wrenfold.geometry import Quaternion


def snavely_reprojection_error(camera: wf.Vector9, point: wf.Vector3, measured_xy: wf.Vector2):
    """
    Symbolic implementation of the ceres `SnavelyReprojectionError`.

    See `simple_bundle_adjuster.cc` for the original implementation in C++.

    Args:
        camera: A 9-DOF vector with the camera paramters:
          - 3-DOF rotation vector.
          - 3-DOF camera translation.
          - 3 intrinsic parameters in order: [focal, l1, l2]
        point: Euclidean point position.
        measured_xy: Measured location in the image.
    """

    # Transform the point to camera frame:
    camera_R_world = Quaternion.from_rotation_vector(
        camera[0:3], epsilon=sym.float_constant(1.0e-16)
    )

    p_camera = camera_R_world.to_rotation_matrix() * point + camera[3:6]

    # Project into image using Snavely convention (negative z axis):
    xp = -p_camera[0, 0] / p_camera[2, 0]
    yp = -p_camera[1, 0] / p_camera[2, 0]

    # Apply the camera intrinsics:
    focal, l1, l2 = camera[6:]

    r2 = xp * xp + yp * yp
    distortion = 1 + r2 * (l1 + l2 * r2)

    residuals = sym.vector(focal * distortion * xp, focal * distortion * yp) - measured_xy
    return (
        wf.OutputArg(residuals, "residuals"),
        wf.OutputArg(sym.jacobian(residuals, camera), "residuals_D_camera", is_optional=True),
        wf.OutputArg(sym.jacobian(residuals, point), "residuals_D_point", is_optional=True),
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "output_file",
        type=Path,
        help="Path to write the generated code to.",
    )
    args = parser.parse_args()

    generator = wf.CppGenerator()
    code = wf.generate_function(func=snavely_reprojection_error, generator=generator)
    code = generator.apply_preamble(code, namespace="gen")
    wf.mkdir_and_write_file(code=code, path=args.output_file)


if __name__ == "__main__":
    main()
