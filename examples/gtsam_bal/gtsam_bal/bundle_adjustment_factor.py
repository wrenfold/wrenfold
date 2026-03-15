"""
Generate a GTSAM reprojection error for use with BAL datasets.
"""

import dataclasses
import typing as T
from pathlib import Path

from wrenfold import ast, code_generation, sym, type_info
from wrenfold.type_annotations import FloatScalar, Vector2, Vector3

from ..pose3 import Pose3, Rot3


@dataclasses.dataclass
class Cal3Bundler:
    """
    Corresponds to type gtsam::Cal3Bundler (a 3 DOF Snavely camera model).
    """

    f: FloatScalar
    k1: FloatScalar
    k2: FloatScalar

    def to_vector(self) -> sym.MatrixExpr:
        return sym.vector(self.f, self.k1, self.k2)

    def project(self, p: Vector3) -> Vector2:
        """
        Project and apply intrinsic model.

        We could improve this by adding a conditional here that handles p[2] <= 0.
        """
        xp = p[0] / p[2]
        yp = p[1] / p[2]

        r2 = xp * xp + yp * yp
        distortion = 1 + r2 * (self.k1 + self.k2 * r2)
        return sym.vector(self.f * distortion * xp, self.f * distortion * yp)


@dataclasses.dataclass
class SfmCamera:
    """
    Corresponds to type gtsam::SfmCamera.
    """

    pose: Pose3
    calibration: Cal3Bundler

    def to_vector(self) -> sym.MatrixExpr:
        return sym.vstack([self.pose.to_vector(), self.calibration.to_vector()])


class GtsamCppGenerator(code_generation.CppGenerator):
    """
    Customize the code-generation to work with GTSAM types.
    """

    def format_get_field(self, element: ast.GetField) -> str:
        """
        Customize access to `gtsam::Pose3` and `Cal3Bundler`.
        """
        if (element.struct_type.python_type == Pose3 and element.field_name == "rotation"):
            # Retrieve Rot3, and then the Eigen Quaternion:
            return f"{self.format(element.arg)}.rotation().toQuaternion()"
        elif (element.struct_type.python_type == Cal3Bundler and element.field_name == "f"):
            # fx() == fy() for the Cal3Bundler type:
            return f"{self.format(element.arg)}.fx()"

        return f"{self.format(element.arg)}.{element.field_name}()"

    def format_custom_type(self, element: type_info.CustomType) -> str:
        """Place types into the `gtsam` namespace."""
        if element.python_type in [Pose3, Rot3, Cal3Bundler, SfmCamera]:
            return f"gtsam::{element.name}"
        return self.super_format(element)


def bundle_adjustment_factor(camera: SfmCamera, p_world: Vector3):
    """
    Transforms Euclidean point `p_world` into the frame of `camera`, and then projects it
    using a simplified intrinsic model with three parameters [f, k1, k2].
    """
    # Transform the point from world to camera:
    world_T_camera = camera.pose
    p_cam = world_T_camera.rotation.rotation_matrix().T * (p_world - world_T_camera.translation)

    # Project and compute projection error
    p_image = camera.calibration.project(p_cam)

    # Compute jacobian wrt the flattened pose and intrinsic model:
    p_image_D_camera = sym.jacobian(p_image, camera.to_vector())
    assert p_image_D_camera.shape == (2, 10)

    # Convert it to be with respect to the right-tangent space of the camera pose:
    p_image_D_camera_tangent = p_image_D_camera * sym.diag(
        [camera.pose.right_retract_derivative(), sym.eye(3)])
    assert p_image_D_camera_tangent.shape == (2, 9)

    # Compute jacobian wrt the point:
    p_image_D_point = sym.jacobian(p_image, p_world)

    return (
        code_generation.OutputArg(p_image, "p_image"),
        code_generation.OutputArg(p_image_D_camera_tangent, "p_image_D_camera", is_optional=True),
        code_generation.OutputArg(p_image_D_point, "p_image_D_point", is_optional=True),
    )


def main():
    generator = GtsamCppGenerator()
    code = code_generation.generate_function(func=bundle_adjustment_factor, generator=generator)
    code = generator.apply_preamble(code, namespace="gen")
    output_path = (Path(__file__).parent.absolute() / "generated" / "bundle_adjustment_factor.h")
    code_generation.mkdir_and_write_file(code=code, path=output_path)


if __name__ == "__main__":
    main()
