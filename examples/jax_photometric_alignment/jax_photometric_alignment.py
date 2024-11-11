"""
Demonstration of direct photometric alignment implemented in JAX, using wrenfold to generate the
Jacobians we need.
"""

import argparse

import jax
import jax.numpy as jnp
import numpy as np

from wrenfold import code_generation, geometry, sym, type_annotations

from ..shared_expressions import kb_camera_projection, kb_camera_unprojection
from .utils import (
    build_image_pyramid,
    create_src_pts_array,
    load_gzipped_image,
)

IMAGE_SHAPE = (480, 640)

# Camera is a Kannala-Brandt camera with an equidistant profile.
INTRINSICS_K = np.array([280.0, 280.0, 319.5, 239.5], dtype=np.float32).reshape([-1, 1])
INTRINSICS_COEFFS = np.array([0.0, 0.0, 0.0, 0.0], dtype=np.float32).reshape([-1, 1])


def remap_point_symbolic(
    dest_R_source_wxyz: type_annotations.Vector4,
    dest_t_source: type_annotations.Vector3,
    v_src: type_annotations.Vector3,
    inverse_range: type_annotations.FloatScalar,
    K: type_annotations.Vector4,
    coeffs: type_annotations.Vector4,
):
    """
    Transform and project a source point (parameterized by a unit vector + inverse range) into
    a destination camera.

    Args:
      dest_R_source_wxyz: Quaternion elements of the rotation `dest_R_source`.
      dest_t_source: Translation vector from source --> dest in dest frame.
      v_src: Unit vector pointing from source camera origin to the point, in source frame.
      inverse_range: Inverse range along `v_src` to the point in source frame.
      K: Intrinsic parameters, ordered [fx, fy, cx, cy].
      coeffs: Distortion coefficients of the KB camera model.

    Returns:
      * Remapped pixel coordinates in the destination camera (`p_dest_pixels`).
      * The incident ray angle wrt the optical axis, theta.
      * Jacobians of `p_dest_pixels` wrt `dest_R_source` and `dest_t_source`.
    """
    dest_Q_src = geometry.Quaternion.from_wxyz(dest_R_source_wxyz)
    dest_R_src = dest_Q_src.to_rotation_matrix()

    # Compute the point in the destination camera, scaled by inverse_range.
    p_dest_cam_scaled = dest_R_src * v_src + dest_t_source * inverse_range

    # Project into pixel coordinates in the destination camera.
    p_dest_pixels, theta = kb_camera_projection(p_cam=p_dest_cam_scaled, K=K, coeffs=coeffs)

    # Compute jacobians wrt tangent-space rotation + translation.
    p_dest_D_rotation = sym.jacobian(p_dest_pixels, dest_R_source_wxyz) * \
        dest_Q_src.right_retract_derivative()

    p_dest_D_translation = sym.jacobian(p_dest_pixels, dest_t_source)

    return [
        code_generation.ReturnValue(p_dest_pixels),
        code_generation.OutputArg(theta, name="theta"),
        code_generation.OutputArg(p_dest_D_rotation, name="p_dest_D_rotation"),
        code_generation.OutputArg(p_dest_D_translation, name="p_dest_D_translation")
    ]


def main(args: argparse.Namespace):
    src_gray = load_gzipped_image("src_gray.gz", dtype=np.uint8, shape=IMAGE_SHAPE)
    src_inverse_range = load_gzipped_image("src_inv_range.gz", dtype=np.uint16, shape=IMAGE_SHAPE)
    dest_gray = load_gzipped_image("dest_gray.gz", dtype=np.uint8, shape=IMAGE_SHAPE)

    # Convert loaded inverse range to 1/meters.
    # A value of 0u16 is 0.0m^-1, while a value of 65535u16 is 10m^-1 (0.1 meters).
    src_inverse_range = (src_inverse_range.astype(np.float32) / 65535.0) * 10.0

    # Generate a JAX method to compute the source-camera unit-vector rays:
    unproject_pt, _ = code_generation.generate_python(
        func=kb_camera_unprojection, target=code_generation.PythonGeneratorTarget.JAX)
    unproject_pt_jit = jax.jit(jax.vmap(unproject_pt, in_axes=(0, None, None), out_axes=0))

    # Compute the source camera rays:
    p_src_pixels = create_src_pts_array(shape=src_gray.shape)
    v_src = unproject_pt_jit(p_src_pixels[..., np.newaxis], INTRINSICS_K, INTRINSICS_COEFFS)

    # Generate a JAX method to remap the source-camera rays (and inverse ranges) into the
    # destination image.
    remap_pt, remap_code = code_generation.generate_python(
        func=remap_point_symbolic, target=code_generation.PythonGeneratorTarget.JAX)
    if args.print_code:
        print('-- Generated code:')
        print(remap_code)

    # Then batch over the point-parameters and jit:
    remap_pt_jit = jax.jit(jax.vmap(remap_pt, in_axes=(None, None, 0, 0, None, None), out_axes=0))

    src_pyramid = build_image_pyramid(src_gray)
    dest_pyramid = build_image_pyramid(dest_gray)

    # TODO: Using a gaussian pyramid on inverse-range might be a bit debatable here. Maybe
    # max-pooling is better, but for the same of simplicity just blur it. We'll refine at
    # lower pyramid levels anyways.
    src_inv_range_pyramid = build_image_pyramid(src_inverse_range, output_dtype=jnp.float32)

    import ipdb
    ipdb.set_trace()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--visualize", action="store_true", help="Visualize the outputs")
    parser.add_argument('--print-code', action='store_true', help='Print the generated code')
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
