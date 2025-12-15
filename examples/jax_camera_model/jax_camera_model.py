"""
Code generate a camera model in JAX and test it numerically.
"""

import typing as T

import jax
import jax.numpy as jnp
import numpy as np
from wrenfold import code_generation

from ..shared_expressions import (
    kb_camera_projection_with_jacobians,
    kb_camera_unprojection_with_jacobians,
)


def get_test_camera_coeffs() -> list[tuple[jnp.ndarray, float]]:
    """Get test coefficients for some representative camera models."""
    coeffs = [
        ([0.08327424, 0.00852979, 0.00063325, 0.00017048], 1.99),
        ([0.0, 0.0, 0.0, 0.0], jnp.pi / 2),
        ([-4.16666666e-02, 5.20833056e-04, -3.09988000e-06, 1.06158708e-08], 1.41),
        ([-1.66666587e-01, 8.33305775e-03, -1.98095183e-04, 2.60641004e-06], 0.99),
    ]
    return [(jnp.asarray(k).reshape([-1, 1]), max_radius) for (k, max_radius) in coeffs]


def generate_kb_camera_model_functions() -> tuple[T.Callable, T.Callable, T.Callable, T.Callable]:
    """
    Generate forward and backward projection functions for the Kannala-Brandt camera model.

    We call ``generate_python``, which returns a callable that operates on JAX types.
    """
    project, _ = code_generation.generate_python(
        func=kb_camera_projection_with_jacobians,
        generator=code_generation.PythonGenerator(target=code_generation.PythonGeneratorTarget.JAX),
    )
    unproject, _ = code_generation.generate_python(
        func=kb_camera_unprojection_with_jacobians,
        generator=code_generation.PythonGenerator(target=code_generation.PythonGeneratorTarget.JAX),
    )

    # Batch over points (first argument), and then JIT with xla.
    # We need to specify which derivatives we want here, since `vmap` does not know what to do with
    # these boolean arguments.
    project_batched = jax.vmap(
        lambda *args: project(
            *args,
            compute_p_pixels_D_p_cam=True,
            compute_p_pixels_D_K=True,
            compute_p_pixels_D_coeffs=True,
        ),
        in_axes=(0, None, None),
        out_axes=0,
    )

    unproject_batched = jax.vmap(
        lambda *args: unproject(
            *args,
            compute_p_cam_D_p_pixels=True,
            compute_p_cam_D_K=True,
            compute_p_cam_D_coeffs=True,
        ),
        in_axes=(0, None, None),
        out_axes=0,
    )

    project_no_jacobians = jax.vmap(
        lambda *args: project(
            *args,
            compute_p_pixels_D_p_cam=False,
            compute_p_pixels_D_K=False,
            compute_p_pixels_D_coeffs=False,
        )[0],
        in_axes=(0, None, None),
        out_axes=0,
    )

    unproject_no_jacobians = jax.vmap(
        lambda *args: unproject(
            *args,
            compute_p_cam_D_p_pixels=False,
            compute_p_cam_D_K=False,
            compute_p_cam_D_coeffs=False,
        )[0],
        in_axes=(0, None, None),
        out_axes=0,
    )

    return (
        jax.jit(project_batched),
        jax.jit(unproject_batched),
        jax.jit(project_no_jacobians),
        jax.jit(unproject_no_jacobians),
    )


def get_pixel_coords(shape: tuple[int, int]) -> jnp.ndarray:
    """
    Get pixel coordinates for every pixel in an image.

    Args:
      * shape: (rows, cols)
    """
    x, y = jnp.mgrid[0 : shape[1], 0 : shape[0]]
    return jnp.stack([x.reshape(-1), y.reshape(-1)], axis=-1).reshape(-1, 2, 1)


def test_kb_camera_model_methods():
    """
    Numerical tests of the wrenfold-generated and JAX-jitted Kannala-Brandt camera model.
    """
    project, unproject, project_no_jacobians, unproject_no_jacobians = (
        generate_kb_camera_model_functions()
    )

    image_w = 160
    image_h = 120
    cx = (image_w - 1) / 2.0
    cy = (image_h - 1) / 2.0
    pixel_radius = jnp.sqrt(cx**2 + cy**2)

    p_pixels_in = get_pixel_coords(shape=(image_h, image_w)).astype(jnp.float32)

    for coeffs, max_radius in get_test_camera_coeffs():
        focal_length = pixel_radius / max_radius
        K = jnp.array([focal_length * 1.02, focal_length * 1.08, cx, cy]).reshape(4, 1)

        # `unproject` will return a dict of tensors. For convenience build a namespace object so
        # that we can use `.member` syntax.
        p_cam, _p_cam_D_pixels, p_cam_D_K, p_cam_D_coeffs = unproject(p_pixels_in, K, coeffs)

        # project back the other way:
        p_pixels, _p_pixels_D_p_cam, p_pixels_D_K, p_pixels_D_coeffs = project(p_cam, K, coeffs)

        # check round-trip conversion produced the same coordinates
        np.testing.assert_allclose(desired=p_pixels_in, actual=p_pixels, atol=1.0e-4)

        # Test jacobians of against jacfwd:
        (p_pixels_D_K_jax, p_pixels_D_coeffs_jax) = [
            jnp.squeeze(J)
            for J in jax.jacfwd(project_no_jacobians, argnums=[1, 2], has_aux=False)(
                p_cam, K, coeffs
            )
        ]

        np.testing.assert_allclose(desired=p_pixels_D_K_jax, actual=p_pixels_D_K, rtol=1.0e-6)
        np.testing.assert_allclose(
            desired=p_pixels_D_coeffs_jax,
            actual=p_pixels_D_coeffs,
            rtol=1.0e-6,
        )

        # Jacobians of the backward/unproject method.
        (p_cam_D_K_jax, p_cam_D_coeffs_jax) = [
            jnp.squeeze(J)
            for J in jax.jacfwd(unproject_no_jacobians, argnums=[1, 2], has_aux=False)(
                p_pixels_in, K, coeffs
            )
        ]

        np.testing.assert_allclose(desired=p_cam_D_K_jax, actual=p_cam_D_K, atol=1.0e-6)
        np.testing.assert_allclose(
            desired=p_cam_D_coeffs_jax,
            actual=p_cam_D_coeffs,
            rtol=1.0e-5,
        )


if __name__ == "__main__":
    jax.config.update("jax_platform_name", "cpu")
    test_kb_camera_model_methods()
