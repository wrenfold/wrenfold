"""
Symbolic expressions that are shared among the provided examples.
"""
import typing as T

from wrenfold import code_generation, sym, type_annotations


def kb_fisheye_distortion(theta: type_annotations.FloatScalar,
                          coeffs: type_annotations.Vector4) -> sym.Expr:
    """Evaluate the Kannala-Brandt fisheye distortion curve (theta -> radius)."""
    k1, k2, k3, k4 = coeffs
    radius = theta * (1 + k1 * theta ** 2 + k2 * theta ** 4 + k3 * theta ** 6 + k4 * theta ** 8)
    return radius


def kb_fisheye_invert_distortion(
    radius: type_annotations.FloatScalar,
    coeffs: type_annotations.Vector4,
    num_iters: int = 6,
) -> sym.Expr:
    """
    Invert `kb_fisheye_distortion` with newton solver.

    Args:
        radius: radius in the image plane.
        coeffs: Radial distortion coefficients [k1, k2, k3, 4].
        num_iters: Number of iterations to unroll.

    Tip:
        We symbolically unroll `num_iters` of iteration. You may wish to raise this number if the
        accuracy proves insufficient for your camera model.
    """
    assert num_iters > 0, f"num_iters = {num_iters}"

    theta = 0
    for iteration in range(0, num_iters):
        # Evaluate the forward projection model:
        theta_sym = sym.symbols("theta", nonnegative=True)
        r_predicted = kb_fisheye_distortion(theta=theta_sym, coeffs=coeffs)

        # Compute derivative wrt theta.
        r_D_theta = r_predicted.diff(theta_sym).subs(theta_sym, theta)

        error = r_predicted.subs(theta_sym, theta) - radius
        updated_theta = sym.max(theta - error / r_D_theta, 0)

        if iteration + 1 < num_iters:
            theta = sym.stop_derivative(updated_theta)
        else:
            # Only allow derivative to propagate on the final iteration.
            # In other words, we will linearize about the _solution_ of the solver.
            theta = updated_theta

    return theta


def kb_camera_projection(
    p_cam: type_annotations.Vector3,
    K: type_annotations.Vector4,
    coeffs: type_annotations.Vector4,
) -> T.Tuple[sym.MatrixExpr, sym.Expr]:
    """
    Evaluate the projection model of a Kannala-Brandt camera model. Only the 4 radial distortion
    coefficients are implemented in this example.

    Args:
        p_cam: Incident vector in camera coordinates. We assume it has non-zero norm.
        K: Intrinsic parameters as a 4-element vector [fx, fy, cx, cy].
        coeffs: Radial distortion coefficients [k1, k2, k3, 4].
    """
    # Angle with respect to the optical axis.
    # We could use acos() here and probably be a bit faster, although we would additionally need to
    # normalized `p_cam` as well.
    xy_norm = p_cam[0:2].norm()
    theta = sym.where(xy_norm > 0, sym.atan2(xy_norm, p_cam[2]), 0)

    # Angle in the image plane:
    phi = sym.where(xy_norm > 0, sym.atan2(p_cam[1], p_cam[0]), 0)

    # Distort theta with the radial model:
    r = kb_fisheye_distortion(theta=theta, coeffs=coeffs)

    # Compute image plane location:
    p_image = sym.vector(sym.cos(phi) * r, sym.sin(phi) * r, 1)

    # Convert to pixel coordinates
    fx, fy, cx, cy = K
    return sym.vector(fx * p_image[0] + cx, fy * p_image[1] + cy), theta


def kb_camera_unprojection(
    p_pixels: type_annotations.Vector2,
    K: type_annotations.Vector4,
    coeffs: type_annotations.Vector4,
):
    """
    Evaluate the un-projection model of a Kannala-Brandt camera model. This function computes the
    inverse (up to scale) of `kb_camera_projection`.

    Args:
        p_cam: Incident vector in camera coordinates.
        K: Intrinsic parameters as a 4-element vector [fx, fy, cx, cy].
        coeffs: Radial distortion coefficients [k1, k2, k3, 4].
    """
    fx, fy, cx, cy = K
    p_image = sym.vector((p_pixels[0] - cx) / fx, (p_pixels[1] - cy) / fy)
    radius = p_image.norm()

    # Convert to angle in radians.
    theta = kb_fisheye_invert_distortion(radius, coeffs)

    # Angle to the point in the image plane:
    phi = sym.where(radius > 0, sym.atan2(p_image[1], p_image[0]), 0)

    # Convert back to a unit vector:
    return sym.vector(sym.cos(phi) * sym.sin(theta), sym.sin(phi) * sym.sin(theta), sym.cos(theta))


def kb_camera_projection_with_jacobians(
    p_cam: type_annotations.Vector3,
    K: type_annotations.Vector4,
    coeffs: type_annotations.Vector4,
):
    """
    Code-generate the Kannala-Brandt camera model with Jacobians.
    """
    p_pixels, _ = kb_camera_projection(p_cam=p_cam, K=K, coeffs=coeffs)
    p_pixels_D_p_cam = sym.jacobian(p_pixels, p_cam)
    p_pixels_D_K = sym.jacobian(p_pixels, K)
    p_pixels_D_coeffs = sym.jacobian(p_pixels, coeffs)
    return [
        code_generation.OutputArg(p_pixels, name="p_pixels"),
        code_generation.OutputArg(p_pixels_D_p_cam, name="p_pixels_D_p_cam", is_optional=True),
        code_generation.OutputArg(p_pixels_D_K, name="p_pixels_D_K", is_optional=True),
        code_generation.OutputArg(p_pixels_D_coeffs, name="p_pixels_D_coeffs", is_optional=True),
    ]


def kb_camera_unprojection_with_jacobians(
    p_pixels: type_annotations.Vector2,
    K: type_annotations.Vector4,
    coeffs: type_annotations.Vector4,
):
    """
    Code-generate the Kannala-Brandt camera model with Jacobians.
    """
    p_cam = kb_camera_unprojection(p_pixels=p_pixels, K=K, coeffs=coeffs)
    p_cam_D_p_pixels = sym.jacobian(p_cam, p_pixels)
    p_cam_D_K = sym.jacobian(p_cam, K)
    p_cam_D_coeffs = sym.jacobian(p_cam, coeffs)

    return [
        code_generation.OutputArg(p_cam, name="p_cam"),
        code_generation.OutputArg(p_cam_D_p_pixels, name="p_cam_D_p_pixels", is_optional=True),
        code_generation.OutputArg(p_cam_D_K, name="p_cam_D_K", is_optional=True),
        code_generation.OutputArg(p_cam_D_coeffs, name="p_cam_D_coeffs", is_optional=True),
    ]
