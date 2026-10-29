"""
Symbolic expressions that are shared among the provided examples.
"""

import wrenfold as wf
from wrenfold import sym


def kb_fisheye_distortion(theta: wf.FloatScalar, coeffs: wf.Vector4) -> sym.Expr:
    """Evaluate the Kannala-Brandt fisheye distortion curve (theta -> radius)."""
    k1, k2, k3, k4 = coeffs.to_flat_list()
    radius = theta * (1 + k1 * theta**2 + k2 * theta**4 + k3 * theta**6 + k4 * theta**8)
    return radius


def kb_fisheye_invert_distortion(
    radius: wf.FloatScalar,
    coeffs: wf.Vector4,
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

    theta: sym.Expr = sym.zero
    for iteration in range(0, num_iters):
        # Evaluate the forward projection model:
        theta_sym = sym.symbol("theta", wf.NumberSet.RealNonNegative)
        r_predicted = kb_fisheye_distortion(theta=theta_sym, coeffs=coeffs)

        # Compute derivative wrt theta.
        r_D_theta = r_predicted.diff(theta_sym).subs(theta_sym, theta)

        error = r_predicted.subs(theta_sym, theta) - radius
        updated_theta = sym.max(theta - error / r_D_theta, 0)

        if iteration + 1 < num_iters:  # noqa: SIM108
            theta = sym.stop_derivative(updated_theta)
        else:
            # Only allow derivative to propagate on the final iteration.
            # In other words, we will linearize about the _solution_ of the solver.
            theta = updated_theta

    return theta


def kb_camera_projection(
    p_cam: wf.Vector3,
    K: wf.Vector4,
    coeffs: wf.Vector4,
):
    """
    Evaluate the projection model of a Kannala-Brandt camera model. Only the 4 radial distortion
    coefficients are implemented in this example. We accept an incident vector in camera coordinates
    and convert it to a location in pixels.

    Args:
        p_cam: Incident vector in camera coordinates. We assume it has non-zero norm.
        K: Intrinsic parameters as a 4-element vector [fx, fy, cx, cy].
        coeffs: Radial distortion coefficients [k1, k2, k3, 4].
    """
    # Angle with respect to the optical axis.
    # We could use acos() here and probably be a bit faster, although we would additionally need to
    # normalized `p_cam` as well.
    xy_squared_norm = p_cam[0:2].squared_norm()
    # Keep the inactive branch finite, including its derivatives when evaluated by JAX.
    xy_norm = sym.sqrt(sym.where(xy_squared_norm > 0, xy_squared_norm, 1))
    theta = sym.atan2(xy_norm, p_cam[2, 0])

    # Distort theta with the radial model:
    r = kb_fisheye_distortion(theta=theta, coeffs=coeffs)

    # Compute image plane location:
    # On the forward optical axis, r / xy_norm tends to 1 / z. Multiplying this
    # scale by x and y preserves the nonzero image-coordinate derivatives there.
    scale = sym.where(xy_squared_norm > 0, r, 1) / sym.where(
        xy_squared_norm > 0, xy_norm, p_cam[2, 0]
    )
    p_image = sym.vector(p_cam[0, 0] * scale, p_cam[1, 0] * scale)

    # Convert to pixel coordinates
    fx, fy, cx, cy = K.to_flat_list()
    return sym.vector(fx * p_image[0, 0] + cx, fy * p_image[1, 0] + cy)


def kb_camera_unprojection(
    p_pixels: wf.Vector2,
    K: wf.Vector4,
    coeffs: wf.Vector4,
):
    """
    Evaluate the un-projection model of a Kannala-Brandt camera model. This function computes the
    inverse (up to scale) of `kb_camera_projection`. We accept a point in pixels `p_pixels` and
    compute a unit-vector pointing out of the optical center.

    Args:
        p_cam: Incident vector in camera coordinates.
        K: Intrinsic parameters as a 4-element vector [fx, fy, cx, cy].
        coeffs: Radial distortion coefficients [k1, k2, k3, 4].
    """
    fx, fy, cx, cy = K.to_flat_list()
    p_image = sym.vector((p_pixels[0, 0] - cx) / fx, (p_pixels[1, 0] - cy) / fy)
    squared_radius = p_image.squared_norm()
    safe_radius = sym.sqrt(sym.where(squared_radius > 0, squared_radius, 1))
    radius = sym.where(squared_radius > 0, safe_radius, 0)

    # Convert to angle in radians.
    theta = kb_fisheye_invert_distortion(radius, coeffs)

    # sin(theta) / radius tends to one at the image center. Avoid polar coordinates,
    # whose angle and radial derivatives are singular at that point.
    scale = sym.where(squared_radius > 0, sym.sin(theta) / safe_radius, 1)
    return sym.vector(p_image[0, 0] * scale, p_image[1, 0] * scale, sym.cos(theta))


def kb_camera_projection_with_jacobians(
    p_cam: wf.Vector3,
    K: wf.Vector4,
    coeffs: wf.Vector4,
):
    """
    Define a symbolic function that implements the Kannala-Brandt camera model with Jacobians.

    This method implements the forward (projection) method.
    """
    p_pixels = kb_camera_projection(p_cam=p_cam, K=K, coeffs=coeffs)
    p_pixels_D_p_cam = sym.jacobian(p_pixels, p_cam)
    p_pixels_D_K = sym.jacobian(p_pixels, K)
    p_pixels_D_coeffs = sym.jacobian(p_pixels, coeffs)
    return [
        wf.OutputArg(p_pixels, name="p_pixels"),
        wf.OutputArg(p_pixels_D_p_cam, name="p_pixels_D_p_cam", is_optional=True),
        wf.OutputArg(p_pixels_D_K, name="p_pixels_D_K", is_optional=True),
        wf.OutputArg(p_pixels_D_coeffs, name="p_pixels_D_coeffs", is_optional=True),
    ]


def kb_camera_unprojection_with_jacobians(
    p_pixels: wf.Vector2,
    K: wf.Vector4,
    coeffs: wf.Vector4,
):
    """
    Define a symbolic function that implements the Kannala-Brandt camera model with Jacobians.

    This method implements the backwards (unprojection or back-projection) method.
    """
    p_cam = kb_camera_unprojection(p_pixels=p_pixels, K=K, coeffs=coeffs)
    p_cam_D_p_pixels = sym.jacobian(p_cam, p_pixels)
    p_cam_D_K = sym.jacobian(p_cam, K)
    p_cam_D_coeffs = sym.jacobian(p_cam, coeffs)

    return [
        wf.OutputArg(p_cam, name="p_cam"),
        wf.OutputArg(p_cam_D_p_pixels, name="p_cam_D_p_pixels", is_optional=True),
        wf.OutputArg(p_cam_D_K, name="p_cam_D_K", is_optional=True),
        wf.OutputArg(p_cam_D_coeffs, name="p_cam_D_coeffs", is_optional=True),
    ]
