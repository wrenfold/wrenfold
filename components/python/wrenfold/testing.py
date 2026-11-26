"""Numerical checks for derivatives of generated functions. Requires NumPy."""

import typing

import numpy as np
import numpy.typing as npt


def _real_array(value: npt.ArrayLike, name: str) -> npt.NDArray[np.float64]:
    array = np.asarray(value)
    if array.dtype.kind not in "iuf":
        raise ValueError(f"{name} must be real-valued, got {array.dtype}")
    return np.asarray(array, dtype=np.float64)


def _is_vector(array: np.ndarray) -> bool:
    return array.size > 0 and (array.ndim <= 1 or (array.ndim == 2 and 1 in array.shape))


def check_jacobian(
    func: typing.Callable[[typing.Any], npt.ArrayLike],
    jacobian: typing.Callable[[typing.Any], npt.ArrayLike],
    *,
    at: typing.Any,
    retract: typing.Callable[[typing.Any, npt.NDArray[np.float64]], typing.Any] | None = None,
    tangent_dim: int | None = None,
    step: npt.ArrayLike = 0.01,
    atol: float = 1e-8,
    rtol: float = 1e-6,
) -> None:
    """Check a Jacobian at one point using sixth-order central differences in float64.

    Args:
        func: Function returning a real scalar or vector (flat, row, or column).
        jacobian: Function returning a matrix of shape (output size, tangent dimension).
        at: Linearization point. With additive perturbations, supply a scalar or vector.
        retract: Optional function mapping the point and a flat tangent vector to a new point.
            Supply the same update used by the solver. The callback must leave the point intact.
        tangent_dim: Number of tangent coordinates. Required with a custom retraction.
        step: Positive finite scalar or one step per tangent coordinate. Each column samples
            the function at offsets of plus and minus one, two, and three steps.
        atol: Absolute tolerance for each entry.
        rtol: Relative tolerance, scaled by the magnitude of the analytic entry.

    Returns:
        None when all entries agree at the requested tolerances.

    Raises:
        ValueError: For invalid arguments or incompatible shapes.
        AssertionError: For non-finite evaluations or a derivative mismatch. A mismatch
            includes the estimate at half the step for the worst entry as a diagnostic.
    """
    if not (np.isfinite(atol) and atol >= 0 and np.isfinite(rtol) and rtol >= 0):
        raise ValueError("atol and rtol must be finite and non-negative")
    if tangent_dim is not None and (
        isinstance(tangent_dim, bool)
        or not isinstance(tangent_dim, (int, np.integer))
        or tangent_dim <= 0
    ):
        raise ValueError("tangent_dim must be a positive integer")

    if retract is None:
        point = _real_array(at, "at")
        if not _is_vector(point) or not np.isfinite(point).all():
            raise ValueError("at must be a finite scalar or vector for additive perturbations")
        if tangent_dim is not None and tangent_dim != point.size:
            raise ValueError("tangent_dim must match the input size for additive perturbations")
        tangent_dim = point.size
        at = point[()] if point.ndim == 0 else point

        def add_delta(x, delta: npt.NDArray[np.float64]):
            shifted = x + delta.reshape(point.shape)
            if np.array_equal(shifted, x):
                raise ValueError(f"step is too small to perturb the input by {delta}")
            return shifted

        retract = add_delta
    elif tangent_dim is None:
        raise ValueError("tangent_dim is required with a custom retraction")

    steps = _real_array(step, "step")
    if steps.ndim == 0:
        steps = np.full(tangent_dim, steps.item())
    if steps.shape != (tangent_dim,) or not (np.isfinite(steps) & (steps > 0)).all():
        raise ValueError("step must be positive and finite, with one value per tangent coordinate")

    output = _real_array(func(at), "func(at)")
    if not _is_vector(output):
        raise ValueError(f"func(at) must be a scalar or vector, got shape {output.shape}")
    if not np.isfinite(output).all():
        raise AssertionError("Non-finite output at the linearization point")
    analytic = _real_array(jacobian(at), "jacobian(at)").copy()
    shape = (output.size, tangent_dim)
    if analytic.shape != shape:
        raise ValueError(f"Expected Jacobian shape {shape}, got {analytic.shape}")
    if not np.isfinite(analytic).all():
        row, col = np.argwhere(~np.isfinite(analytic))[0]
        raise AssertionError(f"Non-finite analytic Jacobian at row {row}, column {col}")

    def sample(col: int, offset: float) -> np.ndarray:
        if not np.isfinite(offset):
            raise ValueError(f"Perturbation overflows at column {col}")
        delta = np.zeros(tangent_dim)
        delta[col] = offset
        value = _real_array(func(retract(at, delta)), "func output")
        if value.shape != output.shape:
            raise ValueError(f"Output shape changed from {output.shape} to {value.shape}")
        if not np.isfinite(value).all():
            raise AssertionError(f"Non-finite output at column {col}, offset {offset:+g}")
        # A callback may reuse its output buffer on the next evaluation.
        return value.flatten()

    def estimate_column(col: int, h: float) -> np.ndarray:
        # Same six-point stencil as wf::numerical_derivative.
        differences = [sample(col, k * h) - sample(col, -k * h) for k in (1, 2, 3)]
        d1, d2, d3 = differences
        estimate = (0.75 * d1 - 0.15 * d2 + d3 / 60) / h
        if not np.isfinite(estimate).all():
            raise AssertionError(f"Non-finite numerical derivative at column {col}, step {h:g}")
        return estimate

    numerical = np.column_stack([estimate_column(col, h) for col, h in enumerate(steps)])
    try:
        with np.errstate(over="raise", invalid="raise"):
            tolerance = atol + rtol * np.abs(analytic)
            excess = np.abs(analytic - numerical) - tolerance
    except FloatingPointError as error:
        raise AssertionError("Jacobian comparison exceeds float64 range") from error
    row, col = np.unravel_index(np.argmax(excess), shape)
    if excess[row, col] > 0:
        h = steps[col]
        try:
            refined = f"{estimate_column(int(col), h / 2)[row]:.12g}"
        except (AssertionError, ValueError, ArithmeticError) as error:
            refined = f"unavailable ({error})"
        raise AssertionError(
            f"Jacobian mismatch at row {row}, column {col}: analytic={analytic[row, col]:.12g}, "
            f"numerical={numerical[row, col]:.12g}, step={h:g}, tolerance={tolerance[row, col]:g}, "
            f"numerical at half step={refined}"
        )
