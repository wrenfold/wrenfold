"""Tests for the public finite-difference Jacobian checker."""

import unittest

import numpy as np
from wrenfold.testing import check_jacobian

from examples.check_camera_jacobian import camera_sample, pose_increment


class JacobianTest(unittest.TestCase):
    def test_scalar_and_vector(self):
        # A degree-five polynomial is differentiated exactly by the six-point stencil.
        check_jacobian(
            lambda x: x**5, lambda x: [[5 * x**4]], at=1.25, step=0.2, atol=1e-12, rtol=0
        )
        for shape in ((2,), (1, 2), (2, 1)):
            point = np.array([0.5, -2.0]).reshape(shape)

            def func(x, shape=shape):
                self.assertEqual(x.shape, shape)
                a, b = x.flat
                return np.array([a * b, a**2 - b]).reshape(shape)

            check_jacobian(func, lambda x: [[-2, 0.5], [1, -1]], at=point, step=[1e-2, 1e-3])

    def test_generated_camera(self):
        pose, pixel, jacobian = camera_sample()

        def check(J, update):
            check_jacobian(pixel, J, at=pose, tangent_dim=6, retract=update)

        def right(T, delta):
            return T @ pose_increment(delta)

        check(jacobian, right)
        with self.assertRaisesRegex(AssertionError, "Jacobian mismatch"):
            check(lambda T: -jacobian(T), right)
        with self.assertRaisesRegex(AssertionError, "Jacobian mismatch"):
            check(lambda T: jacobian(T)[:, [1, 0, 2, 3, 4, 5]], right)
        with self.assertRaisesRegex(AssertionError, "Jacobian mismatch"):
            check(jacobian, lambda T, delta: pose_increment(delta) @ T)

    def test_mismatch_reports_worst_entry_and_rechecks_only_its_column(self):
        samples = []

        def func(x):
            samples.append(x.copy())
            return [x[0] + 2 * x[1], 3 * x[1]]

        with self.assertRaisesRegex(
            AssertionError,
            r"row 1, column 1: analytic=-3, numerical=3, step=0.1, tolerance=0.001, "
            r"numerical at half step=3",
        ):
            check_jacobian(
                func, lambda x: [[1, 1], [0, -3]], at=[0, 0], step=0.1, atol=1e-3, rtol=0
            )
        self.assertEqual(len(samples), 1 + 2 * 6 + 6)
        np.testing.assert_allclose(
            [x[1] for x in samples[-6:]], [0.05, -0.05, 0.1, -0.1, 0.15, -0.15]
        )

    def test_invalid_shapes_and_options(self):
        for options in (
            {"step": 0},
            {"step": [0.1]},
            {"step": [0.1, np.inf]},
            {"tangent_dim": 3},
            {"tangent_dim": -1},
            {"tangent_dim": 2.5},
            {"atol": -1},
            {"rtol": np.nan},
            {"retract": lambda x, dx: x + dx},
        ):
            with self.subTest(options=options), self.assertRaises(ValueError):
                check_jacobian(lambda x: x, lambda x: np.eye(2), at=[0, 1], **options)
        with self.assertRaisesRegex(ValueError, "Expected Jacobian shape"):
            check_jacobian(lambda x: x, lambda x: [1, 1], at=[0, 1])
        with self.assertRaisesRegex(ValueError, "Output shape changed"):
            check_jacobian(lambda x: [x] if x == 0 else [x, x], lambda x: [[1]], at=0)
        with self.assertRaisesRegex(ValueError, "real-valued"):
            check_jacobian(lambda x: 1j * x, lambda x: [[1]], at=0)

    def test_relative_tolerance_and_reused_output_buffer(self):
        buffer = np.empty(2)

        def func(x):
            buffer[:] = [1000 * x, 0]
            return buffer

        def jacobian(x):
            return [[1000.5], [1e-9]]

        check_jacobian(func, jacobian, at=0, atol=1e-8, rtol=1e-3)
        with self.assertRaisesRegex(AssertionError, "row 0, column 0"):
            check_jacobian(func, jacobian, at=0, atol=1e-8, rtol=1e-4)
        with self.assertRaisesRegex(AssertionError, "row 1, column 0"):
            check_jacobian(func, jacobian, at=0, atol=0, rtol=1e-3)

    def test_nonfinite_evaluations(self):
        with self.assertRaisesRegex(AssertionError, "linearization point"):
            check_jacobian(lambda x: np.inf, lambda x: [[1]], at=0)
        with self.assertRaisesRegex(AssertionError, r"analytic Jacobian at row 0, column 1"):
            check_jacobian(lambda x: 0, lambda x: [[1, np.nan]], at=[0, 1])
        with self.assertRaisesRegex(AssertionError, r"column 1, offset -0.02"):
            check_jacobian(lambda x: np.nan if x[1] < -0.01 else 0, lambda x: [[0, 0]], at=[0, 0])
        # A failed half-step diagnostic should preserve the original mismatch.
        with self.assertRaisesRegex(AssertionError, r"analytic=2, numerical=1.*offset \+0.5"):
            check_jacobian(lambda x: np.nan if x == 0.5 else x, lambda x: [[2]], at=0, step=1)


if __name__ == "__main__":
    unittest.main()
