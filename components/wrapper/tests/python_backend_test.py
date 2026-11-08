"""Runtime contracts exercised through the public Python generator and formatter overrides."""

import contextlib
import unittest
import warnings

import numpy as np
import wrenfold as wf
from wrenfold import exceptions, external_functions, sym

try:
    import jax
except ImportError:
    jax = None

try:
    import torch
except ImportError:
    torch = None


class BackendContracts:
    """The same generated functions must honor each backend's dtype and shape conventions."""

    def generate(self, func, width=wf.PythonGeneratorFloatWidth.Float64, **kwargs):
        return wf.generate_python(
            func, generator=wf.PythonGenerator(target=self.TARGET, float_width=width), **kwargs
        )[0]

    def test_constants_and_float_widths(self):
        def mixed(x: wf.FloatScalar):
            return sym.vector(x, 0.5, 3, sym.pi, sym.E)

        def constants():
            return sym.vector(0.5, 3, sym.pi, sym.E)

        for width, dtype in (
            (wf.PythonGeneratorFloatWidth.Float32, np.float32),
            (wf.PythonGeneratorFloatWidth.Float64, np.float64),
        ):
            with self.subTest(width=width):
                actual = np.asarray(self.generate(mixed, width)(2.0))
                self.assertEqual(dtype, actual.dtype)
                np.testing.assert_allclose(actual[:, 0], [2, 0.5, 3, np.pi, np.e], rtol=1e-6)
                actual = np.asarray(self.generate(constants, width)())
                self.assertEqual(dtype, actual.dtype)
                np.testing.assert_allclose(actual[:, 0], [0.5, 3, np.pi, np.e], rtol=1e-6)

    def test_math_on_constants(self):
        def constants():
            return sym.vector(sym.sin(sym.pi / 3), sym.sqrt(sym.pi))

        for width, dtype in (
            (wf.PythonGeneratorFloatWidth.Float32, np.float32),
            (wf.PythonGeneratorFloatWidth.Float64, np.float64),
        ):
            with self.subTest(width=width):
                actual = np.asarray(self.generate(constants, width)())
                self.assertEqual(dtype, actual.dtype)
                np.testing.assert_allclose(
                    actual[:, 0], [np.sin(np.pi / 3), np.sqrt(np.pi)], rtol=1e-6
                )

    def test_integer_width(self):
        def checks(value: wf.IntScalar):
            return sym.vector(
                sym.iverson(sym.eq(value, 2**40)),
                sym.iverson(sym.eq(value, -(2**63))),
                sym.iverson(sym.eq(value, 2**63 - 1)),
                sym.iverson(value > 0),
            )

        generated = self.generate(checks)
        for value in (0, 2**40, -(2**63), 2**63 - 1):
            with self.subTest(value=value):
                expected = [value == 2**40, value == -(2**63), value == 2**63 - 1, value > 0]
                for arg in (value, np.int64(value)):
                    np.testing.assert_array_equal(np.asarray(generated(arg))[:, 0], expected)

    def test_input_layouts(self):
        def elements(x: wf.Matrix2):
            return sym.vector(x[0, 0], x[0, 1], x[1, 0], x[1, 1])

        generated = self.generate(elements)
        array = np.arange(4, dtype=np.float64).reshape(2, 2)
        layouts = [
            array,
            array.T,
            np.asfortranarray(array),
            np.arange(16.0).reshape(4, 4)[::2, ::2],
        ]
        for value in layouts:
            with self.subTest(strides=value.strides):
                np.testing.assert_array_equal(np.asarray(generated(value))[:, 0], value.ravel())

    def test_external_matrix_conversion(self):
        external = external_functions.declare_external_function(
            "external", [("x", wf.FloatScalar)], wf.Matrix2
        )

        def invoke(x: wf.FloatScalar):
            return external(x)

        calls = []

        def implementation(x):
            calls.append(x)
            return np.array([[1, 2], [3, 4]], dtype=np.float32).T

        generated = self.generate(invoke, context={"external": implementation})
        actual = np.asarray(generated(2.0))
        np.testing.assert_array_equal(actual, [[1, 3], [2, 4]])
        self.assertEqual(np.float64, actual.dtype)
        self.assertEqual(1, len(calls))

    def test_formatter_overrides(self):
        calls = []

        class CustomGenerator(wf.PythonGenerator):
            def format_float_literal(self, element):
                calls.append("literal")
                return "2.5" if element.value == 0.5 else self.super_format(element)

            def format_construct_matrix(self, element):
                calls.append("matrix")
                return self.super_format(element)

            def format_call_std_function(self, element):
                calls.append("math")
                return self.super_format(element)

            def format_cast(self, element):
                calls.append("cast")
                return self.super_format(element)

        def function(x: wf.FloatScalar, n: wf.IntScalar):
            return sym.vector(sym.cos(x), 0.5, n)

        generator = CustomGenerator(target=self.TARGET)
        generated, code = wf.generate_python(function, generator=generator)
        np.testing.assert_allclose(np.asarray(generated(0.4, 7))[:, 0], [np.cos(0.4), 2.5, 7])
        self.assertEqual({"literal", "matrix", "math", "cast"}, set(calls))
        # Standalone generated code needs only the selected backend's normal preamble.
        namespace = {}
        exec(generator.apply_preamble(code), namespace)
        np.testing.assert_allclose(np.asarray(namespace["function"](0.4, 7)), generated(0.4, 7))


class NumpyBackendTest(BackendContracts, unittest.TestCase):
    TARGET = wf.PythonGeneratorTarget.NumPy

    def test_symbolic_gradient(self):
        def differentiated(x: wf.FloatScalar):
            value = sym.sin(x) * x
            return sym.vector(value, value.diff(x))

        value, derivative = self.generate(differentiated)(0.4)[:, 0]
        np.testing.assert_allclose(value, np.sin(0.4) * 0.4)
        np.testing.assert_allclose(derivative, np.sin(0.4) + 0.4 * np.cos(0.4))


@unittest.skipIf(jax is None, "JAX is not installed")
class JaxBackendTest(BackendContracts, unittest.TestCase):
    TARGET = wf.PythonGeneratorTarget.JAX

    def setUp(self):
        stack = contextlib.ExitStack()
        self.addCleanup(stack.close)
        stack.enter_context(jax.experimental.enable_x64())

    def test_autograd_and_jit(self):
        def expression(x: wf.FloatScalar, v: wf.Vector2):
            return sym.vector(x * x + v.squared_norm(), 0.5)

        generated = jax.jit(self.generate(expression))
        dx, dv = jax.grad(lambda x, v: generated(x, v).sum(), argnums=(0, 1))(
            2.0, np.array([3.0, 4.0])
        )
        np.testing.assert_array_equal(dx, 4)
        np.testing.assert_array_equal(dv, [6, 8])

    def test_default_integer_width(self):
        def positive(n: wf.IntScalar):
            return sym.iverson(n > 0)

        with jax.experimental.disable_x64(), warnings.catch_warnings():
            warnings.simplefilter("error")
            generated = self.generate(positive, wf.PythonGeneratorFloatWidth.Float32)
            self.assertEqual(1, generated(np.int32(3)))
            self.assertEqual(0, generated(np.int32(-3)))
            self.assertFalse(jax.config.x64_enabled)

    def test_immutable_output_arguments(self):
        def output(x: wf.Vector2):
            return wf.OutputArg(x, "out")

        with self.assertRaisesRegex(exceptions.TypeError, "immutable"):
            wf.generate_function(
                output, wf.PythonGenerator(target=self.TARGET, use_output_arguments=True)
            )


@unittest.skipIf(torch is None, "PyTorch is not installed")
class PytorchBackendTest(BackendContracts, unittest.TestCase):
    TARGET = wf.PythonGeneratorTarget.PyTorch

    def test_autograd_through_conversions_and_constants(self):
        def expression(x: wf.FloatScalar, v: wf.Vector2):
            return sym.vector(x * x + v.squared_norm(), 0.5, sym.pi)

        # Converting float64 inputs to float32 must retain the graph back to the originals.
        generated = self.generate(expression, wf.PythonGeneratorFloatWidth.Float32)
        x = torch.tensor(2.0, dtype=torch.float64, requires_grad=True)
        v = torch.tensor([3.0, 4.0], dtype=torch.float64, requires_grad=True)
        result = generated(x, v)
        self.assertEqual(torch.float32, result.dtype)
        result.sum().backward()
        self.assertEqual(4, x.grad.item())
        np.testing.assert_array_equal(v.grad.numpy(), [6, 8])

    def test_external_result_keeps_gradients(self):
        external = external_functions.declare_external_function(
            "external", [("x", wf.FloatScalar)], wf.Vector2
        )

        def invoke(x: wf.FloatScalar):
            return external(x)

        generated = self.generate(invoke, context={"external": lambda x: torch.stack([x, x * x])})
        x = torch.tensor(2.0, dtype=torch.float64, requires_grad=True)
        generated(x).sum().backward()
        self.assertEqual(5, x.grad.item())

    def test_tensor_views_and_batching(self):
        def double(x: wf.Vector2):
            return sym.vector(x[0] * 2, x[1] * 2, 0.5)

        generated = self.generate(double)
        storage = torch.arange(12.0, dtype=torch.float64, requires_grad=True)
        view = storage.reshape(3, 4)[:, ::2]
        actual = torch.vmap(generated)(view)
        torch.testing.assert_close(actual[:, :2, 0], view * 2)
        self.assertTrue(torch.all(actual[:, 2, 0] == 0.5))
        actual.sum().backward()
        np.testing.assert_array_equal(storage.grad.numpy().reshape(3, 4), [[2, 0, 2, 0]] * 3)

    def test_construction_does_not_reserve_argument_names(self):
        def mixed(
            _values: wf.FloatScalar,
            _device: wf.FloatScalar,
            _value: wf.IntScalar,
            next: wf.FloatScalar,
        ):
            return sym.vector(_values, _device, _value, next, 0.5)

        np.testing.assert_array_equal(self.generate(mixed)(1, 2, 3, 4), [[1], [2], [3], [4], [0.5]])

    def test_integer_math_keeps_integer_precision(self):
        def check(n: wf.IntScalar):
            return sym.vector(sym.iverson(sym.eq(sym.floor(n), 2**40 + 3)), sym.sign(n))

        generated = self.generate(check, wf.PythonGeneratorFloatWidth.Float32)
        np.testing.assert_array_equal(generated(2**40 + 3), [[1], [1]])

    def test_computed_constants_follow_tensor_device(self):
        def mixed(x: wf.FloatScalar):
            return sym.vector(sym.sqrt(sym.pi), x, 0.5)

        # The meta device exercises device selection without requiring a GPU.
        x = torch.empty((), device="meta", dtype=torch.float64, requires_grad=True)
        result = self.generate(mixed)(x)
        self.assertEqual(x.device, result.device)
        self.assertTrue(result.requires_grad)

    def test_output_views(self):
        def outputs(x: wf.Vector4):
            return [wf.OutputArg(x * 2, "out"), wf.OutputArg(x, "optional", is_optional=True)]

        generated, _ = wf.generate_python(
            outputs, generator=wf.PythonGenerator(target=self.TARGET, use_output_arguments=True)
        )
        for optional_enabled in (False, True):
            storage = torch.full((4, 4), -1.0, dtype=torch.float64)
            out = storage[::2, ::2].T
            optional = torch.full((2, 2), -1.0, dtype=torch.float64).T if optional_enabled else None
            generated(torch.arange(4.0), out, optional)
            torch.testing.assert_close(
                out, torch.arange(4.0, dtype=torch.float64).reshape(2, 2) * 2
            )
            self.assertEqual(12, torch.count_nonzero(storage == -1).item())
            if optional is not None:
                torch.testing.assert_close(
                    optional, torch.arange(4.0, dtype=torch.float64).reshape(2, 2)
                )

    @unittest.skipIf(torch is None or not torch.cuda.is_available(), "CUDA is not available")
    def test_constants_follow_tensor_device(self):
        def mixed(x: wf.FloatScalar):
            return sym.vector(0.5, x, sym.pi)

        x = torch.tensor(2.0, dtype=torch.float64, device="cuda", requires_grad=True)
        result = self.generate(mixed)(x)
        self.assertEqual(x.device, result.device)
        result.sum().backward()
        self.assertEqual(1, x.grad.item())


if __name__ == "__main__":
    unittest.main()
