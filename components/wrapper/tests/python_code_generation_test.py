"""
Test generation and execution of python code.
"""

import collections
import typing as T
import unittest

import jax
import jax.numpy as jnp
import numpy as np
import torch as th

from wrenfold import code_generation, external_functions, sym
from wrenfold.geometry import Quaternion
from wrenfold.type_annotations import (
    FloatScalar,
    IntScalar,
    Matrix3,
    Vector2,
    Vector3,
    Vector4,
)

from .test_base import MathTestBase


def create_evaluator(func: T.Callable) -> T.Callable:
    """
    Create a python function that evaluates a symbolic function numerically by walking the
    expression tree and substituting in floats.

    We could promote this to a library feature, although handling of custom types is somewhat
    tricky. For now it only handles scalars and vectors.
    """
    desc = code_generation.create_function_description(func=func)
    output_expressions = desc.output_expressions()

    input_args = [a for a in desc.arguments if a.is_input]

    def evaluator(*args):
        assert len(args) == len(
            input_args), f"Mismatch in # of input args: {len(args)} != {len(input_args)}"
        sub_list: T.List[T.Tuple[sym.Expr, sym.Expr]] = []
        for numeric_arg, arg_desc in zip(args, input_args):
            sym_arg = arg_desc.create_symbolic_input()
            if isinstance(sym_arg, sym.Expr):
                if isinstance(numeric_arg, (float, np.float32, np.float64)):
                    numeric_arg = sym.float(float(numeric_arg))
                elif isinstance(numeric_arg, (int, np.int32, np.int64)):
                    numeric_arg = sym.integer(int(numeric_arg))
                else:
                    raise TypeError(
                        f"Invalid type for numeric arg: value = {numeric_arg}, type = {type(numeric_arg)}"
                    )
                sub_list.append((sym_arg, numeric_arg))
            elif isinstance(sym_arg, sym.MatrixExpr):
                assert isinstance(numeric_arg, np.ndarray), f"{type(numeric_arg)}"
                if len(numeric_arg.shape) > 2:
                    raise TypeError("Numpy array must be 1D or 2D")
                matrix = sym.matrix(numeric_arg.astype(float))
                for var, num in zip(sym_arg.to_flat_list(), matrix.to_flat_list()):
                    sub_list.append((var, num))
            else:
                raise TypeError(f"Not supported yet: {arg_desc.type}")

        # Now produce output expressions:
        assert len(output_expressions) > 0, "Must be at least one output"

        return_value = None
        output_arg_dict = dict()
        for key, output_exprs in output_expressions.items():
            output_exprs = sym.subs(output_exprs, sub_list).eval()
            assert isinstance(output_exprs,
                              (float, np.ndarray)), f"Failed to evaluate output: {key}"
            if not key.name:
                return_value = output_exprs
            else:
                output_arg_dict[key.name] = output_exprs

        if return_value is None:
            return output_arg_dict
        elif len(output_arg_dict) == 0:
            return return_value
        else:
            return return_value, output_arg_dict

    return evaluator


def batch_evaluator(func: T.Callable) -> T.Callable:
    """
    Create a new callable that runs an evaluator in a for-loop so we can more easily compare
    to batched variants.
    """

    def batched(*args):
        outputs = collections.defaultdict(list)
        for x in zip(*args):
            y = func(*x)
            if isinstance(y, tuple):
                rv, oa = y
                outputs[None].append(rv)
                for k, v in oa.items():
                    outputs[k].append(v)
            elif isinstance(y, dict):
                for k, v in y.items():
                    outputs[k].append(v)
            else:
                outputs[None].append(y)

        # now collate the results
        collated = dict()
        for k, vals in outputs.items():
            if isinstance(vals[0], np.ndarray):
                collated[k] = np.stack(vals)
            else:
                collated[k] = np.array(vals)

        if None in collated:
            return_value = collated.pop(None)
            if len(collated):
                return return_value, collated
            else:
                return return_value
        else:
            return collated

    return batched


class PythonCodeGenerationTestBase(MathTestBase):
    """
    Tests are implemented on this base class. Sub-classes inherit and apply customizations to
    handle the different values of `PythonGeneratorTarget`.
    """

    SUPPORTS_BATCH = True
    VERBOSE = False

    @classmethod
    def generate(cls, func: T.Callable[..., code_generation.CodegenFuncInvocationResult],
                 batch: bool, context: T.Dict[str, T.Any]) -> T.Callable:
        """Derived classes may override to customize the code-generation process."""
        raise NotImplementedError()

    @classmethod
    def make_array(cls, v: np.ndarray):
        raise NotImplementedError()

    @classmethod
    def convert_outputs_to_array(cls, outputs, strip_batch_dim: bool = False):
        if strip_batch_dim:
            constructor = lambda x: np.squeeze(np.array(x), axis=0)
        else:
            constructor = np.array
        if isinstance(outputs, tuple):
            return_val, output_args = outputs
            return constructor(return_val), {
                k: constructor(v) for (k, v) in output_args.items()
            }
        elif isinstance(outputs, dict):
            return {
                k: constructor(v) for (k, v) in output_args.items()
            }
        else:
            return constructor(outputs)

    @classmethod
    def get_optional_output_flags(
        cls, func: T.Callable[..., code_generation.CodegenFuncInvocationResult]
    ) -> T.Tuple[int, T.Dict[str, bool]]:
        """
        Get the function description and do two things:
        - Count the # of input args.
        - Return a dict of flags to enable all optional outputs.
        """
        description = code_generation.create_function_description(func)
        optional_arg_flags = dict()
        num_input_args = 0
        for arg in description.arguments:
            if arg.is_input:
                num_input_args += 1
            elif arg.is_optional:
                optional_arg_flags[f"compute_{arg.name}"] = True

        return num_input_args, optional_arg_flags

    def test_rotate_vector2d(self):
        """Test a function that rotates a vector."""

        def vector_rotation_2d(theta: FloatScalar, v: Vector2):
            R = sym.matrix([[sym.cos(theta), -sym.sin(theta)], [sym.sin(theta), sym.cos(theta)]])
            f = R * v
            J_theta = sym.jacobian(f, [theta])
            return [
                code_generation.ReturnValue(R * v),
                code_generation.OutputArg(J_theta, name="J_theta"),
            ]

        func = self.generate(vector_rotation_2d, batch=False, context=dict())

        # Create a symbolic evaluator as well:
        evaluator = create_evaluator(vector_rotation_2d)

        # Test against symbolic implementation:
        theta = 1.32
        v = np.array([-2.3, 5.3])
        rv_num, oa_num = func(theta, v)
        rv_sym, oa_sym = evaluator(theta, v)

        np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
        np.testing.assert_allclose(desired=oa_sym["J_theta"], actual=oa_num["J_theta"], rtol=1.0e-6)

        if not self.SUPPORTS_BATCH:
            return

        # Test the batched version:
        func_batched = self.generate(vector_rotation_2d, batch=True, context=dict())

        theta = np.array([-0.34, 0.9])
        v = np.array([[10.2, -8.2], [5.3, -2.31]])
        rv_num, oa_num = func_batched(theta, v)
        rv_sym, oa_sym = batch_evaluator(evaluator)(theta, v)

        np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
        np.testing.assert_allclose(desired=oa_sym["J_theta"], actual=oa_num["J_theta"], rtol=1.0e-6)

    def test_nested_conditional(self):
        """Test a function that creates a nested conditional."""

        def nested_cond(x: FloatScalar, y: FloatScalar):
            return sym.where(
                x > 0,
                sym.where(y > 0, 0, x * y),
                sym.where(y > 0, x + y, 0),
            )

        func = self.generate(nested_cond, batch=False, context=dict())
        evaluator = create_evaluator(nested_cond)

        for x in [-0.3, 0, 0.7]:
            for y in [-1.2, 0, 3.1]:
                np.testing.assert_allclose(desired=evaluator(x, y), actual=func(x, y), rtol=1.0e-6)
                np.testing.assert_allclose(desired=evaluator(x, y), actual=func(x, y), rtol=1.0e-6)

        if not self.SUPPORTS_BATCH:
            return

        # Test the batched version:
        x = np.array([-0.9, 0, 1.7])
        y = np.array([-0.5, 0, 3.5])

        func_batched = self.generate(nested_cond, batch=True, context=dict())
        evaluator_batched = batch_evaluator(evaluator)
        np.testing.assert_allclose(
            desired=evaluator_batched(x, y), actual=func_batched(x, y), rtol=1.0e-6)

    def test_std_math_functions(self):
        """Test a function that uses every built-in math function."""

        def built_ins(x: FloatScalar):
            # Alter the inputs here so everything is real:
            return sym.vector(
                sym.cos(x),
                sym.sin(x),
                sym.tan(x),
                sym.acos(x / 10),
                sym.asin(x / 10),
                sym.atan(x / 10),
                sym.cosh(x),
                sym.sinh(x),
                sym.tanh(x),
                sym.acosh(1 + sym.abs(x / 10)),
                sym.asinh(sym.abs(x)),
                sym.atanh(x / 10),
                sym.log(sym.abs(x) + 1.0e-6),
                sym.sign(x),
                sym.floor(x),
                sym.atan2(x * x, x + x),
            )

        func = self.generate(built_ins, batch=False, context=dict())
        evaluator = create_evaluator(built_ins)
        for x in np.linspace(-5.0, 5.0, num=20):
            np.testing.assert_allclose(desired=evaluator(x), actual=func(x), rtol=1.0e-6)

        if not self.SUPPORTS_BATCH:
            return

        func_batched = self.generate(built_ins, batch=True, context=dict())
        evaluator_batched = batch_evaluator(evaluator)
        x = np.linspace(-5.0, 5.0, num=20)
        np.testing.assert_allclose(
            desired=evaluator_batched(x), actual=func_batched(x), rtol=1.0e-6)

    def test_rotate_vector(self):
        """Test a function that creates a rotation matrix."""

        def rotate_vector(w: Vector3, v: Vector3):
            v_rot = (Quaternion.from_rotation_vector(w, epsilon=1.0e-6).to_rotation_matrix() * v)
            v_rot_D_w = v_rot.jacobian(vars=w)
            return (
                code_generation.ReturnValue(v_rot),
                code_generation.OutputArg(v_rot_D_w, name="v_rot_D_w", is_optional=True),
            )

        func = self.generate(rotate_vector, batch=False, context=dict())
        evaluator = create_evaluator(rotate_vector)

        ws = np.array([[-0.5e-8, -1.3e-7, 4.2e-8], [0.1, -0.3, 0.7], [1.2, -0.3, -0.8]])
        vs = np.array([[-4.2, 3.6, 8.1], [3.0, -7.0, 1.2], [0.02, -0.4, -3.0]])
        for w, v in zip(ws, vs):
            rv_num, oa_num = func(w, v)
            rv_sym, oa_sym = evaluator(w, v)
            np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
            np.testing.assert_allclose(
                desired=oa_sym["v_rot_D_w"], actual=oa_num["v_rot_D_w"], rtol=2.0e-6)

        if not self.SUPPORTS_BATCH:
            return

        func_batched = self.generate(rotate_vector, batch=True, context=dict())
        evaluator_batched = batch_evaluator(evaluator)

        rv_num, oa_num = func_batched(ws, vs)
        rv_sym, oa_sym = evaluator_batched(ws, vs)
        np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
        np.testing.assert_allclose(
            desired=oa_sym["v_rot_D_w"], actual=oa_num["v_rot_D_w"], rtol=2.0e-6)

    def test_quaternion_to_and_from_matrix(self):
        """Test conversion of a quaternion to/from a matrix."""

        def matrix_from_quat(q_wxyz: Vector4):
            return Quaternion.from_wxyz(q_wxyz).to_rotation_matrix()

        def quat_from_matrix(m: Matrix3):
            return Quaternion.from_rotation_matrix(m).to_vector_wxyz()

        q2m_func = self.generate(matrix_from_quat, batch=False, context=dict())
        q2m_evaluator = create_evaluator(matrix_from_quat)

        m2q_func = self.generate(quat_from_matrix, batch=False, context=dict())
        m2q_evaluator = create_evaluator(quat_from_matrix)

        qs = np.array([
            [0.34275355, -0.4911505, 0.74564528, 0.292069],
            [-0.55168672, -0.3643096, -0.24763068, -0.70823678],
            [-0.62807818, -0.61816808, -0.05451869, -0.46948242],
            [0.66212773, -0.69169043, 0.08646926, 0.27508959],
            [0.59426466, 0.57949089, -0.44302533, -0.33877483],
            [-0.32050495, -0.4959491, 0.49529348, -0.63717771],
            [-0.33024803, -0.55360404, 0.69653515, 0.3151152],
            [-0.42917766, 0.73698446, 0.29120886, -0.43342572],
            [-0.96585751, -0.21180895, 0.11426334, -0.09591728],
            [0.82019495, -0.20024658, 0.49402012, -0.20766724],
            [-0.88405116, -0.00382236, -0.43228709, -0.17767051],
            [-0.20492276, -0.84642162, -0.09095168, 0.48301645],
            [-0.50511663, 0.38294217, -0.34734593, 0.69105954],
            [0.7151773, -0.05036465, -0.63918558, -0.2782564],
            [-0.69895435, 0.37952658, 0.34789252, 0.49638008],
            [0.69070411, -0.29540322, -0.61085795, 0.25003466],
            [-0.5953591, -0.62963857, -0.45582188, -0.20329591],
            [0.87445741, -0.36888073, 0.12098375, 0.29088517],
            [-0.69501499, 0.68907201, -0.00272476, -0.20524742],
            [-0.70538009, 0.62270118, 0.12854222, 0.31330346],
        ])

        for q_wxyz_in in qs:
            R_num = q2m_func(q_wxyz_in)
            np.testing.assert_allclose(desired=q2m_evaluator(q_wxyz_in), actual=R_num, rtol=3.0e-6)

            q_wxyz_out_num = m2q_func(R_num)
            np.testing.assert_allclose(
                desired=m2q_evaluator(R_num), actual=q_wxyz_out_num, rtol=1.0e-6)

            # Signs may differ in the round-trip conversion, so multiply by sign of `w`.
            q_wxyz_out_num = q_wxyz_out_num.reshape([-1])
            np.testing.assert_allclose(
                desired=q_wxyz_in * np.sign(q_wxyz_in[0]),
                actual=q_wxyz_out_num * np.sign(q_wxyz_out_num[0]),
                rtol=3.0e-6,
            )

        if not self.SUPPORTS_BATCH:
            return

        # Test the batched version:
        q2m_func_batched = self.generate(matrix_from_quat, batch=True, context=dict())
        m2q_func_batched = self.generate(quat_from_matrix, batch=True, context=dict())

        R_num = q2m_func_batched(qs)
        qs_out_num = m2q_func_batched(R_num)[..., 0]  # Outputs (N, 4, 1) --> convert to (N, 4)
        np.testing.assert_allclose(
            desired=qs * np.sign(qs[:, 0:1]),
            actual=qs_out_num * np.sign(qs_out_num[:, 0:1]),
            rtol=3.0e-6,
        )

    def test_external_function_call(self):
        """
        Test calling an external function via a generated python method.
        """
        external_func = external_functions.declare_external_function(
            name="external_func", arguments=[("x", FloatScalar)], return_type=Vector2)

        def call_external_func(a: Vector2, b: Vector2):
            dot, = a.T * b
            return external_func(3 * dot + 0.2)

        func = self.generate(
            call_external_func,
            batch=False,
            context={
                external_func.name: lambda x: self.make_array([x * x, x * x * x])
            })

        np.testing.assert_allclose(
            desired=np.array([1.283689, 1.4544196370000002]).reshape([2, 1]),
            actual=func(np.array([0.5, -0.23]), np.array([0.3, -0.7])),
            rtol=1.0e-6,
        )

    def test_integer_inputs(self):
        """
        Test a function with integer inputs and outputs.

        TODO: Right now this doesn't produce the right output type, because we lack the
        ta::int_scalar_expr annotation that exists in C++. I will fix the type
        deduction ASAP, and then come back and update this test to reflect the right output
        type.
        """

        def compute_with_ints(x: IntScalar, y: IntScalar):
            return [
                code_generation.ReturnValue(x * y),
                code_generation.OutputArg(x + y, name="sum"),
            ]

        func = self.generate(compute_with_ints, batch=False, context=dict())
        evaluator = create_evaluator(compute_with_ints)

        xs = np.arange(-5, 5, step=1, dtype=np.int32)
        ys = np.arange(-5, 5, step=1, dtype=np.int32)

        for x in xs:
            for y in ys:
                rv_num, oa_num = func(x, y)
                rv_sym, oa_sym = evaluator(x, y)
                np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=3.0e-6)
                np.testing.assert_allclose(desired=oa_sym["sum"], actual=oa_num["sum"], rtol=3.0e-6)


class NumPyCodeGenerationTest(PythonCodeGenerationTestBase):
    """Run with NumPy configuration."""

    SUPPORTS_BATCH = False

    @classmethod
    def generate(cls, func: T.Callable[..., code_generation.CodegenFuncInvocationResult],
                 batch: bool, context: T.Dict[str, T.Any]) -> T.Callable:
        _, optional_arg_flags = cls.get_optional_output_flags(func)
        func_gen, code = code_generation.generate_python(
            func=func,
            target=code_generation.PythonGeneratorTarget.NumPy,
            context=context,
        )
        if cls.VERBOSE:
            print(code)

        def wrapped_func(*args):
            return func_gen(*args, **optional_arg_flags)

        return wrapped_func

    @classmethod
    def make_array(cls, v: np.ndarray):
        return np.asarray(v)


class JaxCodeGenerationTest(PythonCodeGenerationTestBase):
    """Run with JAX configuration."""

    @classmethod
    def generate(cls, func: T.Callable[..., code_generation.CodegenFuncInvocationResult],
                 batch: bool, context: T.Dict[str, T.Any]) -> T.Callable:
        func_gen, code = code_generation.generate_python(
            func=func, target=code_generation.PythonGeneratorTarget.JAX, context=context)
        if cls.VERBOSE:
            print(code)

        num_input_args, optional_arg_flags = cls.get_optional_output_flags(func)

        if batch:
            func_batched = jax.vmap(
                lambda *args: func_gen(*args, **optional_arg_flags),
                in_axes=[0] * num_input_args,
                out_axes=0,
            )
            func_jit = jax.jit(func_batched)
        else:
            func_jit = jax.jit(lambda *args: func_gen(*args, **optional_arg_flags))

        def wrapped_func(*args):
            results = func_jit(*args)
            return cls.convert_outputs_to_array(outputs=results)

        return wrapped_func

    @classmethod
    def make_array(cls, v: np.ndarray):
        return jnp.asarray(v)


class PyTorchCodeGenerationTest(PythonCodeGenerationTestBase):
    """Run with PyTorch configuration."""

    @classmethod
    def generate(cls, func: T.Callable[..., code_generation.CodegenFuncInvocationResult],
                 batch: bool, context: T.Dict[str, T.Any]) -> T.Callable:
        func_gen, code = code_generation.generate_python(
            func=func, target=code_generation.PythonGeneratorTarget.PyTorch, context=context)
        if cls.VERBOSE:
            print(code)

        num_input_args, optional_arg_flags = cls.get_optional_output_flags(func)

        # For PyTorch, test that we can batch the code we generate.
        if batch:
            func_maybe_batched = th.vmap(
                lambda *args: func_gen(*args, **optional_arg_flags),
                in_dims=tuple([0] * num_input_args),
                out_dims=0,
            )
        else:
            func_maybe_batched = lambda *args: func_gen(*args, **optional_arg_flags)

        def wrapped_func(*args):
            # Inject a batch dimension, then strip it.
            results = func_maybe_batched(*[th.tensor(x) for x in args])
            return cls.convert_outputs_to_array(outputs=results)

        return wrapped_func

    @classmethod
    def make_array(cls, v: np.ndarray):
        return th.asarray(v)


def main():
    test_cases = [
        NumPyCodeGenerationTest,
        JaxCodeGenerationTest,
        PyTorchCodeGenerationTest,
    ]
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite)


if __name__ == "__main__":
    main()
