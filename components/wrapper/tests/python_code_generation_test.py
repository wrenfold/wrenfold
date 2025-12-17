"""
Test generation and execution of python code.
"""

import collections
import dataclasses
import inspect
import sys
import typing as T
import unittest

import jax
import numba
import numpy as np
import numpy.typing as npt

try:
    import torch as th
except ImportError:
    print("Torch not installed, PyTorch tests will be skipped.")
    th = None

from wrenfold import code_generation, external_functions, sym, type_info
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
        assert len(args) == len(input_args), (
            f"Mismatch in # of input args: {len(args)} != {len(input_args)}"
        )
        sub_list: list[tuple[sym.Expr, sym.Expr]] = []
        for numeric_arg, arg_desc in zip(args, input_args, strict=True):
            sym_arg = arg_desc.create_symbolic_input()
            if isinstance(sym_arg, sym.Expr):
                if isinstance(numeric_arg, (float, np.float32, np.float64)):  # type: ignore
                    numeric_arg = sym.float(float(numeric_arg))
                elif isinstance(numeric_arg, (int, np.int32, np.int64)):  # type: ignore
                    numeric_arg = sym.integer(int(numeric_arg))
                else:
                    raise TypeError(
                        f"Invalid type for numeric arg: value = {numeric_arg}, type = {type(numeric_arg)}"  # noqa: E501
                    )
                sub_list.append((sym_arg, numeric_arg))
            elif isinstance(sym_arg, sym.MatrixExpr):
                assert isinstance(numeric_arg, np.ndarray), (
                    f"{type(numeric_arg)}, arg = {arg_desc.name}"
                )
                if len(numeric_arg.shape) > 2:
                    raise TypeError("Numpy array must be 1D or 2D")
                matrix = sym.matrix(numeric_arg.astype(float))
                for var, num in zip(sym_arg.to_flat_list(), matrix.to_flat_list(), strict=True):
                    sub_list.append((var, num))
            else:
                raise TypeError(f"Not supported yet: {arg_desc.type}")

        # Now produce output expressions:
        assert len(output_expressions) > 0, "Must be at least one output"

        return_value = None
        output_arg_dict = dict()
        for key, output_exprs in output_expressions.items():
            output_exprs = sym.subs(output_exprs, sub_list).eval()
            assert isinstance(output_exprs, (float, np.ndarray)), (
                f"Failed to evaluate output: {key}"
            )
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
        for x in zip(*args, strict=True):
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


def get_optional_output_flags(
    func: T.Callable[..., code_generation.CodegenFuncInvocationResult],
) -> tuple[int, dict[str, bool]]:
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


def convert_outputs_to_array(outputs, strip_batch_dim: bool = False):
    def constructor(x):
        if x is None:
            return None
        if strip_batch_dim:
            return np.squeeze(np.array(x), axis=0)
        else:
            return np.array(x)

    if isinstance(outputs, tuple):
        return (constructor(v) for v in outputs)
    else:
        return constructor(outputs)


class PythonCodeGenerationTestBase(MathTestBase):
    """
    Tests are implemented on this base class. Sub-classes inherit and apply customizations to
    handle the different values of `PythonGeneratorTarget`.
    """

    SUPPORTS_BATCH = True
    EXPECTED_TYPE: npt.DTypeLike | None = None

    def _generator(self, func: T.Callable, batch: bool = False) -> T.Callable:
        raise NotImplementedError()

    def test_rotate_vector2d(self):
        """Test a function that rotates a vector."""

        def vector_rotation_2d(theta: FloatScalar, v: Vector2):
            R = sym.matrix([[sym.cos(theta), -sym.sin(theta)], [sym.sin(theta), sym.cos(theta)]])
            f = R * v
            J_theta = sym.jacobian(f, [theta])
            return [
                code_generation.ReturnValue(f),
                code_generation.OutputArg(J_theta, name="J_theta"),
            ]

        func = self._generator(vector_rotation_2d)

        # Create a symbolic evaluator as well:
        evaluator = create_evaluator(vector_rotation_2d)

        # Test against symbolic implementation:
        theta = 1.32
        v = np.array([-2.3, 5.3])
        rv_num, J_theta_num = func(theta, v)
        rv_sym, oa_sym = evaluator(theta, v)

        np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
        np.testing.assert_allclose(desired=oa_sym["J_theta"], actual=J_theta_num, rtol=1.0e-6)
        self.assertEqual(J_theta_num.dtype, self.EXPECTED_TYPE)

        if not self.SUPPORTS_BATCH:
            return

        # Test the batched version:
        func_batched = self._generator(vector_rotation_2d, batch=True)

        theta = np.array([-0.34, 0.9])
        v = np.array([[10.2, -8.2], [5.3, -2.31]])
        rv_num, J_theta_num = func_batched(theta, v)
        rv_sym, oa_sym = batch_evaluator(evaluator)(theta, v)

        np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
        np.testing.assert_allclose(desired=oa_sym["J_theta"], actual=J_theta_num, rtol=1.0e-6)

    def test_nested_conditional(self):
        """Test a function that creates a nested conditional."""

        def nested_cond(x: FloatScalar, y: FloatScalar):
            return sym.where(
                x > 0,
                sym.where(y > 0, 0, x * y),
                sym.where(y > 0, x + y, 0),
            )

        func = self._generator(nested_cond)
        evaluator = create_evaluator(nested_cond)

        for x in [-0.3, 0, 0.7]:
            for y in [-1.2, 0, 3.1]:
                result = func(x, y)
                np.testing.assert_allclose(desired=evaluator(x, y), actual=result, rtol=1.0e-6)
                np.testing.assert_allclose(desired=evaluator(x, y), actual=result, rtol=1.0e-6)
                if not isinstance(result, float):
                    self.assertEqual(result.dtype, self.EXPECTED_TYPE)

        if not self.SUPPORTS_BATCH:
            return

        # Test the batched version:
        x = np.array([-0.9, 0, 1.7])
        y = np.array([-0.5, 0, 3.5])

        func_batched = self._generator(nested_cond, batch=True)
        evaluator_batched = batch_evaluator(evaluator)
        np.testing.assert_allclose(
            desired=evaluator_batched(x, y), actual=func_batched(x, y), rtol=1.0e-6
        )

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
                sym.asinh(sym.abs(x * sym.pi)),
                sym.atanh(x / 10),
                sym.log(sym.abs(x * sym.E) + 1.0e-6),
                sym.sign(x),
                sym.floor(x),
                sym.atan2(x * x, x + x),
                sym.iverson(x > 0),
            )

        func = self._generator(built_ins)
        evaluator = create_evaluator(built_ins)
        for x in np.linspace(-5.0, 5.0, num=20):
            result = func(x)
            np.testing.assert_allclose(desired=evaluator(x), actual=result, rtol=1.0e-6)
            self.assertEqual(result.dtype, self.EXPECTED_TYPE)

        if not self.SUPPORTS_BATCH:
            return

        func_batched = self._generator(built_ins, batch=True)
        evaluator_batched = batch_evaluator(evaluator)
        x = np.linspace(-5.0, 5.0, num=20)
        np.testing.assert_allclose(
            desired=evaluator_batched(x), actual=func_batched(x), rtol=1.0e-6
        )

    def test_rotate_vector(self):
        """Test a function that creates a rotation matrix."""

        def rotate_vector(w: Vector3, v: Vector3):
            v_rot = Quaternion.from_rotation_vector(w, epsilon=1.0e-6).to_rotation_matrix() * v
            v_rot_D_w = v_rot.jacobian(vars=w)
            return (
                code_generation.ReturnValue(v_rot),
                code_generation.OutputArg(v_rot_D_w, name="v_rot_D_w", is_optional=True),
            )

        func = self._generator(rotate_vector)
        evaluator = create_evaluator(rotate_vector)

        ws = np.array([[-0.5e-8, -1.3e-7, 4.2e-8], [0.1, -0.3, 0.7], [1.2, -0.3, -0.8]])
        vs = np.array([[-4.2, 3.6, 8.1], [3.0, -7.0, 1.2], [0.02, -0.4, -3.0]])
        for w, v in zip(ws, vs, strict=True):
            rv_num, D_w_num = func(w, v)
            rv_sym, oa_sym = evaluator(w, v)
            np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
            np.testing.assert_allclose(desired=oa_sym["v_rot_D_w"], actual=D_w_num, rtol=2.0e-6)
            self.assertEqual(rv_num.dtype, self.EXPECTED_TYPE)
            self.assertEqual(D_w_num.dtype, self.EXPECTED_TYPE)

        if not self.SUPPORTS_BATCH:
            return

        func_batched = self._generator(rotate_vector, batch=True)
        evaluator_batched = batch_evaluator(evaluator)

        rv_num, D_w_num = func_batched(ws, vs)
        rv_sym, oa_sym = evaluator_batched(ws, vs)
        np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=1.0e-6)
        np.testing.assert_allclose(desired=oa_sym["v_rot_D_w"], actual=D_w_num, rtol=2.0e-6)

    def test_quaternion_to_and_from_matrix(self):
        """Test conversion of a quaternion to/from a matrix."""

        def matrix_from_quat(q_wxyz: Vector4):
            return Quaternion.from_wxyz(q_wxyz).to_rotation_matrix()

        def quat_from_matrix(m: Matrix3):
            return Quaternion.from_rotation_matrix(m).to_vector_wxyz()

        q2m_func = self._generator(matrix_from_quat)
        q2m_evaluator = create_evaluator(matrix_from_quat)

        m2q_func = self._generator(quat_from_matrix)
        m2q_evaluator = create_evaluator(quat_from_matrix)

        qs = np.array(
            [
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
            ]
        )

        for q_wxyz_in in qs:
            R_num = q2m_func(q_wxyz_in)
            np.testing.assert_allclose(desired=q2m_evaluator(q_wxyz_in), actual=R_num, rtol=3.0e-6)
            self.assertEqual(R_num.dtype, self.EXPECTED_TYPE)

            q_wxyz_out_num = m2q_func(R_num)
            np.testing.assert_allclose(
                desired=m2q_evaluator(R_num), actual=q_wxyz_out_num, rtol=1.0e-6
            )

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
        q2m_func_batched = self._generator(matrix_from_quat, batch=True)
        m2q_func_batched = self._generator(quat_from_matrix, batch=True)

        R_num = q2m_func_batched(qs)
        qs_out_num = m2q_func_batched(R_num)[..., 0]  # Outputs (N, 4, 1) --> convert to (N, 4)
        np.testing.assert_allclose(
            desired=qs * np.sign(qs[:, 0:1]),
            actual=qs_out_num * np.sign(qs_out_num[:, 0:1]),
            rtol=3.0e-6,
        )

    def test_no_required_output(self):
        """
        Test a method that only has optional outputs.
        """

        def only_optional_outputs(x: Vector3, y: Vector3):
            inner_product = x.T * y
            outer_product = x * y.T
            return [
                code_generation.OutputArg(inner_product, name="inner", is_optional=True),
                code_generation.OutputArg(outer_product, name="outer", is_optional=True),
            ]

        func = self._generator(only_optional_outputs)
        evaluator = create_evaluator(only_optional_outputs)

        xs = np.array([[-0.5, -8.5, -2.8], [-3.0, 3.5, 8.2], [9.8, 5.1, -4.3]])
        ys = np.array([[7.3, -1.1, -1.3], [-2.5, -0.4, 0.6], [5.9, 3.3, -1.8]])

        for x in xs:
            for y in ys:
                oa_sym = evaluator(x, y)
                inner_num, outer_num = func(x, y)
                np.testing.assert_allclose(desired=oa_sym["inner"], actual=inner_num, rtol=1.0e-6)
                np.testing.assert_allclose(desired=oa_sym["outer"], actual=outer_num, rtol=1.0e-6)
                if not isinstance(inner_num, float):
                    self.assertEqual(inner_num.dtype, self.EXPECTED_TYPE)
                self.assertEqual(outer_num.dtype, self.EXPECTED_TYPE)

        if not self.SUPPORTS_BATCH:
            return

        func_batched = self._generator(only_optional_outputs, batch=True)
        evaluator_batched = batch_evaluator(evaluator)

        oa_sym = evaluator_batched(xs, ys)
        inner_num, outer_num = func_batched(xs, ys)
        np.testing.assert_allclose(desired=oa_sym["inner"], actual=inner_num, rtol=1.0e-6)
        np.testing.assert_allclose(desired=oa_sym["outer"], actual=outer_num, rtol=1.0e-6)

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
                code_generation.OutputArg(sym.vector(x + y), name="sum"),
            ]

        func = self._generator(compute_with_ints)
        evaluator = create_evaluator(compute_with_ints)

        xs = np.arange(-5, 5, step=1, dtype=np.int32)
        ys = np.arange(-5, 5, step=1, dtype=np.int32)

        for x in xs:
            for y in ys:
                rv_num, sum_numerical = func(x, y)
                rv_sym, oa_sym = evaluator(x, y)
                np.testing.assert_allclose(desired=rv_sym, actual=rv_num, rtol=3.0e-6)
                np.testing.assert_allclose(desired=oa_sym["sum"], actual=sum_numerical, rtol=3.0e-6)
                if not isinstance(rv_num, float):
                    self.assertEqual(rv_num.dtype, self.EXPECTED_TYPE)
                if not isinstance(sum_numerical, float):
                    self.assertEqual(sum_numerical.dtype, self.EXPECTED_TYPE)


@dataclasses.dataclass
class SimParamsSymbolic:
    """
    A sample struct with some parameters of a simulation. This is the symbolic version.
    We use this to test code-generation of custom types in python methods.
    """

    mass: FloatScalar
    drag_coefficient: FloatScalar
    gravity: FloatScalar


@dataclasses.dataclass
class SimStateSymbolic:
    position: Vector2
    velocity: Vector2


@dataclasses.dataclass
class SimParams:
    """Numeric equivalent of ``SimParamsSymbolic`."""

    mass: float
    drag_coefficient: float
    gravity: float


@dataclasses.dataclass
class SimState:
    """Numeric equivalent of ``SimStateSymbolic`."""

    position: np.ndarray
    velocity: np.ndarray


def integrate_sim(
    x: SimStateSymbolic,
    dt: FloatScalar,
    params: SimParamsSymbolic,
):
    """
    An example function used to test code generation. We integrate some simple 2D dynamics
    for a projectile subject to gravity and drag.

    Args:
        position: Position of 2D object.
        velocity: Velocity of 2D object.
        dt: Step length.
        params: Some made up simulation params.
    """
    # Compute force:
    f = sym.vector(0, -params.gravity * params.mass) - x.velocity * x.velocity.norm() * (
        params.drag_coefficient / 2
    )
    accel = f * (1 / params.mass)

    # Euler integration:
    velocity = x.velocity + accel * dt
    position = x.position + x.velocity * dt + accel * (dt**2) / 2

    return SimStateSymbolic(position=position, velocity=velocity)


class NumPyCodeGenerationTestBase(PythonCodeGenerationTestBase):
    """Run with NumPy configuration."""

    VERBOSE = False
    SUPPORTS_BATCH = False
    USE_OUTPUT_ARGUMENTS = False

    def _generator(
        self,
        func: T.Callable,
        batch: bool = False,
        custom_generator_type: type[code_generation.PythonGenerator] | None = None,
        **kwargs,
    ) -> T.Callable:
        if custom_generator_type is None:
            custom_generator_type = code_generation.PythonGenerator
        generator = custom_generator_type(
            target=code_generation.PythonGeneratorTarget.NumPy,
            float_width=(
                code_generation.PythonGeneratorFloatWidth.Float32
                if np.float32 == self.EXPECTED_TYPE
                else code_generation.PythonGeneratorFloatWidth.Float64
            ),
            use_output_arguments=self.USE_OUTPUT_ARGUMENTS,
        )

        func_gen, code = code_generation.generate_python(func=func, generator=generator, **kwargs)
        if self.VERBOSE:
            print(code)

        if not self.USE_OUTPUT_ARGUMENTS:
            _, optional_arg_flags = get_optional_output_flags(func)

            def wrapped_func(*args, **kwargs):
                return func_gen(*args, **kwargs, **optional_arg_flags)

            return wrapped_func
        else:
            # Generate a wrapped version that passes arrays to be filled as arguments.
            description = code_generation.create_function_description(func)
            output_arg_spec = dict()
            for arg in description.arguments:
                if not arg.is_input:
                    assert isinstance(arg.type, type_info.MatrixType)
                    output_arg_spec[arg.name] = arg.type.shape

            def wrapped_func_capture_output_args(*args, **kwargs):
                output_args = {
                    k: np.zeros(shape=s, dtype=self.EXPECTED_TYPE)
                    for (k, s) in output_arg_spec.items()
                }
                ret_value = func_gen(*args, **kwargs, **output_args)
                if ret_value is not None and len(output_args):
                    return ret_value, *output_args.values()
                elif ret_value is not None and not len(output_args):
                    return ret_value
                elif ret_value is None and len(output_args):
                    return tuple(output_args.values())

            return wrapped_func_capture_output_args

    def test_external_function_call_np(self):
        """
        Test calling an external function via a generated python method.
        """
        external_func = external_functions.declare_external_function(
            name="external_func", arguments=[("x", FloatScalar)], return_type=Vector2
        )

        def call_external_func(a: Vector2, b: Vector2):
            (dot,) = a.T * b
            return external_func(3 * dot + 0.2)

        def external_function(x: float):
            return np.array([x * x, x * x * x])

        func = self._generator(
            call_external_func,
            batch=False,
            context={external_func.name: external_function},
        )

        np.testing.assert_allclose(
            desired=np.array([1.283689, 1.4544196370000002]).reshape([2, 1]),
            actual=func(np.array([0.5, -0.23]), np.array([0.3, -0.7])),
            rtol=1.0e-6,
        )

    def test_custom_types(self):
        """
        Test we can generated a method that uses a custom type.
        """

        class CustomPythonGenerator(code_generation.PythonGenerator):
            def format_custom_type(self, custom: type_info.CustomType):
                if custom.python_type == SimParamsSymbolic:
                    return "SimParams"
                elif custom.python_type == SimStateSymbolic:
                    return "SimState"
                return self.super_format(custom)

        func = self._generator(
            func=integrate_sim,
            batch=False,
            custom_generator_type=CustomPythonGenerator,
            context={
                "SimParams": SimParams,
                "SimState": SimState,
            },
        )

        # Test against the symbolic implementation:
        p_in = np.array([10.2, -5.0]).reshape((2, 1))
        v_in = np.array([9.1, 8.3]).reshape((2, 1))

        x_out_sym: SimStateSymbolic = integrate_sim(
            x=SimStateSymbolic(position=Vector2(p_in), velocity=Vector2(v_in)),
            dt=0.7,
            params=SimParamsSymbolic(mass=5.7, drag_coefficient=2.4, gravity=9.81),
        )

        x_out: SimState = func(
            x=SimState(position=p_in, velocity=v_in),
            dt=0.7,
            params=SimParams(mass=5.7, drag_coefficient=2.4, gravity=9.81),
        )

        np.testing.assert_allclose(x_out_sym.position.eval(), x_out.position, atol=1.0e-14)
        np.testing.assert_allclose(x_out_sym.velocity.eval(), x_out.velocity, atol=1.0e-14)


class NumPyCodeGenerationF32Test(NumPyCodeGenerationTestBase):
    EXPECTED_TYPE = np.float32


class NumPyCodeGenerationF64Test(NumPyCodeGenerationTestBase):
    EXPECTED_TYPE = np.float64


class NumPyCodeGenerationF32OutputArgsTest(NumPyCodeGenerationTestBase):
    EXPECTED_TYPE = np.float32
    USE_OUTPUT_ARGUMENTS = True


class NumPyCodeGenerationF64OutputArgsTest(NumPyCodeGenerationTestBase):
    EXPECTED_TYPE = np.float64
    USE_OUTPUT_ARGUMENTS = True


class NumbaCodeGenerationTestBase(PythonCodeGenerationTestBase):
    """Run with NumPy+numba.njit configuration."""

    SUPPORTS_BATCH = False
    VERBOSE = False
    USE_OUTPUT_ARGUMENTS = False

    def _generator(
        self,
        func: T.Callable,
        batch: bool = False,
        **kwargs,
    ) -> T.Callable:
        generator = code_generation.PythonGenerator(
            float_width=(
                code_generation.PythonGeneratorFloatWidth.Float32
                if np.float32 == self.EXPECTED_TYPE
                else code_generation.PythonGeneratorFloatWidth.Float64
            ),
            use_output_arguments=self.USE_OUTPUT_ARGUMENTS,
        )

        func_gen, code = code_generation.generate_python(func=func, generator=generator, **kwargs)
        if self.VERBOSE:
            print(code)

        _, optional_arg_flags = get_optional_output_flags(func)

        jitted_func = numba.njit(func_gen)

        if not self.USE_OUTPUT_ARGUMENTS:
            _, optional_arg_flags = get_optional_output_flags(func)

            def wrapped_func(*args, **kwargs):
                return jitted_func(*args, **kwargs, **optional_arg_flags)

            return wrapped_func
        else:
            description = code_generation.create_function_description(func)
            output_arg_spec = dict()
            for arg in description.arguments:
                if not arg.is_input:
                    assert isinstance(arg.type, type_info.MatrixType)
                    output_arg_spec[arg.name] = arg.type.shape

            def wrapped_func_capture_output_args(*args, **kwargs):
                output_args = {
                    k: np.zeros(shape=s, dtype=self.EXPECTED_TYPE)
                    for (k, s) in output_arg_spec.items()
                }
                ret_value = jitted_func(*args, **kwargs, **output_args)
                if ret_value is not None and len(output_args):
                    return ret_value, *output_args.values()
                elif ret_value is not None and not len(output_args):
                    return ret_value
                elif ret_value is None and len(output_args):
                    return tuple(output_args.values())

            return wrapped_func_capture_output_args

    def test_external_function_call_numba(self):
        """
        Test calling an external function via a generated python method.
        """
        external_func = external_functions.declare_external_function(
            name="external_func", arguments=[("x", FloatScalar)], return_type=Vector2
        )

        def call_external_func(a: Vector2, b: Vector2):
            (dot,) = a.T * b
            return external_func(3 * sym.cos(dot) + 0.2 - sym.sin(a[0]))

        @numba.njit
        def external_function(x: float):
            return np.array([x * np.cos(x), x - np.sin(x) * x])

        func = self._generator(
            call_external_func,
            batch=False,
            context={external_func.name: external_function},
        )

        np.testing.assert_allclose(
            desired=np.array([-2.1763077, 1.1972187]).reshape([2, 1]),
            actual=func(np.array([0.5, -0.23]), np.array([0.3, -0.7])),
            rtol=1.0e-6,
        )


class NumbaCodeGenerationF32Test(NumbaCodeGenerationTestBase):
    EXPECTED_TYPE = np.float32


class NumbaCodeGenerationF64Test(NumbaCodeGenerationTestBase):
    EXPECTED_TYPE = np.float64


class NumbaCodeGenerationF32OutputArgsTest(NumbaCodeGenerationTestBase):
    EXPECTED_TYPE = np.float32
    USE_OUTPUT_ARGUMENTS = True


class NumbaCodeGenerationF64OutputArgsTest(NumbaCodeGenerationTestBase):
    EXPECTED_TYPE = np.float64
    USE_OUTPUT_ARGUMENTS = True


class JaxCodeGenerationTest(PythonCodeGenerationTestBase):
    """Run with JAX configuration."""

    EXPECTED_TYPE = np.float32  # Jax only supports F32

    def _generator(
        self,
        func: T.Callable,
        batch: bool = False,
        **kwargs,
    ) -> T.Callable:
        generator = code_generation.PythonGenerator(
            target=code_generation.PythonGeneratorTarget.JAX,
            float_width=(code_generation.PythonGeneratorFloatWidth.Float32),
        )

        func_gen, _code = code_generation.generate_python(func=func, generator=generator, **kwargs)
        num_input_args, optional_arg_flags = get_optional_output_flags(func)

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
            if isinstance(results, tuple):
                return (np.array(x) for x in results)
            else:
                return np.array(results)

        return wrapped_func


class PyTorchCodeGenerationTest(PythonCodeGenerationTestBase):
    """Run with PyTorch configuration."""

    EXPECTED_TYPE = np.float32

    def _generator(
        self,
        func: T.Callable,
        batch: bool = False,
        **kwargs,
    ) -> T.Callable:
        generator = code_generation.PythonGenerator(
            target=code_generation.PythonGeneratorTarget.PyTorch,
            float_width=(code_generation.PythonGeneratorFloatWidth.Float32),
        )

        func_gen, _code = code_generation.generate_python(func=func, generator=generator, **kwargs)
        num_input_args, optional_arg_flags = get_optional_output_flags(func)

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
            results = func_maybe_batched(*[th.as_tensor(x) for x in args])

            def convert_tensor(x):
                if isinstance(x, th.Tensor):
                    return x.numpy()
                return x

            if isinstance(results, tuple):
                return (convert_tensor(x) for x in results)
            else:
                return convert_tensor(results)

        return wrapped_func


def test_apply_preamble():
    """A simple test to check that we apply the preamble correctly to code."""
    targets_to_test = [
        code_generation.PythonGeneratorTarget.NumPy,
        code_generation.PythonGeneratorTarget.JAX,
    ]
    if th is not None:
        targets_to_test.append(code_generation.PythonGeneratorTarget.PyTorch)

    target_imports = {
        code_generation.PythonGeneratorTarget.NumPy: "import numpy as np",
        code_generation.PythonGeneratorTarget.JAX: "import jax.numpy as jnp",
        code_generation.PythonGeneratorTarget.PyTorch: "import torch as th",
    }

    code = "def foo():\n\tpass"
    preamble_template = "# Machine generated code.\nimport typing as T\n{}\n\n{}\n"

    for target in targets_to_test:
        code_generator = code_generation.PythonGenerator(target=target)
        code_with_preamble = code_generator.apply_preamble(code)
        expected = preamble_template.format(target_imports[target], code)
        assert code_with_preamble == expected


def test_numpy_type_annotations():
    """
    Test that type annotations are correct.
    """

    def sym1(x: Vector2, y: Vector3, z: FloatScalar, w: IntScalar):
        return [
            code_generation.ReturnValue(x * z * w),
            code_generation.OutputArg(sym.vector(*x, z), name="foo"),
            code_generation.OutputArg(y * y.T, name="bar", is_optional=True),
        ]

    func, _code = code_generation.generate_python(
        sym1, generator=code_generation.PythonGenerator(use_output_arguments=False)
    )

    spec = inspect.getfullargspec(func=func)
    for arg_name in spec.args:
        assert arg_name in spec.annotations, f"Missing type annotation for argument: {arg_name}"

    assert (
        str(spec.annotations["return"])
        == "tuple[numpy.ndarray, numpy.ndarray, numpy.ndarray | None]"
    )
    assert spec.annotations["x"] is np.ndarray
    assert spec.annotations["y"] is np.ndarray
    assert spec.annotations["z"] is float
    assert spec.annotations["w"] is int
    assert spec.annotations["compute_bar"] is bool

    func, _code = code_generation.generate_python(
        sym1, generator=code_generation.PythonGenerator(use_output_arguments=True)
    )

    spec = inspect.getfullargspec(func=func)
    for arg_name in spec.args:
        assert arg_name in spec.annotations, f"Missing type annotation for argument: {arg_name}"

    assert spec.annotations["return"] is np.ndarray
    assert spec.annotations["x"] is np.ndarray
    assert spec.annotations["y"] is np.ndarray
    assert spec.annotations["z"] is float
    assert spec.annotations["w"] is int
    assert spec.annotations["foo"] is np.ndarray
    assert (
        str(spec.annotations["bar"]) == "numpy.ndarray | None"
        or str(spec.annotations["bar"]) == "np.ndarray | None"
    )


def main():
    # TOOD: Find a better way to test the different permutations.
    test_cases = [
        NumPyCodeGenerationF32Test,
        NumPyCodeGenerationF64Test,
        NumPyCodeGenerationF32OutputArgsTest,
        NumPyCodeGenerationF64OutputArgsTest,
        NumbaCodeGenerationF32Test,
        NumbaCodeGenerationF64Test,
        NumbaCodeGenerationF32OutputArgsTest,
        NumbaCodeGenerationF64OutputArgsTest,
        JaxCodeGenerationTest,
    ]
    if th is not None:
        test_cases.append(PyTorchCodeGenerationTest)

    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))
    suite.addTest(unittest.FunctionTestCase(test_apply_preamble))
    suite.addTest(unittest.FunctionTestCase(test_numpy_type_annotations))
    runner = unittest.TextTestRunner(verbosity=2)
    if not runner.run(suite).wasSuccessful():
        sys.exit(1)


if __name__ == "__main__":
    main()
