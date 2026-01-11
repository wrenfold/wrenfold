"""
Test the code-generation wrapper.

Most of the output code generation is tested by cpp_generation_test/rust_generation_test. The
examples are also run as tests - these offer a lot more diverse coverage. The purpose of this
test is just to validate that the wrapper works and exposes the correct members.
"""

import dataclasses
import typing
import unittest

import wrenfold as wf
from wrenfold import ast, external_functions, sym, type_info
from wrenfold.code_generation import Argument, ArgumentDirection, OutputKey

from .test_base import MathTestBase


def func1(x: wf.FloatScalar, y: wf.FloatScalar, v: wf.Vector2):
    """A test function."""
    return (v.T * v)[0] * x + 5 * x * sym.pow(y, 3.1)


def func2(x: wf.Vector2, y: wf.Vector2, z: wf.FloatScalar):
    """Another test function with some output args."""
    m = sym.where(
        z > x[1],
        (x * y.transpose() + sym.eye(2) * sym.cos(z / x[0])).squared_norm(),
        z * 5,
    )
    diff_x = sym.vector(m).jacobian(x)
    diff_y = sym.vector(m).jacobian(y)
    diff_z = sym.vector(m).jacobian([z])
    return [
        wf.ReturnValue(m),
        wf.OutputArg(diff_x, name="diff_x", is_optional=False),
        wf.OutputArg(diff_y, name="diff_y", is_optional=False),
        wf.OutputArg(diff_z, name="diff_z", is_optional=True),
    ]


@dataclasses.dataclass
class Point2d:
    """A custom type."""

    x: wf.FloatScalar
    y: wf.FloatScalar


def rotate_point(angle: wf.FloatScalar, p: Point2d):
    R = sym.matrix([[sym.cos(angle), -sym.sin(angle)], [sym.sin(angle), sym.cos(angle)]])
    p_rotated = R * sym.vector(p.x, p.y)
    p_out = Point2d(*p_rotated)
    return [wf.OutputArg(p_out, name="p_rotated")]


class OpaqueType(wf.Opaque):
    """An external type we will pass to our test external function."""


external_func = external_functions.declare_external_function(
    "external_func",
    arguments=[("foo", OpaqueType), ("bar", wf.FloatScalar)],
    return_type=wf.FloatScalar,
)


def opaque_type_func(u: OpaqueType, x: wf.FloatScalar, y: wf.FloatScalar):
    """Use an opaque type as an argument to a generated function."""
    f = external_func(u, x * y + 3)
    return f + sym.cos(x * y)


@dataclasses.dataclass
class NestedType1:
    x: wf.FloatScalar
    y: wf.FloatScalar


@dataclasses.dataclass
class NestedType2:
    v: wf.Vector2
    z: wf.FloatScalar


@dataclasses.dataclass
class NestedType3:
    """A custom type that uses two other custom types."""

    fizz: NestedType1
    buzz: NestedType2


def nested_type_func(arg0: NestedType1, arg1: NestedType3):
    """Ingest custom types and do something with them."""
    return (
        sym.cos(arg0.x * arg0.y) / sym.pow(arg1.fizz.x, arg1.buzz.v.norm())
        + arg1.fizz.y * arg1.buzz.z
    )


class CustomCppGenerator(wf.CppGenerator):
    """Customize C++ generation."""

    def format_call_std_function(self, element: ast.CallStdFunction) -> str:
        if element.function == wf.StdMathFunction.Cos:
            return f"custom::cos({self.format(element.args[0])})"
        return self.super_format(element)

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        if external_func == element.function:
            args = ", ".join(self.format(x) for x in element.args)
            return f"custom::{element.function.name}({args})"
        return self.super_format(element)


class CodeGenerationWrapperTest(MathTestBase):
    @staticmethod
    def _run_generators(
        definition: ast.FunctionDefinition | typing.Sequence[ast.FunctionDefinition],
    ):
        """Run the generators just to make sure we don't hit an assertion."""
        if isinstance(definition, ast.FunctionDefinition):
            definition = (definition,)

        for d in definition:
            for generator in [CustomCppGenerator(), wf.RustGenerator()]:
                generator.generate(d)

    def _check_arg(
        self,
        arg: Argument,
        name: str,
        t: type_info.ScalarType | type_info.MatrixType | type_info.CustomType,
        direction: ArgumentDirection,
    ):
        self.assertEqual(name, arg.name)
        self.assertEqual(t, arg.type)
        self.assertEqual(direction, arg.direction)

    def _test_cse_function_description(self, desc: wf.FunctionDescription):
        """
        Run compute_output_expressions and validate that we can re-assemble
        """
        expected_outputs: dict[OutputKey, sym.AnyExpression] = desc.output_expressions()

        params = wf.OptimizationParams()
        params.factorization_passes = 0
        cse_outputs, intermediate_values = wf.cse_function_description(desc, params)

        for key, expected in expected_outputs.items():
            self.assertTrue(key in cse_outputs)
            csed_expr: sym.AnyExpression = cse_outputs[key]

            # TODO: This is not efficient, but good enough for this test for now.
            # subs(...) needs a mode where order is respected to address this.
            for var, val in reversed(intermediate_values):
                csed_expr = sym.subs(csed_expr, target=var, replacement=val)

            # We need to distribute here to check equivalence properly.
            # This is because the substitution order means some constants won't be distributed over
            # additions.
            self.assertIdentical(sym.distribute(expected), sym.distribute(csed_expr))

    def test_func1(self):
        """Test simple function that returns a scalar."""
        desc = wf.create_function_description(func1)
        self.assertIsInstance(desc, wf.FunctionDescription)
        self.assertEqual("func1", desc.name)

        definition = wf.transpile(desc)
        self.assertIsInstance(definition, ast.FunctionDefinition)
        self.assertIsInstance(definition.body, ast.AstSpan)
        self.assertIsInstance(definition.signature, ast.FunctionSignature)

        # Check we can introspect the resulting definition/signature:
        sig = definition.signature
        self.assertEqual("func1", sig.name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), sig.return_type)

        self._check_arg(
            sig.arguments[0],
            "x",
            type_info.ScalarType(type_info.NumericType.Float),
            ArgumentDirection.Input,
        )
        self._check_arg(
            sig.arguments[1],
            "y",
            type_info.ScalarType(type_info.NumericType.Float),
            ArgumentDirection.Input,
        )
        self._check_arg(
            sig.arguments[2],
            "v",
            type_info.MatrixType(2, 1),
            ArgumentDirection.Input,
        )

        self._run_generators(definition)
        self._test_cse_function_description(desc)

    def test_func2(self):
        """Test function with optional output arguments."""
        desc = wf.create_function_description(func2)
        definition = wf.transpile(desc)
        sig = definition.signature

        self.assertEqual("FunctionDescription('func2', 6 args)", repr(desc))
        self.assertEqual("func2", sig.name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), sig.return_type)

        self.assertEqual("Argument(x: matrix_type<2, 1>)", repr(sig.arguments[0]))
        self.assertEqual("Argument(z: floating_point)", repr(sig.arguments[2]))
        self._check_arg(
            sig.arguments[0],
            "x",
            type_info.MatrixType(2, 1),
            direction=ArgumentDirection.Input,
        )
        self._check_arg(
            sig.arguments[1],
            "y",
            type_info.MatrixType(2, 1),
            direction=ArgumentDirection.Input,
        )
        self._check_arg(
            sig.arguments[2],
            "z",
            type_info.ScalarType(type_info.NumericType.Float),
            direction=ArgumentDirection.Input,
        )

        # check output args:
        self._check_arg(
            sig.arguments[3],
            "diff_x",
            type_info.MatrixType(1, 2),
            direction=ArgumentDirection.Output,
        )
        self._check_arg(
            sig.arguments[4],
            "diff_y",
            type_info.MatrixType(1, 2),
            direction=ArgumentDirection.Output,
        )
        self._check_arg(
            sig.arguments[5],
            "diff_z",
            type_info.MatrixType(1, 1),
            direction=ArgumentDirection.OptionalOutput,
        )

        self._run_generators(definition=definition)
        self._test_cse_function_description(desc=desc)

    def test_custom_type(self):
        """Test function that uses a custom type."""
        desc = wf.create_function_description(rotate_point)
        definition = wf.transpile(desc)
        sig = definition.signature

        self.assertEqual("rotate_point", sig.name)
        self.assertEqual("p", sig.arguments[1].name)

        ctype = typing.cast(type_info.CustomType, sig.arguments[1].type)
        self.assertIsInstance(ctype, type_info.CustomType)
        self.assertEqual(f"CustomType('Point2d', 2 fields, {repr(Point2d)})", repr(ctype))
        self.assertEqual("Point2d", ctype.name)
        self.assertEqual(Point2d, ctype.python_type)
        self.assertEqual(2, ctype.total_size)
        self.assertEqual("x", ctype.fields[0].name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), ctype.fields[0].type)
        self.assertEqual("y", ctype.fields[1].name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), ctype.fields[1].type)

        self._run_generators(definition)
        self._test_cse_function_description(desc)

    def test_opaque_func(self):
        """Test we can customize invocation of a custom function."""
        code = wf.generate_function(opaque_type_func, generator=CustomCppGenerator())
        self.assertEqual(
            "external_func(foo: OpaqueType, bar: floating_point) -> floating_point",
            repr(external_func),
        )
        self.assertTrue("custom::external_func" in code)

    def test_nested_type_func(self):
        """Test we can create a function description for a complicated nested type."""
        desc = wf.create_function_description(nested_type_func)
        definition = wf.transpile(desc)

        ctype = typing.cast(type_info.CustomType, definition.signature.arguments[1].type)
        self.assertIsInstance(ctype, type_info.CustomType)
        self.assertEqual("NestedType3", ctype.name)
        self.assertEqual(NestedType3, ctype.python_type)
        self.assertEqual(5, ctype.total_size)
        self._run_generators(definition)

    def test_boolean_args_disallowed(self):
        """Test that boolean arguments are disallowed."""

        class Boolean(sym.Expr):
            # This is illegal.
            NUMERIC_PRIMITIVE_TYPE = type_info.NumericType.Bool

        def boolean_arg_func(x: Boolean, y: wf.FloatScalar):
            return [
                wf.ReturnValue(5 + y * x),
                wf.OutputArg(x, name="x_out"),
            ]

        self.assertRaises(
            TypeError,
            lambda: wf.generate_function(func=boolean_arg_func, generator=wf.RustGenerator()),
        )


if __name__ == "__main__":
    unittest.main(verbosity=2)
