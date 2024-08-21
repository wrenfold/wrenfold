"""
Test the code-generation wrapper.

Most of the output code generation is tested by cpp_generation_test/rust_generation_test. The
examples are also run as tests - these offer a lot more diverse coverage. The purpose of this
test is just to validate that the wrapper works and exposes the correct members.
"""
import dataclasses
import typing as T
import unittest

from test_base import MathTestBase

from wrenfold import ast, code_generation, external_functions, sym, type_info
from wrenfold.enumerations import StdMathFunction
from wrenfold.type_annotations import FloatScalar, Opaque, Vector2


def func1(x: FloatScalar, y: FloatScalar, v: Vector2):
    """A test function."""
    return (v.T * v)[0] * x + 5 * x * sym.pow(y, 3.1)


def func2(x: Vector2, y: Vector2, z: FloatScalar):
    """Another test function with some output args."""
    m = sym.where(z > x[1], (x * y.transpose() + sym.eye(2) * sym.cos(z / x[0])).squared_norm(),
                  z * 5)
    diff_x = sym.vector(m).jacobian(x)
    diff_y = sym.vector(m).jacobian(y)
    diff_z = sym.vector(m).jacobian([z])
    return [
        code_generation.ReturnValue(m),
        code_generation.OutputArg(diff_x, name='diff_x', is_optional=False),
        code_generation.OutputArg(diff_y, name='diff_y', is_optional=False),
        code_generation.OutputArg(diff_z, name='diff_z', is_optional=True),
    ]


@dataclasses.dataclass
class Point2d:
    """A custom type."""
    x: FloatScalar
    y: FloatScalar


def rotate_point(angle: FloatScalar, p: Point2d):
    R = sym.matrix([[sym.cos(angle), -sym.sin(angle)], [sym.sin(angle), sym.cos(angle)]])
    p_rotated = R * sym.vector(p.x, p.y)
    p_out = Point2d(*p_rotated)
    return [code_generation.OutputArg(p_out, name="p_rotated")]


class OpaqueType(Opaque):
    """An external type we will pass to our test external function."""


external_func = external_functions.declare_external_function(
    "external_func", arguments=[('foo', OpaqueType), ('bar', FloatScalar)], return_type=FloatScalar)


def opaque_type_func(u: OpaqueType, x: FloatScalar, y: FloatScalar):
    """Use an opaque type as an argument to a generated function."""
    f = external_func(u, x * y + 3)
    return f + sym.cos(x * y)


@dataclasses.dataclass
class NestedType1:
    x: FloatScalar
    y: FloatScalar


@dataclasses.dataclass
class NestedType2:
    v: Vector2
    z: FloatScalar


@dataclasses.dataclass
class NestedType3:
    """A custom type that uses two other custom types."""
    fizz: NestedType1
    buzz: NestedType2


def nested_type_func(arg0: NestedType1, arg1: NestedType3):
    """Ingest custom types and do something with them."""
    return sym.cos(arg0.x * arg0.y) / sym.pow(arg1.fizz.x, arg1.buzz.v.norm()) + \
        arg1.fizz.y * arg1.buzz.z


class CustomCppGenerator(code_generation.CppGenerator):
    """Customize C++ generation."""

    def format_call_std_function(self, element: ast.CallStdFunction) -> str:
        if element.function == StdMathFunction.Cos:
            return f'custom::cos({self.format(element.args[0])})'
        return self.super_format(element)

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        if external_func == element.function:
            args = ', '.join(self.format(x) for x in element.args)
            return f'custom::{element.function.name}({args})'
        return self.super_format(element)


class CodeGenerationWrapperTest(MathTestBase):

    @staticmethod
    def _run_generators(definition: T.Union[ast.FunctionDefinition,
                                            T.Sequence[ast.FunctionDefinition]]):
        """Run the generators just to make sure we don't hit an assertion."""
        if isinstance(definition, ast.FunctionDefinition):
            definition = (definition,)

        for d in definition:
            for generator in [CustomCppGenerator(), code_generation.RustGenerator()]:
                generator.generate(d)

    def _check_arg(self, arg: code_generation.Argument, name: str,
                   t: T.Union[type_info.ScalarType, type_info.MatrixType,
                              type_info.CustomType], direction: code_generation.ArgumentDirection):
        self.assertEqual(name, arg.name)
        self.assertEqual(t, arg.type)
        self.assertEqual(direction, arg.direction)

    def _test_cse_function_description(self, desc: code_generation.FunctionDescription):
        """
        Run compute_output_expressions and validate that we can re-assemble
        """
        expected_outputs: T.Dict[code_generation.OutputKey,
                                 sym.AnyExpression] = desc.output_expressions()

        params = code_generation.OptimizationParams()
        params.factorization_passes = 0
        cse_outputs, intermediate_values = code_generation.cse_function_description(desc, params)

        for key, expected in expected_outputs.items():
            self.assertTrue(key in cse_outputs)
            csed_expr: sym.AnyExpression = cse_outputs[key]

            # TODO: This is not efficient, but good enough for this test for now.
            # subs(...) needs a mode where order is respected to address this.
            for var, val in reversed(intermediate_values):
                csed_expr = sym.subs(csed_expr, target=var, replacement=val)

            # We need to distribute here to check equivalence properly.
            # This is because the substitution order means some constants won't be distributed over additions.
            self.assertIdentical(sym.distribute(expected), sym.distribute(csed_expr))

    def test_func1(self):
        """Test simple function that returns a scalar."""
        desc = code_generation.create_function_description(func1)
        self.assertIsInstance(desc, code_generation.FunctionDescription)
        self.assertEqual('func1', desc.name)

        definition = code_generation.transpile(desc)
        self.assertIsInstance(definition, ast.FunctionDefinition)
        self.assertIsInstance(definition.body, ast.AstSpan)
        self.assertIsInstance(definition.signature, ast.FunctionSignature)

        # Check we can introspect the resulting definition/signature:
        sig = definition.signature
        self.assertEqual('func1', sig.name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), sig.return_type)

        self._check_arg(sig.arguments[0], 'x', type_info.ScalarType(type_info.NumericType.Float),
                        code_generation.ArgumentDirection.Input)
        self._check_arg(sig.arguments[1], 'y', type_info.ScalarType(type_info.NumericType.Float),
                        code_generation.ArgumentDirection.Input)
        self._check_arg(sig.arguments[2], 'v', type_info.MatrixType(2, 1),
                        code_generation.ArgumentDirection.Input)

        self._run_generators(definition)
        self._test_cse_function_description(desc)

    def test_func2(self):
        """Test function with optional output arguments."""
        desc = code_generation.create_function_description(func2)
        definition = code_generation.transpile(desc)
        sig = definition.signature

        self.assertEqual('func2', sig.name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), sig.return_type)

        self._check_arg(
            sig.arguments[0],
            'x',
            type_info.MatrixType(2, 1),
            direction=code_generation.ArgumentDirection.Input)
        self._check_arg(
            sig.arguments[1],
            'y',
            type_info.MatrixType(2, 1),
            direction=code_generation.ArgumentDirection.Input)
        self._check_arg(
            sig.arguments[2],
            'z',
            type_info.ScalarType(type_info.NumericType.Float),
            direction=code_generation.ArgumentDirection.Input)

        # check output args:
        self._check_arg(
            sig.arguments[3],
            'diff_x',
            type_info.MatrixType(1, 2),
            direction=code_generation.ArgumentDirection.Output)
        self._check_arg(
            sig.arguments[4],
            'diff_y',
            type_info.MatrixType(1, 2),
            direction=code_generation.ArgumentDirection.Output)
        self._check_arg(
            sig.arguments[5],
            'diff_z',
            type_info.MatrixType(1, 1),
            direction=code_generation.ArgumentDirection.OptionalOutput)

        self._run_generators(definition=definition)
        self._test_cse_function_description(desc=desc)

    def test_custom_type(self):
        """Test function that uses a custom type."""
        desc = code_generation.create_function_description(rotate_point)
        definition = code_generation.transpile(desc)
        sig = definition.signature

        self.assertEqual('rotate_point', sig.name)
        self.assertEqual('p', sig.arguments[1].name)

        ctype = sig.arguments[1].type
        self.assertIsInstance(ctype, type_info.CustomType)
        self.assertEqual('Point2d', ctype.name)
        self.assertEqual(Point2d, ctype.python_type)
        self.assertEqual(2, ctype.total_size)
        self.assertEqual('x', ctype.fields[0].name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), ctype.fields[0].type)
        self.assertEqual('y', ctype.fields[1].name)
        self.assertEqual(type_info.ScalarType(type_info.NumericType.Float), ctype.fields[1].type)

        self._run_generators(definition)
        self._test_cse_function_description(desc)

    def test_opaque_func(self):
        """Test we can customize invocation of a custom function."""
        code = code_generation.generate_function(opaque_type_func, generator=CustomCppGenerator())
        self.assertTrue('custom::external_func' in code)

    def test_nested_type_func(self):
        """Test we can create a function description for a complicated nested type."""
        desc = code_generation.create_function_description(nested_type_func)
        definition = code_generation.transpile(desc)

        ctype = definition.signature.arguments[1].type
        self.assertIsInstance(ctype, type_info.CustomType)
        self.assertEqual('NestedType3', ctype.name)
        self.assertEqual(NestedType3, ctype.python_type)
        self.assertEqual(5, ctype.total_size)
        self._run_generators(definition)


if __name__ == '__main__':
    unittest.main(verbosity=2)
