"""
Test of code-generation functionality.

Most of this logic is actually tested by the python examples, which are executed as tests. This
test just evaluates that we can call the wrapper methods without crashing.

In future when I add back python code-generation we can import some outputs directly and invoke them.
"""
import unittest
import dataclasses

from wrenfold import ast
from wrenfold import code_generation
from wrenfold import external_functions
from wrenfold import sym
from wrenfold.code_generation import ReturnValue, OutputArg
from wrenfold.enumerations import StdMathFunction
from wrenfold.type_annotations import RealScalar, Vector2, Opaque

from test_base import MathTestBase


def func1(x: RealScalar, y: RealScalar, v: Vector2):
    """A test function."""
    return (v.T * v)[0] * x + 5 * x * sym.pow(y, 3.1)


def func2(x: RealScalar, y: RealScalar):
    """Another test function."""
    return sym.where(x > y / 2, sym.cos(x - y), sym.sin(x + y * 2))


def func3(x: Vector2, y: Vector2, z: RealScalar):
    """Another test function."""
    m = sym.where(z > x[1], (x * y.transpose() + sym.eye(2) * sym.cos(z / x[0])).squared_norm(),
                  z * 5)
    diff_x = sym.vector(m).jacobian(x)
    diff_y = sym.vector(m).jacobian(y)
    diff_z = sym.vector(m).jacobian([z])
    return [
        ReturnValue(m),
        OutputArg(diff_x, name='diff_x', is_optional=False),
        OutputArg(diff_y, name='diff_y', is_optional=False),
        OutputArg(diff_z, name='diff_z', is_optional=True),
    ]


@dataclasses.dataclass
class Point2d:
    """A custom type."""
    x: RealScalar
    y: RealScalar


def rotate_point(angle: RealScalar, p: Point2d):
    R = sym.matrix([[sym.cos(angle), -sym.sin(angle)], [sym.sin(angle), sym.cos(angle)]])
    p_rotated = R * sym.vector(p.x, p.y)
    p_out = Point2d(*p_rotated)
    return [OutputArg(p_out, name="p_rotated")]


class OpaqueType(Opaque):
    """An external type we will pass to our test external function."""


external_func = external_functions.declare_external_function(
    "external_func", arguments=[('foo', OpaqueType), ('bar', sym.Expr)], return_type=sym.Expr)


def opaque_type_func(u: OpaqueType, x: sym.Expr, y: sym.Expr):
    """Use an opaque type as an argument to a generated function."""
    f = external_func(u, x * y + 3)
    return f + sym.cos(x * y)


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

    def test_code_generation(self):
        descriptions = [
            code_generation.create_function_description(func1),
            code_generation.create_function_description(func2),
            code_generation.create_function_description(func3),
            code_generation.create_function_description(rotate_point),
            code_generation.create_function_description(opaque_type_func),
        ]
        definitions = code_generation.transpile(descriptions)

        generator = CustomCppGenerator()
        print(generator.generate(definitions))

        generator = code_generation.RustGenerator()
        print(generator.generate(definitions))


if __name__ == '__main__':
    unittest.main(verbosity=2)
