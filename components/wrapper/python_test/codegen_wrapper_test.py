"""
Test of code-generation functionality.

Most of this logic is actually tested by the python examples, which are executed as tests. This
test just evaluates that we can call the wrapper methods without crashing.

In future when I add back python code-generation we can import some outputs directly and invoke them.
"""
import unittest

from wrenfold import sym
from wrenfold.type_annotations import RealScalar, Vector2
from wrenfold.code_generation import ReturnValue, OutputArg
from wrenfold import code_generation

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


class CodeGenerationWrapperTest(MathTestBase):

    def test_code_generation(self):
        descriptions = [
            code_generation.create_function_description(func1),
            code_generation.create_function_description(func2),
            code_generation.create_function_description(func3),
        ]
        definitions = code_generation.transpile(descriptions=descriptions)
        code_generation.generate_cpp(definitions=definitions)
        code_generation.generate_rust(definitions=definitions)


if __name__ == '__main__':
    unittest.main(verbosity=2)
