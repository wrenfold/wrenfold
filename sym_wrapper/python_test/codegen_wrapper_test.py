"""
Test of code-generation functionality.
"""
import unittest

from sym import sym
from sym.type_annotations import RealScalar, Vector2
from sym.code_generation import codegen_function
from sym_wrapper import pycodegen as codegen

from test_base import MathTestBase


def func1(x: RealScalar, y: RealScalar, v: Vector2):
    """A test function."""
    return (v.T * v) * x + 5 * x * pow(y, 3.1)


class CodeGenerationWrapperTest(MathTestBase):

    def test_ir_builder(self):
        x, y, z = sym.symbols('x, y, z')
        f = x * x + sym.sin(y / z) + sym.pow(y / z, 3 * x * x)

        builder = codegen.IrBuilder([f])
        output_value = builder.output_values()[0]

        self.assertIdentical(f, builder.create_expression(output_value))

        builder.eliminate_duplicates()
        self.assertIdentical(f, builder.create_expression(output_value))

        codegen_function(func1)


if __name__ == '__main__':
    unittest.main(verbosity=2)
