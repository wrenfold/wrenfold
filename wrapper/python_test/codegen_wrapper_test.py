"""
Test of code-generation functionality.
"""
import unittest

import mc

from test_base import MathTestBase


class CodeGenerationWrapperTest(MathTestBase):

    def test_ir_builder(self):
        x, y, z = mc.symbols('x, y, z')
        f = x * x + mc.sin(y / z) + mc.pow(y / z, 3 * x * x)

        builder = mc.IrBuilder([f])
        output_value = builder.output_values()[0]

        self.assertIdentical(f, builder.create_expression(output_value))

        builder.eliminate_duplicates()
        self.assertIdentical(f, builder.create_expression(output_value))


if __name__ == '__main__':
    unittest.main(verbosity=2)
