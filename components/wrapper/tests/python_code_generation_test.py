"""
Test generation and execution of python code.
"""
import typing as T
import unittest

from test_base import MathTestBase


class PythonCodeGenerationBase(MathTestBase):

    def test_simple_operations(self):
        """Test a few simple operations."""


class NumPyCodeGenerationTest(PythonCodeGenerationBase):
    """Run with NumPy configuration."""


class JaxCodeGenerationTest(PythonCodeGenerationBase):
    """Run with JAX configuration."""


class PyTorchCodeGenerationTest(PythonCodeGenerationBase):
    """Run with PyTorch configuration."""


def main():
    test_cases = [NumPyCodeGenerationTest, JaxCodeGenerationTest, PyTorchCodeGenerationTest]
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite)


if __name__ == "__main__":
    main()
