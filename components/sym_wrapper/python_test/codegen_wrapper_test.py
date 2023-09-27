"""
Test of code-generation functionality.
"""
import importlib.util
import numpy as np
import os
import shutil
import tempfile
import typing as T
import unittest

from sym import sym
from sym.type_annotations import RealScalar, Vector2
from sym.code_generation import codegen_function, PythonCodeGenerator, create_numeric_evaluator

from test_base import MathTestBase


def func1(x: RealScalar, y: RealScalar, v: Vector2):
    """A test function."""
    return (v.T * v) * x + 5 * x * sym.pow(y, 3.1)


def func2(x: RealScalar, y: RealScalar):
    """Another test function."""
    return sym.where(x > y / 2, sym.cos(x - y), sym.sin(x + y * 2))


class CodeGenerationWrapperTest(MathTestBase):

    def __init__(self, methodName: str = "runTest") -> None:
        super().__init__(methodName)
        self._tmp_dir: T.Optional[str] = None

    def setUp(self) -> None:
        self._tmp_dir = tempfile.mkdtemp(prefix='code_gen_wrapper_test_')
        print(f'Temporary directory: {self._tmp_dir}')
        return super().setUp()

    def tearDown(self) -> None:
        if self._tmp_dir is not None:
            shutil.rmtree(self._tmp_dir, ignore_errors=True)
        return super().tearDown()

    def load_code(self, code: str, name: str) -> T.Callable:
        """Write out generated code into a module, and import it for use."""
        fd, file_path = tempfile.mkstemp(prefix=name + "_", suffix='.py', dir=self._tmp_dir)
        with os.fdopen(fd, 'w') as handle:
            handle.write('import numpy as np\n\n')
            handle.write(code)
            handle.flush()

        module_name, _ = os.path.splitext(os.path.basename(file_path))
        spec = importlib.util.spec_from_file_location(module_name, file_path)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return getattr(mod, name)

    def test_code_generation_1(self):
        signature, ast = codegen_function(func1)
        code = PythonCodeGenerator().generate(signature=signature, body=ast)

        generated_func = self.load_code(code=code, name=func1.__name__)
        numeric_func = create_numeric_evaluator(func=func1)

        val_generated = generated_func(0.5, 1.2, np.array([0., 1.2]))
        val_subs = numeric_func(0.5, 1.2, np.array([0., 1.2]))
        self.assertAlmostEqual(val_subs, val_generated, places=16)

    def test_code_generation_2(self):
        signature, ast = codegen_function(func2)
        code = PythonCodeGenerator().generate(signature=signature, body=ast)

        generated_func = self.load_code(code=code, name=func2.__name__)
        numeric_func = create_numeric_evaluator(func=func2)

        self.assertAlmostEqual(numeric_func(1.6, 0.4), generated_func(1.6, 0.4), places=16)
        self.assertAlmostEqual(numeric_func(0.2, 2.1), generated_func(0.2, 2.1), places=16)


if __name__ == '__main__':
    unittest.main(verbosity=2)
