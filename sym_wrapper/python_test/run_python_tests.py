"""
Indirection for running the python tests. This is required because we cannot set python
path correctly from cmake on windows.
"""
import os
import platform
import sys
import subprocess
from pathlib import Path

SCRIPT_PATH = Path(os.path.realpath(__file__)).parent


def main():
    binary_dir = Path(os.getenv('CMAKE_BINARY_DIR')).absolute()

    # set python path and run tests
    env = dict(os.environ)
    if platform.system() == "Windows":
        sep = ";"
    else:
        sep = ":"
    env['PYTHONPATH'] = f'{str(binary_dir)}{sep}{str(SCRIPT_PATH.parent.parent)}'

    for name in ['codegen_wrapper_test.py', 'expression_wrapper_test.py', 'matrix_wrapper_test.py']:
        subprocess.check_call([sys.executable, "-B", str(SCRIPT_PATH / name)], env=env)


if __name__ == '__main__':
    main()
