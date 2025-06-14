"""
Run commands intended to test a built python wheel.
"""

import glob
import os
import subprocess
import sys
import tempfile
import typing as T
from pathlib import Path

ROOT = Path(__file__).parent.parent.absolute()


def get_module_path(script_path: Path | str) -> str:
    if isinstance(script_path, str):
        script_path = Path(script_path)
    test_path = script_path.with_suffix("").relative_to(ROOT)
    return str(test_path).replace(os.path.sep, ".")


def main():
    # Run all the unit tests.
    print("Running package tests...")
    wrapper_test_dir = ROOT / "components" / "wrapper" / "tests"
    for filename in glob.glob(str(wrapper_test_dir / "*_test.py")):
        subprocess.check_call([sys.executable, "-m", get_module_path(filename)], cwd=str(ROOT))

    # Run all the generators to make sure they don't crash.
    print("Running all examples...")
    ex = ROOT / Path("examples")
    for script, needs_output_file in [
        ([ex / "bspline" / "bspline.py", "--language", "cpp"], True),
        ([ex / "bspline" / "bspline.py", "--language", "rust"], True),
        ([ex / "c_generation" / "c_generation.py"], True),
        ([ex / "custom_types" / "custom_types_gen.py", "--language", "cpp"], True),
        ([ex / "custom_types" / "custom_types_gen.py", "--language", "rust"], True),
        ([ex / "imu_integration" / "imu_integration.py"], True),
        ([ex / "motion_planning" / "problem.py"], True),
        ([ex / "quaternion_interpolation" / "quaternion_interpolation.py"], True),
        ([ex / "rosenbrock" / "rosenbrock.py"], True),
        ([ex / "rotation_error" / "rotation_error.py"], True),
    ]:
        command = [sys.executable, "-m", get_module_path(script[0])] + script[1:]
        print(f"Running: {' '.join([str(x) for x in command])}")

        if needs_output_file:
            with tempfile.NamedTemporaryFile("w", prefix="codegen_") as fd:
                fd.close()  # Need to close it so we can overwrite the file.
                subprocess.check_call(command + [fd.name], cwd=str(ROOT))
        else:
            subprocess.check_call(command, cwd=str(ROOT))


if __name__ == "__main__":
    main()
