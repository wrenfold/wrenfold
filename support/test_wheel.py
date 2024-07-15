"""
Run commands intended to test a built python wheel.
"""
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).parent.parent.absolute()


def main():
    # Run all the unit tests.
    print("Running package tests...")
    subprocess.check_call([
        sys.executable, "-m", "unittest", "discover",
        str(ROOT / "components" / "wrapper" / "tests"), "--pattern", "*_test.py"
    ])

    # Run all the generators to make sure they don't crash.
    print("Running all examples...")
    ex = ROOT / "examples"
    subprocess.check_call([sys.executable, str(ex / "python_generation" / "python_generation.py")])

    for script in [
        [ex / "bspline" / "bspline.py", "--language", "cpp"],
        [ex / "bspline" / "bspline.py", "--language", "rust"],
        [ex / "custom_types" / "custom_types_gen.py", "--language", "cpp"],
        [ex / "custom_types" / "custom_types_gen.py", "--language", "rust"],
        [ex / "imu_integration" / "imu_integration.py"],
        [ex / "motion_planning" / "problem.py"],
        [ex / "quaternion_interpolation" / "quaternion_interpolation.py"],
        [ex / "rosenbrock" / "rosenbrock.py"],
        [ex / "rotation_error" / "rotation_error.py"],
    ]:
        with tempfile.NamedTemporaryFile("w", prefix="codegen_") as fd:
            fd.close()  # Need to close it so we can overwrite the file.
            cmd = " ".join([str(x) for x in script])
            print(f'Running: {cmd}')
            subprocess.check_call([sys.executable] + [str(x) for x in script] + [fd.name])


if __name__ == '__main__':
    main()
