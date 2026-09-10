"""Type-check the generated stubs and representative public APIs."""

import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent.absolute()


def main() -> int:
    env = os.environ.copy()
    source_paths = [
        str(ROOT / "components" / "wrapper" / "stubs"),
        str(ROOT / "components" / "python"),
    ]
    env["MYPYPATH"] = os.pathsep.join(source_paths)
    env["PYTHONPATH"] = os.pathsep.join(source_paths)

    mypy_result = subprocess.call(
        [
            sys.executable,
            "-m",
            "mypy",
            str(ROOT / "components" / "wrapper" / "stubs" / "pywrenfold"),
            str(ROOT / "support" / "stub_typing_test.py"),
            "--no-incremental",
            "--show-error-codes",
            "--disallow-any-generics",
        ],
        cwd=ROOT,
        env=env,
    )
    if mypy_result != 0:
        return mypy_result

    return subprocess.call(
        [
            sys.executable,
            "-m",
            "basedpyright",
            str(ROOT / "support" / "stub_typing_test.py"),
            "--pythonpath",
            sys.executable,
        ],
        cwd=ROOT,
        env=env,
    )


if __name__ == "__main__":
    raise SystemExit(main())
