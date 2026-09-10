"""Type-check the generated stubs and their public comparison operators."""

import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).parent.parent.absolute()


def main() -> int:
    env = os.environ.copy()
    env["MYPYPATH"] = os.pathsep.join(
        [
            str(ROOT / "components" / "wrapper" / "stubs"),
            str(ROOT / "components" / "python"),
        ]
    )
    return subprocess.call(
        [
            sys.executable,
            "-m",
            "mypy",
            str(ROOT / "components" / "wrapper" / "stubs" / "pywrenfold"),
            str(ROOT / "support" / "stub_typing_test.py"),
            "--no-incremental",
            "--show-error-codes",
        ],
        cwd=ROOT,
        env=env,
    )


if __name__ == "__main__":
    raise SystemExit(main())
