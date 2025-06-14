"""
Check that there is no git diff in the generated rust code for rust_generation_test. This is to
ensure the generated code that is checked-in is up to date.
"""

import argparse
import subprocess
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("filename", type=str)
    args = parser.parse_args()

    working_dir = Path(__file__).parent.absolute()
    try:
        subprocess.check_call(
            ["git", "diff", "--name-only", "--exit-code", str(args.filename)],
            cwd=str(working_dir),
        )
    except subprocess.CalledProcessError:
        print(f"git diff detected changes in file: {args.filename}")
        exit(1)


if __name__ == "__main__":
    main()
