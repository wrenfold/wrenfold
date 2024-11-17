"""
Run code-coverage report locally.
"""

import argparse
import os
import subprocess
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent.absolute()


def main(args: argparse.Namespace):
    if args.build_dir is None:
        args.build_dir = REPO_ROOT / "build"
    if args.gcov_tool is None:
        args.gcov_tool = "gcov"

    build_dir = Path(args.build_dir).absolute()
    output_dir = Path(args.output).absolute()
    os.makedirs(str(output_dir), exist_ok=True)

    # Generate coverage summary:
    subprocess.check_call([
        "lcov",
        "--gcov-tool",
        args.gcov_tool,
        "--directory",
        str(build_dir),
        "--capture",
        "--output-file",
        str(output_dir / "coverage.info"),
    ])

    # Filter the summary:
    subprocess.check_call([
        "lcov",
        "--gcov-tool",
        args.gcov_tool,
        "--remove",
        str(output_dir / "coverage.info"),
        "-o",
        str(output_dir / "filtered.info"),
        "/usr/include/*",
        "/usr/lib/*",
        "*/dependencies/*",
        "*/core/tests/*",
        "*/core/test_support/*",
    ])

    # Generate HTML report:
    subprocess.check_call([
        "genhtml",
        str(output_dir / "filtered.info"),
        "--output-directory",
        str(output_dir),
    ])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-b", "--build-dir", type=str, help="Path to build-directory")
    parser.add_argument("-o", "--output", type=str, help="Output directory", required=True)
    parser.add_argument("-g", "--gcov-tool", type=str, help="Path to gcov command")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
