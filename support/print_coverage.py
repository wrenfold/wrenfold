"""
Print the code-coverage percentage. Invoked from coverage.yml
"""
import re
import subprocess
import sys
from pathlib import Path


def main():
    output = subprocess.check_output(
        ["lcov", "--gcov-tool", sys.argv[1], "--summary",
         Path(sys.argv[2]).absolute()])
    output = output.decode("utf-8")
    # Pull the percentage coverage out:
    regex = re.compile(r"lines\.+:\s+([0-9.]+)\%", flags=re.IGNORECASE)
    percentage, = regex.findall(output)
    print(f"{percentage}%")


if __name__ == "__main__":
    main()
