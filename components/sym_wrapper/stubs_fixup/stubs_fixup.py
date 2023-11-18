"""
Hacky build step to fix the imports in the generated stubs.
"""
import argparse
from pathlib import Path


def main(args: argparse.Namespace):
    for input_path_str in args.inputs:
        input_path = Path(input_path_str)
        output_path = Path(args.output_dir) / input_path.name

        with open(input_path, 'r') as handle:
            contents = handle.read()

        contents = contents.replace('import zen_pysym', 'from sym_wrapper import zen_pysym')

        print(f'Writing: {output_path}')
        with open(output_path, 'w') as handle:
            handle.write(contents)
            handle.flush()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--inputs', type=str, nargs='*', required=True, help='Input file path')
    parser.add_argument('--output-dir', type=str, required=True, help='Output file directory')
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
