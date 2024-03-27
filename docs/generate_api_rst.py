"""
Script that generates the `python_api_docs.rst` file. This
"""
import argparse
import inspect

from pathlib import Path

# The built library needs to be on the import path by this point.
from wrenfold import sym

PREFIX = """Python API Documentation
========================

"""


def main(args: argparse.Namespace):
    names = sorted([n for n in dir(sym) if not n.startswith('__')])

    functions = []
    classes = []

    for member_name in names:
        member = getattr(sym, member_name)
        if inspect.ismodule(member):
            continue
        if member.__doc__ is None or 'OMIT_FROM_SPHINX' in member.__doc__:
            continue

        # Pybind11 functions show up as built-in functions:
        if inspect.isbuiltin(member) or inspect.isfunction(member):
            functions.append(member_name)
        elif inspect.isclass(member):
            classes.append(member_name)
        elif isinstance(inspect, (sym.Expr, sym.BooleanExpr)):
            # TODO: Attach __doc__ to attributes? Presently there is no way to do this.
            pass

    contents = str(PREFIX)
    for klass in classes:
        contents += f'.. autoclass:: wrenfold.sym.{klass}\n  :members:\n\n'

    for func in functions:
        contents += f'.. autofunction:: wrenfold.sym.{func}\n\n'

    output_path = Path(args.output_dir) / 'python_api_docs.rst'
    with open(output_path, 'wb') as handle:
        handle.write(contents.encode('utf-8'))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_dir', type=str, help='Output directory write the RST file into.')
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
