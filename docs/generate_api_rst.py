"""
Script that generates the `python_api_docs.rst` file. This
"""
import argparse
import inspect
import typing as T
from pathlib import Path

# The built library needs to be on the import path by this point.
from wrenfold import (
    ast,
    code_generation,
    enumerations,
    exceptions,
    external_functions,
    geometry,
    sym,
    sympy_conversion,
    type_annotations,
    type_info,
)


def generate_rst_for_module(module: T.Any, module_name: str, output_dir: Path):
    """
    Write out rst files for classes + functions in the provided module.
    """
    functions = []
    classes = []
    for member_name in sorted(dir(module)):
        if member_name.startswith('_'):
            continue

        member = getattr(module, member_name)
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

    parts = [f'{module_name}\n{"=" * len(module_name)}']
    parts.extend(f'.. autofunction:: wrenfold.{module_name}.{func}' for func in functions)

    class_directives = "\n  ".join([
        ':members:', ':special-members:',
        ':exclude-members: __dict__,__weakref__,__repr__,__getstate__,__setstate__'
    ])
    parts.extend(
        f'.. autoclass:: wrenfold.{module_name}.{klass}\n  {class_directives}' for klass in classes)

    contents = '\n\n'.join(parts)
    contents += '\n'

    output_path = output_dir / f'{module_name}.rst'
    with open(output_path, 'wb') as handle:
        handle.write(contents.encode('utf-8'))


def main(args: argparse.Namespace):
    output_dir = Path(args.output_dir)
    generate_rst_for_module(module=sym, module_name="sym", output_dir=output_dir)
    generate_rst_for_module(module=geometry, module_name="geometry", output_dir=output_dir)
    generate_rst_for_module(module=ast, module_name="ast", output_dir=output_dir)
    generate_rst_for_module(
        module=code_generation, module_name="code_generation", output_dir=output_dir)
    generate_rst_for_module(
        module=sympy_conversion, module_name="sympy_conversion", output_dir=output_dir)
    generate_rst_for_module(
        module=type_annotations, module_name="type_annotations", output_dir=output_dir)
    generate_rst_for_module(module=exceptions, module_name="exceptions", output_dir=output_dir)
    generate_rst_for_module(module=enumerations, module_name="enumerations", output_dir=output_dir)
    generate_rst_for_module(module=type_info, module_name="type_info", output_dir=output_dir)
    generate_rst_for_module(
        module=external_functions, module_name="external_functions", output_dir=output_dir)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_dir', type=str, help='Output directory write the RST file into.')
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
