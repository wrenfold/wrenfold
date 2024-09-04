"""Generate expressions for cpp_generation_test_2 and rust_generation_test_2."""
import argparse
import dataclasses

from wrenfold import ast, code_generation, sym, type_annotations, type_info
from wrenfold.external_functions import declare_external_function


@dataclasses.dataclass
class StructType:
    """A minimal custom type we will return from an external function."""
    x: type_annotations.FloatScalar
    y: type_annotations.FloatScalar


class VectorOfStructs(type_annotations.Opaque):
    """
    This is a placeholder that will be generated as std::vector<StructType> or
    std::vec::Vec<StructType>.
    """

    def interpolate_access(self, x: sym.Expr) -> StructType:
        """
        We assume a user-defined external function that accepts a value `x` and uses it to linearly
        interpolate between values in our `VectorOfStructs`.
        """
        return vector_interpolate_access(vec=self, x=x)


vector_interpolate_access = declare_external_function(
    name="interpolate_access",
    arguments=[("vec", VectorOfStructs), ("x", type_annotations.FloatScalar)],
    return_type=StructType)


def lookup_and_compute_inner_product(vec: VectorOfStructs, a: type_annotations.FloatScalar,
                                     b: type_annotations.FloatScalar):
    """
    A simplified test case that calls a user-provided function to access two elements in a vector,
    then computes the inner product between them.

    This is so we can test: Pass an opaque type to a generated function, and then forward it to an
    external function.
    """
    first = vec.interpolate_access(x=a)
    second = vec.interpolate_access(x=b)
    return first.x * second.x + first.y * second.y


class CustomCppGenerator(code_generation.CppGenerator):

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        """
        Place our external functions in the `external` namespace.
        """
        args = ', '.join(self.format(x) for x in element.args)
        return f'external::{element.function.name}({args})'

    def format_custom_type(self, element: type_info.CustomType) -> str:
        """
        Place `StructType` in `types` namespace. Format vector name.
        """
        if element.python_type == StructType:
            return f'types::{element.name}'
        elif element.python_type == VectorOfStructs:
            return f'std::vector<types::StructType>'
        return self.super_format(element)


class CustomRustGenerator(code_generation.RustGenerator):
    """
    We need a similar set of customizations for Rust as well.
    """

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        args = ', '.join(self.format(x) for x in element.args)
        return f'crate::external::{element.function.name}({args})'

    def format_custom_type(self, element: type_info.CustomType) -> str:
        if element.python_type == StructType:
            return f'crate::types::{element.name}'
        elif element.python_type == VectorOfStructs:
            return f'std::vec::Vec<crate::types::StructType>'
        return self.super_format(element)


def main(args: argparse.Namespace):
    descriptions = [
        code_generation.create_function_description(
            func=lookup_and_compute_inner_product, name="lookup_and_compute_inner_product")
    ]

    definitions = code_generation.transpile(descriptions)
    if args.language == "cpp":
        code = CustomCppGenerator().generate(definitions)
        code = CustomCppGenerator.apply_preamble(code=code, namespace="gen")
    else:
        code = CustomRustGenerator().generate(definitions)
        code = CustomRustGenerator.apply_preamble(code=code)

    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="__doc__")
    parser.add_argument("output", type=str, help="Output path")
    parser.add_argument(
        "--language", type=str, choices=["cpp", "rust"], required=True, help="Target language.")
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
