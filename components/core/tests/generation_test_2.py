"""Generate expressions for cpp_generation_test_2 and rust_generation_test_2."""
import argparse
import dataclasses

from wrenfold import ast
from wrenfold import code_generation
from wrenfold import sym
from wrenfold.code_generation import CppGenerator, RustGenerator
from wrenfold.custom_types import Opaque
from wrenfold.external_functions import declare_external_function
from wrenfold.type_annotations import RealScalar


@dataclasses.dataclass
class StructType:
    """A minimal custom type we will return from an external function."""
    x: sym.Expr
    y: sym.Expr


class VectorOfStructs(Opaque):
    """This is a placeholder that will be generated as std::vector<StructType> or std::vec::Vec<StructType>."""

    def interpolate_access(self, x: sym.Expr) -> sym.Expr:
        """
        We assume a user-defined external function that accepts a value `x` and uses it to linearly interpolate
        between values in our `VectorOfStructs`.
        """
        return vector_interpolate_access(vec=self, x=x)


vector_interpolate_access = declare_external_function(
    name="interpolate_access",
    arguments=[("vec", VectorOfStructs), ("x", sym.Expr)],
    return_type=StructType)


def lookup_and_compute_inner_product(vec: VectorOfStructs, a: RealScalar, b: RealScalar):
    """
    A simplified test case that calls a user-provided function to access two elements in a vector,
    then computes the inner product between them.

    This is so we can test: Pass an opaque type to a generated function, and then forward it to an
    external function.
    """
    first = vec.interpolate_access(x=a)
    second = vec.interpolate_access(x=b)
    return first.x * second.x + first.y * second.y


class CustomCppGenerator(CppGenerator):

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        """
        Place our custom functions in the `external` namespace.
        """
        args = ', '.join(self.format(x) for x in element.args)
        return f'external::{element.function.name}({args})'

    def format_custom_type(self, element: code_generation.codegen.CustomType) -> str:
        """
        Place `StructType` in `types` namespace. Format vector name.
        """
        if element.python_type is StructType:
            return f'types::{element.name}'
        elif element.python_type is VectorOfStructs:
            return f'std::vector<types::StructType>'
        return self.super_format(element)

    def format_declaration_type_annotation(self, element: ast.DeclarationTypeAnnotation) -> str:
        """
        We need to define how to declare our custom types.
        TODO: Unify this with `format_custom_type`.
        """
        if isinstance(
                element.type,
                code_generation.codegen.CustomType) and element.type.python_type is StructType:
            return f'types::{element.type.name}'
        return self.super_format(element)


class CustomRustGenerator(RustGenerator):
    """
    We need a similar set of customizations for Rust as well.
    """

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        args = ', '.join(self.format(x) for x in element.args)
        return f'crate::external::{element.function.name}({args})'

    def format_custom_type(self, element: code_generation.codegen.CustomType) -> str:
        if element.python_type is StructType:
            return f'crate::types::{element.name}'
        elif element.python_type is VectorOfStructs:
            return f'std::vec::Vec<crate::types::StructType>'
        return self.super_format(element)

    def format_declaration_type_annotation(self, element: ast.DeclarationTypeAnnotation) -> str:
        if isinstance(
                element.type,
                code_generation.codegen.CustomType) and element.type.python_type is StructType:
            return f'crate::types::{element.type.name}'
        return self.super_format(element)


def main(args: argparse.Namespace):
    descriptions = [
        code_generation.create_function_description(
            func=lookup_and_compute_inner_product, name="lookup_and_compute_inner_product")
    ]

    definitions = code_generation.transpile(descriptions=descriptions)
    if args.language == "cpp":
        code = CustomCppGenerator().generate(definitions=definitions)
        code = code_generation.apply_cpp_preamble(code=code, namespace="gen")
    else:
        code = CustomRustGenerator().generate(definitions=definitions)
        code = code_generation.apply_rust_preamble(code=code)

    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="__doc__")
    parser.add_argument("output", type=str, help="Output path")
    parser.add_argument(
        "--language", type=str, choices=["cpp", "rust"], required=True, help="Target language.")
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
