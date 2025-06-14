"""
Source code for the `custom_types.rst` file.
"""

import dataclasses

from wrenfold import ast, code_generation, sym, type_annotations, type_info


# [dataclass_declaration_start]
@dataclasses.dataclass
class Vec2:
    """Symbolic version of geo::vec2."""

    x: type_annotations.FloatScalar
    y: type_annotations.FloatScalar

    def to_vector(self) -> sym.MatrixExpr:
        return sym.vector(self.x, self.y)

    @classmethod
    def from_vector(cls, v: sym.MatrixExpr):
        assert v.shape == (2, 1)
        return cls(v[0], v[1])
        # [dataclass_declaration_end]


# [function_definition_start]
def rotate_vector(angle: type_annotations.FloatScalar, v: Vec2):
    """Rotate vector `v` by `angle` radians."""
    R = sym.matrix([[sym.cos(angle), -sym.sin(angle)], [sym.sin(angle), sym.cos(angle)]])
    v_rot = R * v.to_vector()

    # Compute the jacobian of `v_rot` wrt `angle`.
    # This is a 2x1 vector, so we'll put it in Vec2.
    v_rot_diff = v_rot.jacobian([angle])

    # We also want to return `Vec2`:
    return [
        code_generation.ReturnValue(Vec2.from_vector(v_rot)),
        code_generation.OutputArg(Vec2.from_vector(v_rot_diff), name="v_rot_D_angle"),
    ]
    # [function_definition_end]


# [code_generator_start]
class CustomCppGenerator(code_generation.CppGenerator):
    def format_get_field(self, element: ast.GetField) -> str:
        """
        geo::vec2 members are private, so call the accessor method instead:
        """
        if element.struct_type.python_type == Vec2:
            return f"{self.format(element.arg)}.{element.field_name}()"
        return self.super_format(element)

    def format_custom_type(self, element: type_info.CustomType) -> str:
        """
        Place our custom type into the `geo` namespace.
        """
        if element.python_type == Vec2:
            return "geo::vec2"
        return self.super_format(element)
        # [code_generator_end]


# [rust_code_generator_start]
class CustomRustGenerator(code_generation.RustGenerator):
    def format_get_field(self, element: ast.GetField) -> str:
        if element.struct_type.python_type == Vec2:
            return f"{self.format(element.arg)}.{element.field_name}()"
        return self.super_format(element)

    def format_custom_type(self, element: type_info.CustomType) -> str:
        """
        Place our custom type into the `geo` crate.
        """
        if element.python_type == Vec2:
            return "geo::Vec2"
        return self.super_format(element)

    def format_construct_custom_type(self, element: ast.ConstructCustomType) -> str:
        if element.type.python_type == Vec2:
            x = self.format(element.get_field_value("x"))
            y = self.format(element.get_field_value("y"))
            return f"geo::Vec2::new({x}, {y})"
        return self.super_format(element)
        # [rust_code_generator_end]


# [transpilation_start]
code = code_generation.generate_function(func=rotate_vector, generator=CustomCppGenerator())
print(code)
