"""
Source code for the `external_functions.rst` file.
"""

import wrenfold as wf
from wrenfold import (
    ast,
    external_functions,
    sym,
    type_info,
)


# [lookup_table_start]
class LookupTable(wf.Opaque):
    """
    A placeholder we will map to our actual type during the code-generation step.
    """

    pass  # [lookup_table_end]


# [interpolate_table_start]
interpolate_table = external_functions.declare_external_function(
    name="interpolate_table",
    arguments=[("table", LookupTable), ("arg", wf.FloatScalar)],
    return_type=wf.FloatScalar,
)  # [interpolate_table_end]


# [function_definition_start]
def lookup_angle(table: LookupTable, p_0: wf.Vector2, p_1: wf.Vector2):
    """
    Compute bearing angle between two points, and use it as an argument to our lookup table.
    """
    v = p_1 - p_0
    angle = sym.atan2(v[1], v[0])

    # Normalize between [0, 1] (where 0 corresponds to -pi, and 1 corresponds to pi).
    angle_normalized = (angle + sym.pi) / (2 * sym.pi)

    # Perform the lookup.
    table_value = interpolate_table(table=table, arg=angle_normalized)

    # Do some more symbolic operations with the result:
    result = table_value * (p_1 - p_0).squared_norm()
    return [
        wf.ReturnValue(result),
    ]
    # [function_definition_end]


# [code_generator_start]
class CustomCppGenerator(wf.CppGenerator):
    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        """
        Place our external function in the ``utilities`` namespace.
        """
        if element.function == interpolate_table:
            args = ", ".join(self.format(x) for x in element.args)
            return f"utilities::{element.function.name}({args})"
        return self.super_format(element)

    def format_custom_type(self, element: type_info.CustomType) -> str:
        """
        Assume the lookup table is implemented as a std::vector<double>.
        """
        if element.python_type == LookupTable:
            return "std::vector<double>"
        return self.super_format(element)


code = wf.generate_function(func=lookup_angle, generator=CustomCppGenerator())
print(code)
# [code_generator_end]
