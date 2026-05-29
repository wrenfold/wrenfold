import argparse
import dataclasses

import wrenfold as wf
from wrenfold import ast, sym, type_info
from wrenfold.external_functions import declare_external_function


@dataclasses.dataclass
class CustomInvocationResult:
    """
    A custom type that corresponds to a C++ type we will return from an external function.
    """

    f: wf.FloatScalar
    df: wf.FloatScalar
    ddf: wf.FloatScalar


class LookupTable(wf.Opaque):
    """
    This is a placeholder that represents the storage for our lookup table. We use this to denote
    the extra argument that our generated function will take.
    """


acos_lookup_table = declare_external_function(
    name="acos_lookup_table",
    arguments=[("table", LookupTable), ("x", wf.FloatScalar)],
    return_type=CustomInvocationResult,
)


def lookup_method(
    table: LookupTable,
    a: wf.Vector3,
):
    """
    A toy function that uses a lookup table
    """
    b = sym.vector(-0.5, 0.23, 0.33)
    dot = (a.T * b)[0, 0] / (a.norm() * b.norm())

    # We replace
    lookup_result = acos_lookup_table(table=table, x=dot)

    # To take derivatives, we create a placeholder symbolic function `f`.
    dot_sym = sym.symbol("dot")
    placeholder_f = sym.Function("f_lookup")(dot_sym)

    # Then we use the placeholder in downstream expressions:
    result = sym.exp(-placeholder_f / sym.pi * 0.25)

    D1 = result.diff(dot_sym) * sym.jacobian([dot], a)
    D1 = D1.subs(placeholder_f.diff(dot_sym), lookup_result.df).subs(placeholder_f, lookup_result.f)

    # Second dervative of g(h(x)) computed via:
    # d[g(h(x))]/dx^2 = g''(h(x)) * h'(x)**2 + g'(h(x)) * h''(x)
    ax, ay, az = a[0, 0], a[1, 0], a[2, 0]
    D2 = (
        result.diff(dot_sym, 2) * sym.jacobian([dot], a).unary_map(lambda x: x**2) +
        result.diff(dot_sym) * sym.row_vector(dot.diff(ax, 2), dot.diff(ay, 2), dot.diff(az, 2))
    )  # fmt: off
    D2 = (
        D2.subs(placeholder_f.diff(dot_sym, 2), lookup_result.ddf)
        .subs(placeholder_f.diff(dot_sym), lookup_result.df)
        .subs(placeholder_f, lookup_result.f)
    )

    return [
        wf.ReturnValue(result.subs(placeholder_f, lookup_result.f)),
        wf.OutputArg(D1, name="D1", is_optional=True),
        wf.OutputArg(D2, name="D2", is_optional=True),
    ]


class CustomCppGenerator(wf.CppGenerator):
    """
    We need to customize code-generation to indicate how our external functions and types should be
    described in code.
    """

    def format_call_external_function(self, element: ast.CallExternalFunction) -> str:
        args = ", ".join(self.format(x) for x in element.args)
        return f"external::{element.function.name}({args})"

    def format_custom_type(self, element: type_info.CustomType) -> str:
        if element.python_type == CustomInvocationResult:
            return f"external::{element.name}"
        elif element.python_type == LookupTable:
            # Our lookup table is just a vector of values.
            return "std::vector<double>"
        return self.super_format(element)


def main(args: argparse.Namespace):
    generator = CustomCppGenerator()
    code = wf.generate_function(func=lookup_method, generator=generator)
    code = generator.apply_preamble(code=code, namespace="gen")
    wf.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
