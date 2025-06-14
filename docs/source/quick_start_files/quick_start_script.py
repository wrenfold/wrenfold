"""Code that we insert into the quick_start.rst document."""

# [function_def_start]
from wrenfold import code_generation, sym, type_annotations


def rosenbrock(
    xy: type_annotations.Vector2,
    a: type_annotations.FloatScalar,
    b: type_annotations.FloatScalar,
):
    """Evaluates the Rosenbrock function and its first derivative wrt `x` and `y`."""
    x, y = xy
    f = (a - x) ** 2 + b * (y - x**2) ** 2
    return (
        code_generation.ReturnValue(f),
        code_generation.OutputArg(sym.jacobian([f], xy), name="f_D_xy"),
    )
    # [function_def_end]


# [transpilation_start]
# Generate the function as C++, and apply some boilerplate (imports and namespace).
generator = code_generation.CppGenerator()
cpp = code_generation.generate_function(rosenbrock, generator=generator)
cpp = generator.apply_preamble(cpp, namespace="gen")
print(cpp)
# [transpilation_end]

# [rust_transpilation_start]
rust = code_generation.generate_function(rosenbrock, generator=code_generation.RustGenerator())
rust = code_generation.RustGenerator.apply_preamble(rust)
print(rust)
# [rust_transpilation_end]
