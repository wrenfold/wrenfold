"""Code that we insert into the quick_start.rst document."""

# [function_def_start]
import wrenfold as wf
from wrenfold import sym


def rosenbrock(
    xy: wf.Vector2,
    a: wf.FloatScalar,
    b: wf.FloatScalar,
):
    """Evaluates the Rosenbrock function and its first derivative wrt `x` and `y`."""
    x, y = xy
    f = (a - x) ** 2 + b * (y - x**2) ** 2
    return (
        wf.ReturnValue(f),
        wf.OutputArg(sym.jacobian([f], xy), name="f_D_xy"),
    )
    # [function_def_end]


# [transpilation_start]
# Generate the function as C++, and apply some boilerplate (imports and namespace).
generator = wf.CppGenerator()
cpp = wf.generate_function(rosenbrock, generator=generator)
cpp = generator.apply_preamble(cpp, namespace="gen")
print(cpp)
# [transpilation_end]

# [rust_transpilation_start]
rust = wf.generate_function(rosenbrock, generator=wf.RustGenerator())
rust = wf.RustGenerator.apply_preamble(rust)
print(rust)
# [rust_transpilation_end]
