"""
Generate the Rosenbrock function and its first derivative.
"""

import argparse

from wrenfold import code_generation, sym, type_annotations


def rosenbrock(
    a: type_annotations.FloatScalar,
    b: type_annotations.FloatScalar,
    xy: type_annotations.Vector2,
):
    """Evaluate the Rosenbrock function."""
    x, y = xy

    # We formulate function `h(x, y)` such that f(x, y) = h^T * h = (a - x)**2 + b*(y - x**2)**2
    # Then we can find the mimina by doing ordinary NLS on residual h(x, y)
    h = sym.vector(a - x, sym.sqrt(b) * (y - x ** 2))
    f, = h.T * h

    J = sym.jacobian(h, xy)
    return (
        code_generation.ReturnValue(f),
        code_generation.OutputArg(h, name="h"),
        code_generation.OutputArg(J, name="h_D_xy", is_optional=True),
    )


def main(args: argparse.Namespace):
    code = code_generation.generate_function(rosenbrock, generator=code_generation.CppGenerator())
    code = code_generation.CppGenerator.apply_preamble(code, namespace="gen")
    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
