"""
Generate the Rosenbrock function and its first derivative.
"""

import argparse

import wrenfold as wf
from wrenfold import sym


def rosenbrock(
    a: wf.FloatScalar,
    b: wf.FloatScalar,
    xy: wf.Vector2,
):
    """Evaluate the Rosenbrock function."""
    x, y = xy

    # We formulate function `h(x, y)` such that f(x, y) = h^T * h = (a - x)**2 + b*(y - x**2)**2
    # Then we can find the minima by doing ordinary NLS on residual h(x, y)
    h = sym.vector(a - x, sym.sqrt(b) * (y - x**2))
    (f,) = h.T * h

    J = sym.jacobian(h, xy)
    return (
        wf.ReturnValue(f),
        wf.OutputArg(h, name="h"),
        wf.OutputArg(J, name="h_D_xy", is_optional=True),
    )


def main(args: argparse.Namespace):
    # For this example, emit function signatures that use Eigen types directly:
    generator = wf.CppGenerator(wf.CppMatrixTypeBehavior.Eigen)
    code = wf.generate_function(rosenbrock, generator=generator)
    code = generator.apply_preamble(code, namespace="gen")
    wf.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
