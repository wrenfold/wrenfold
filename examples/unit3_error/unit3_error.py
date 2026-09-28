"""Example: A two-dimensional tangent error between two unit vectors."""

import argparse

import wrenfold as wf
from wrenfold.geometry import Unit3


def unit3_error(v0_xyz: wf.Vector3, v1_xyz: wf.Vector3, weight: wf.FloatScalar):
    """Compute a weighted sphere error and tangent derivatives.

    Both inputs must already have unit length; Unit3 does not normalize them.
    The two derivative outputs are 2x2 matrices in each input's tangent frame.
    """
    v0 = Unit3(v0_xyz)
    v1 = Unit3(v1_xyz)

    error = weight * v0.local_coordinates(v1)
    D0 = error.jacobian(v0_xyz) * v0.retract_derivative()
    D1 = error.jacobian(v1_xyz) * v1.retract_derivative()

    return [
        wf.OutputArg(error, name="error"),
        wf.OutputArg(D0, name="d0", is_optional=True),
        wf.OutputArg(D1, name="d1", is_optional=True),
    ]


def main(args: argparse.Namespace):
    generator = wf.CppGenerator()
    eigen_generator = wf.CppGenerator(wf.CppMatrixTypeBehavior.Eigen)
    code = wf.generate_function(func=unit3_error, generator=generator)
    code += "\n\n"
    code += wf.generate_function(
        func=unit3_error,
        generator=eigen_generator,
        name=f"{unit3_error.__name__}_eigen",
    )
    code = generator.apply_preamble(code, namespace="gen", imports="#include <Eigen/Core>")
    wf.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
