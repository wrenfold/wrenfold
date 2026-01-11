"""
Example: Tangent-space delta between two quaternions, with tangent-space jacobians.

This is an example of a relatively simple factor one might implement for a rotation
averaging optimization.
"""

import argparse

import wrenfold as wf
from wrenfold.geometry import Quaternion


def rotation_error(q0_xyzw: wf.Vector4, q1_xyzw: wf.Vector4, weight: wf.FloatScalar):
    """
    Tangent-space difference between two scalar-last quaternions, scaled by
    a positive weight value.
    """
    q0: Quaternion = Quaternion.from_xyzw(q0_xyzw)
    q1: Quaternion = Quaternion.from_xyzw(q1_xyzw)

    error = (q0.conjugate() * q1).to_rotation_vector(epsilon=1.0e-16) * weight
    D0 = error.jacobian(q0.to_vector_wxyz()) * q0.right_retract_derivative()
    D1 = error.jacobian(q1.to_vector_wxyz()) * q1.right_retract_derivative()

    return [
        wf.OutputArg(error, name="error"),
        wf.OutputArg(D0, name="d0", is_optional=True),
        wf.OutputArg(D1, name="d1", is_optional=True),
    ]


def main(args: argparse.Namespace):
    # For this example, we'll generate our function twice:
    # - Once with the default CppGenerator (functions use generics in signatures).
    # - And once with `CppMatrixTypeBehavior.Eigen` (functions use Eigen types in signatures).
    generator = wf.CppGenerator()
    eigen_generator = wf.CppGenerator(wf.CppMatrixTypeBehavior.Eigen)
    code = wf.generate_function(func=rotation_error, generator=generator)
    code += "\n\n"
    code += wf.generate_function(
        func=rotation_error,
        generator=eigen_generator,
        name=f"{rotation_error.__name__}_eigen",
    )
    code = generator.apply_preamble(code, namespace="gen", imports="#include <Eigen/Core>")
    wf.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
