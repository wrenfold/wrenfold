"""
Example: Tangent-space delta between two quaternions, with tangent-space jacobians.

This is an example of a relatively simple factor one might implement for a rotation
averaging optimization.
"""

import argparse

from wrenfold import code_generation
from wrenfold.geometry import Quaternion
from wrenfold.type_annotations import FloatScalar, Vector4


def rotation_error(q0_xyzw: Vector4, q1_xyzw: Vector4, weight: FloatScalar):
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
        code_generation.OutputArg(error, name="error"),
        code_generation.OutputArg(D0, name="d0", is_optional=True),
        code_generation.OutputArg(D1, name="d1", is_optional=True),
    ]


def main(args: argparse.Namespace):
    generator = code_generation.CppGenerator()
    code = code_generation.generate_function(func=rotation_error, generator=generator)
    code = generator.apply_preamble(code, namespace="gen")
    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
