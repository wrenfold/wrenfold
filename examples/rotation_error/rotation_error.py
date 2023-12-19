"""
Example: Tangent-space delta between two quaternions, with tangent-space jacobians.

This is an example of a relatively simple factor one might implement for a rotation
averaging optimization.
"""
import argh
import typing as T

from wrenfold.type_annotations import RealScalar, Vector4
from wrenfold import code_generation
from wrenfold.code_generation import OutputArg, CppGenerator

from wrenfold.geometry import Quaternion


def rotation_error(q0_xyzw: Vector4, q1_xyzw: Vector4, weight: RealScalar):
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
        OutputArg(error, name='error'),
        OutputArg(D0, name='d0', is_optional=True),
        OutputArg(D1, name='d1', is_optional=True)
    ]


def main(output: str):
    description = code_generation.create_function_description(rotation_error)
    definition = code_generation.transpile(description=description)
    code = CppGenerator().generate(definition=definition)
    code = code_generation.apply_cpp_preamble(code, namespace="gen")

    if output is not None:
        code_generation.mkdir_and_write_file(code=code, path=output)
    else:
        print(code)


if __name__ == '__main__':
    argh.dispatch_command(main)
