"""
Example: Tangent-space delta between two quaternions, with tangent-space jacobians.

This is an example of a relatively simple factor one might implement for a rotation
averaging optimization.
"""
from sym.type_annotations import RealScalar, Vector4
from sym.code_generation import codegen_function
from sym.code_generation import OutputArg

from sym.geometry import Quaternion
from sym.codegen import generate_cpp


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


CODE_TEMPLATE = \
"""// Machine generated code.
#include <cmath>
#include <span.h>

namespace {namespace} {{

{code}

}} // namespace {namespace}
"""


def main():
    signature, ast = codegen_function(rotation_error)
    code = generate_cpp(signature=signature, ast=ast)
    code = CODE_TEMPLATE.format(code=code, namespace="gen")
    with open('generated.h', 'w') as handle:
        handle.write(code)
        handle.flush()


if __name__ == '__main__':
    main()
