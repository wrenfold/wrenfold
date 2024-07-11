"""
Generate quaternion interpolation with tangent-space derivatives using wrenfold.
"""
import argparse

from wrenfold import code_generation, sym
from wrenfold.geometry import Quaternion
from wrenfold.type_annotations import FloatScalar, Vector4


def quaternion_interpolation_impl(q0_xyzw: Vector4,
                                  q1_xyzw: Vector4,
                                  alpha: FloatScalar,
                                  use_conditional: bool = True):
    """
    Interpolate between two quaternions, `q0` and `q1` (passed as scalar-last vectors). The
    resulting expression is:

        q_out = q0 * exp(alpha * log(q0^T * q1))

    Outputs the interpolated quaternion, as well as the tangent space derivatives. If
    use_conditional=True, we insert conditional logic to handle the small-angle case. Otherwise, the
    small angle case is ignored.
    """
    q0: Quaternion = Quaternion.from_xyzw(q0_xyzw)
    q1: Quaternion = Quaternion.from_xyzw(q1_xyzw)

    # Compute log(q0^T * q1), the tangent-space rotation vector between the quaternions.
    w01 = (q0.conjugate() * q1).to_rotation_vector(epsilon=1.0e-16 if use_conditional else None)

    # Scale the vector by alpha, then apply it on the _right_ side of q0.
    q_out = q0 * Quaternion.from_rotation_vector(
        w01 * alpha, epsilon=1.0e-16 if use_conditional else None)

    # Use chain-rule to map the Jacobians from the quaternion elements to the tangent space.
    D0 = (
        q_out.right_local_coordinates_derivative() *
        sym.jacobian(q_out.to_vector_wxyz(), q0.to_vector_wxyz()) * q0.right_retract_derivative())
    D1 = (
        q_out.right_local_coordinates_derivative() *
        sym.jacobian(q_out.to_vector_wxyz(), q1.to_vector_wxyz()) * q1.right_retract_derivative())

    return (
        code_generation.OutputArg(q_out.to_vector_xyzw(), name="q_out"),
        code_generation.OutputArg(D0, name="d0", is_optional=True),
        code_generation.OutputArg(D1, name="d1", is_optional=True),
    )


def quaternion_interpolation(q0_xyzw: Vector4, q1_xyzw: Vector4, alpha: FloatScalar):
    return quaternion_interpolation_impl(q0_xyzw, q1_xyzw, alpha, use_conditional=True)


def quaternion_interpolation_no_conditional(q0_xyzw: Vector4, q1_xyzw: Vector4, alpha: FloatScalar):
    return quaternion_interpolation_impl(q0_xyzw, q1_xyzw, alpha, use_conditional=False)


def main(args: argparse.Namespace):
    code = str()
    for function in [quaternion_interpolation, quaternion_interpolation_no_conditional]:
        code += code_generation.generate_function(
            function, generator=code_generation.CppGenerator())
        code += "\n\n"

    code = code_generation.CppGenerator.apply_preamble(code=code, namespace="gen")
    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
