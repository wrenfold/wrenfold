"""
Generate some example functions in C:
- Kannala-Brandt camera model (forward and backward projection).
- A method that converts a rotation matrix to a quaternion.
"""

import argparse
import dataclasses

import wrenfold as wf
from wrenfold import geometry

from ..shared_expressions import (
    kb_camera_projection_with_jacobians,
    kb_camera_unprojection_with_jacobians,
)
from .c_code_generator import C_PREAMBLE, CCodeGenerator


@dataclasses.dataclass
class quaternion_t:
    """
    The equivalent C-struct is declared in `c_span_types.h`.
    """

    w: wf.FloatScalar
    x: wf.FloatScalar
    y: wf.FloatScalar
    z: wf.FloatScalar


def quaternion_from_rotation_matrix(R: wf.Matrix3):
    """
    We use this method to test construction of a struct in C (quaternion_t).
    """
    q = geometry.Quaternion.from_rotation_matrix(R)
    return quaternion_t(w=q.w, x=q.x, y=q.y, z=q.z)


def main(args: argparse.Namespace):
    functions = [
        wf.generate_function(function, generator=CCodeGenerator())
        for function in (
            kb_camera_projection_with_jacobians,
            kb_camera_unprojection_with_jacobians,
            quaternion_from_rotation_matrix,
        )
    ]
    code = C_PREAMBLE.format(code="\n\n".join(functions), header_name="C_GENERATION_EXAMPLE")
    wf.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
