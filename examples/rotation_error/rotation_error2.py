"""
Example: Tangent-space delta between two quaternions, with tangent-space jacobians.

This is an example of a relatively simple factor one might implement for a rotation
averaging optimization.
"""

from pathlib import Path

import numpy as np
import wrenfold as wf
from wrenfold.geometry import Quaternion


def rotation_error(q0_xyzw: wf.Vector4, q1_xyzw: wf.Vector4, q_0_1_xyzw: wf.Vector4):
    """
    Tangent-space difference between two scalar-last quaternions, scaled by
    a positive weight value.
    """
    q0: Quaternion = Quaternion.from_xyzw(q0_xyzw)
    q1: Quaternion = Quaternion.from_xyzw(q1_xyzw)
    q01: Quaternion = Quaternion.from_xyzw(q_0_1_xyzw)

    error = (q01.conjugate() * (q0.conjugate() * q1)).to_rotation_vector(epsilon=1.0e-16)
    D0 = error.jacobian(q0_xyzw)
    D1 = error.jacobian(q1_xyzw)

    return [
        wf.ReturnValue(error),
        wf.OutputArg(D0, name="d0", is_optional=True),
        wf.OutputArg(D1, name="d1", is_optional=True),
    ]


if __name__ == "__main__":
    rotation_error_wf, gen_code = wf.generate_python(
        func=rotation_error, generator=wf.PythonGenerator(use_output_arguments=True)
    )

    (Path(__file__).parent / "output.py").write_text(gen_code)

    q = np.array([0.0, 0.0, 0.0, 1.0])
    jac0 = np.zeros((3, 4))
    jac1 = np.zeros((3, 4))
    error = rotation_error_wf(q, q, q, jac0, jac1)
    print(error)
