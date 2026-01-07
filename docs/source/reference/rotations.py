"""
Source code for the `rotations.rst` file.
"""
# yapf: disable

# [jacobian_computation_start]
from wrenfold import sym
from wrenfold.geometry import Quaternion

q = Quaternion.with_name("q")
p = sym.vector(*sym.make_symbols("p_x", "p_y", "p_z"))

# Rotate `p` by the Quaternion `q`:
p_rot = q.to_rotation_matrix() * p

# Compute the jacobian wrt `q` (method 1):
J1 = sym.jacobian(p_rot, q.to_vector_wxyz()) * q.right_retract_derivative()

# Compute the jacobian wrt `q` (method 2):
dv = sym.vector(*sym.make_symbols("v_x", "v_y", "v_z"))
p_rot = (
    Quaternion.from_wxyz(
        q.to_vector_wxyz() + q.right_retract_derivative() * dv
    ).to_rotation_matrix()
    * p
)
J2 = sym.subs(sym.jacobian(p_rot, dv), [(x, 0) for x in dv])

print(J1.distribute() - J2.distribute())  # prints: [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
# [jacobian_computation_end]
