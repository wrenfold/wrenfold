"""
Source code for the `advanced_derivatives.rst` file.
"""
# yapf: disable

# [potential_energy_start]
from wrenfold import sym

m, g, r, t = sym.symbols("m, g, r, t")

# Theta as a function of time:
theta = sym.Function("theta")(t)
theta_dot = theta.diff(t)
print(theta_dot)  # prints: Derivative(theta(t), t)

# Potential energy of the pendulum mass:
T = m * g * r * sym.cos(theta)
print(T)  # prints: g*m*r*cos(theta(t))
# [potential_energy_end]

# [kinetic_energy_start]
# Position of the pendulum mass:
p_mass = r * sym.vector(sym.cos(theta), sym.sin(theta))
# Velocity of the pendulum mass:
v_mass = p_mass.diff(t)
# Kinetic energy:
V = sym.rational(1, 2) * m * v_mass.squared_norm()

# Simplify the expression by substituting: cos(theta)^2 + sin(theta)^2 --> 1
V = (
    V.distribute()
    .collect([m, r, theta_dot])
    .subs((sym.cos(theta) ** 2 + sym.sin(theta) ** 2) / 2, sym.rational(1, 2))
)
print(V)  # prints: m*r**2*Derivative(theta(t), t)**2/2
# [kinetic_energy_end]

# [lagrangian_start]
# Compute canonical momentum.
# Note that we take the derivative wrt `Derivative(theta(t), t)`.
L = T - V
q_theta = L.diff(theta_dot)

# And the Euler-Lagrange equation, which involves taking the derivative wrt `theta(t)`.
euler_lagrange = q_theta.diff(t) - L.diff(theta)
print(euler_lagrange)  # prints: g*m*r*sin(theta(t)) - m*r**2*Derivative(theta(t), t, 2)
# [lagrangian_end]
