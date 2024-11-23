"""
Specify the dynamics of cart-pole system with a double-pendulum attached.
"""

import dataclasses
import sys
import typing as T

from wrenfold import code_generation, sym, type_annotations, type_info

from .sympy_helpers import get_euler_lagrange_coefficients, get_mat_inverse


@dataclasses.dataclass
class CartPoleParamsSymbolic:
    """Parameters of the cart-double-pole system."""

    m_b: type_annotations.FloatScalar  # Mass of the base/cart (kg)
    m_1: type_annotations.FloatScalar  # Mass of the first pole (kg).
    m_2: type_annotations.FloatScalar  # Mass of the second pole (kg).
    l_1: type_annotations.FloatScalar  # Length of the first pole (m).
    l_2: type_annotations.FloatScalar  # Length of the second pole (m).
    g: type_annotations.FloatScalar  # Gravity (m/s^2).
    mu_b: type_annotations.FloatScalar  # Coefficient of static friction.
    v_mu_b: type_annotations.FloatScalar  # Velocity cutoff of smooth friction model (m/s).
    c_d: type_annotations.FloatScalar  # Aero-coefficient of poles (N / (m/s)^2).
    x_s: type_annotations.FloatScalar  # Bounding spring position (m).
    k_s: type_annotations.FloatScalar  # Bounding spring constant (N / m).


def get_cart_double_pole_dynamics() -> T.Callable:
    """
    Return a symbolic function that evaluates the dynamics of a cart-mounted double pendulum.

    We derive the necessary expressions symbolically, and then create the function
    `cart_double_pole_dynamics` that specifies exactly what we want code-generated.
    """
    t = sym.symbols("t", real=True)

    # Position of the base + angles as a function of time.
    # We create symbolic functions to represent these, and then substitute in the function arguments
    # below.
    b_x = sym.Function("b_x")(t)
    th_1 = sym.Function("th_1")(t)
    th_2 = sym.Function("th_2")(t)

    # First derivatives of the base + angles:
    b_x_dot = b_x.diff(t)
    th_1_dot = th_1.diff(t)
    th_2_dot = th_2.diff(t)

    # Mass of the base, and two weights + lever arm lengths:
    m_b, m_1, m_2, l_1, l_2 = sym.symbols("m_b, m_1, m_2, l_1, l_2", real=True, positive=True)

    # Gravity:
    g = sym.symbols("g", real=True)

    # Friction coefficient on the base, and the cutoff velocity of the smooth Coulomb model.
    mu_b, v_mu_b = sym.symbols("mu_b, v_mu_b", real=True)

    # Air drag coefficient on the mass:
    # This is summarized as: rho * C_d * A, where rho is air density, and A is cross-sectional area.
    c_d = sym.symbols("c_d_1", real=True)

    # Position and spring coefficient on the boundary of the workspace.
    x_s, k_s = sym.symbols("x_s, k_s", real=True)

    # Positions of base, and two weights.
    b = sym.vector(b_x, 0)
    p_1 = b + sym.vector(sym.cos(th_1), sym.sin(th_1)) * l_1
    p_2 = p_1 + sym.vector(sym.cos(th_2), sym.sin(th_2)) * l_2

    b_dot = b.diff(t)
    p_1_dot = p_1.diff(t)
    p_2_dot = p_2.diff(t)

    # Compute kinetic energy. This is the sum of (1/2)*m*v^2 for all pieces.
    half = 1 / sym.integer(2)
    T: sym.Expr = (
        half * m_b * b_dot.squared_norm() + half * m_1 * p_1_dot.squared_norm() +
        half * m_2 * p_2_dot.squared_norm())

    # Simplify this a bit by eliminating: cos^2(x) + sin^2(x) --> 1
    T = (
        T.distribute().collect([m_1, m_2, l_1, l_2, th_1_dot, th_2_dot]).subs(
            (sym.cos(th_1) ** 2 + sym.sin(th_1) ** 2) / 2, half).subs(
                (sym.cos(th_2) ** 2 + sym.sin(th_2) ** 2) / 2, half))

    # Compute potential energy. This is the sum of m*g*y for all pieces.
    V = g * m_1 * p_1[1] + g * m_2 * p_2[1]

    # The lagrangian:
    L: sym.Expr = T - V

    # Canonical momenta:
    q_b = L.diff(b_x_dot)
    q_th_1 = L.diff(th_1_dot)
    q_th_2 = L.diff(th_2_dot)

    # Dissipative force due to friction on the base.
    F_friction_base = (-mu_b * (m_1 + m_2 + m_b) * g * sym.tanh(b_x_dot / sym.max(v_mu_b, 1.0e-6)))

    # Dissipative _power_ due to air drag on the pendulum mass.
    # We use a `where` statement to guard against a singularity in the Jacobian.
    D_air_mass_1 = ((sym.integer(1) / 6) * c_d * sym.where(p_1_dot.squared_norm() > 0,
                                                           p_1_dot.norm() ** 3, 0))
    D_air_mass_2 = ((sym.integer(1) / 6) * c_d * sym.where(p_2_dot.squared_norm() > 0,
                                                           p_2_dot.norm() ** 3, 0))
    D_air_mass = D_air_mass_1 + D_air_mass_2

    # External force from the boundary spring:
    F_s_right = -k_s * sym.max(0, b_x - x_s)
    F_s_left = k_s * sym.max(0, -x_s - b_x)

    # Form the Euler-Lagrange equations (each of these is equal to zero).
    el_b = ((q_b.diff(t) - L.diff(b_x)).distribute() - F_friction_base - F_s_left - F_s_right +
            D_air_mass.diff(b_x_dot))
    el_th_1 = ((q_th_1.diff(t) - L.diff(th_1)).distribute() + D_air_mass.diff(th_1_dot))
    el_th_2 = ((q_th_2.diff(t) - L.diff(th_2)).distribute() + D_air_mass.diff(th_2_dot))

    # Reformulate the Euler-Lagrange equations into form:
    #   A(x, x') * x'' = f(x, x', u)
    A, f = get_euler_lagrange_coefficients(
        euler_lagrange=[el_b, el_th_1, el_th_2],
        second_derivatives=[b_x.diff(t, 2), th_1.diff(t, 2),
                            th_2.diff(t, 2)],
    )

    M_inv, m_symbols = get_mat_inverse(dim=3)
    substitutions = list(zip(m_symbols, A.to_flat_list()))

    # Compute expressions for x'' = A(x, x')^-1 * f(x, x', u)
    x_ddot = M_inv.subs(substitutions) * f

    def cart_double_pole_dynamics(
        params: CartPoleParamsSymbolic,
        x: type_annotations.Vector6,
    ):
        """
        A function we can code-generate that specifies how to compute the derivative of `x` as a
        function of time.
        """
        # We substitute `vel_states` first so that Derivative(x(t), t) is replaced first.
        # Then we can replace x(t).
        states = list(zip([b_x, th_1, th_2], x[:3].to_flat_list()))
        vel_states = list(zip([b_x_dot, th_1_dot, th_2_dot], x[3:].to_flat_list()))

        constants = [
            (m_b, params.m_b),
            (m_1, params.m_1),
            (m_2, params.m_2),
            (l_1, params.l_1),
            (l_2, params.l_2),
            (g, params.g),
            (mu_b, params.mu_b),
            (v_mu_b, params.v_mu_b),
            (c_d, params.c_d),
            (x_s, params.x_s),
            (k_s, params.k_s),
        ]

        x_ddot_subbed = x_ddot.subs(constants).subs(vel_states).subs(states)
        total_energy = (T + V).subs(constants).subs(vel_states).subs(states)

        # Stack the first derivative with the second derivative.
        # This is how we get the 6-element derivative of our state vector.
        x_dot_out = sym.vstack([x[3:], x_ddot_subbed])

        # Compute Jacobian of the state derivative wrt the state itself.
        J_x = sym.jacobian(x_dot_out, x)

        return [
            code_generation.OutputArg(x_dot_out, name="x_dot"),
            code_generation.OutputArg(total_energy, name="energy", is_optional=True),
            code_generation.OutputArg(J_x, name="J_x", is_optional=True),
        ]

    return cart_double_pole_dynamics


class CustomRustGenerator(code_generation.RustGenerator):
    """Custom formatter to place `CartPoleParams` in the crate:: scope."""

    def format_custom_type(self, custom: type_info.CustomType):
        if custom.python_type == CartPoleParamsSymbolic:
            return "crate::CartPoleParams"
        return self.super_format(custom)


def main():
    code = code_generation.generate_function(
        func=get_cart_double_pole_dynamics(), generator=CustomRustGenerator())
    code = CustomRustGenerator.apply_preamble(code=code)
    code_generation.mkdir_and_write_file(code, sys.argv[1])


if __name__ == "__main__":
    main()
