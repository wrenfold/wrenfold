"""
Generate the cart-pole dynamics as NumPy and JAX functions.
"""

import dataclasses
import typing as T
import unittest

import jax
import jax.numpy as jnp
import numpy as np

from wrenfold import code_generation, type_info

from .cart_pole_dynamics import (
    CartPoleParamsSymbolic,
    get_cart_double_pole_dynamics,
)


@dataclasses.dataclass
class CartPoleParamsNumeric:
    """
    Numeric equivalent of `CartPoleParamsSymbolic`.
    """

    m_b: float
    m_1: float
    m_2: float
    l_1: float
    l_2: float
    g: float


class CustomPythonGenerator(code_generation.PythonGenerator):

    def format_custom_type(self, target: type_info.CustomType):
        if target.python_type == CartPoleParamsSymbolic:
            return f"CartPoleParamsNumeric"
        return self.super_format(target)


def rk4(x: np.ndarray, h: float, f: T.Callable[[np.ndarray], np.ndarray]):
    """4th order runge kutta."""
    k1 = f(x)
    k2 = f(x + k1 * h / 2)
    k3 = f(x + k2 * h / 2)
    k4 = f(x + k3 * h)
    return x + (h / 6) * (k1 + k2 * 2 + k3 * 2 + k4)


class CartPoleTest(unittest.TestCase):

    def test_energy_conservation(self):
        """
        Generate the cart-double-pole dynamics as a NumPy/Python function, and test that we can
        integrate the state forward while conserving energy.
        """
        np_func, _ = code_generation.generate_python(
            func=get_cart_double_pole_dynamics(),
            target=code_generation.PythonGeneratorTarget.NumPy,
            context={
                "CartPoleParamsNumeric": CartPoleParamsNumeric
            },
            generator_type=CustomPythonGenerator,
        )

        params = CartPoleParamsNumeric(m_b=1.0, m_1=0.25, m_2=0.25, l_1=0.3, l_2=0.15, g=9.81)

        # Initial state is motionless, w/ the pendulum suspended upwards somewhat:
        x = np.array([0.0, np.pi / 4, np.pi / 4, 0.0, 0.0, 0.0]).reshape(-1, 1)

        # Compute energy at the start of simulation:
        energy_initial = np_func(
            params=params, x=x, compute_energy=True, compute_J_x=False)["energy"]

        # Integrate forward with runge-kutta for 3 seconds:
        dt = 0.002
        for _ in range(0, 1500):
            x = rk4(
                x=x,
                h=dt,
                f=lambda x: np_func(params=params, x=x, compute_energy=False, compute_J_x=False)[
                    "x_dot"],
            )

            energy_integrated = np_func(
                params=params, x=x, compute_energy=True, compute_J_x=False)["energy"]

            # We have no damping sources, so check that energy is conserved.
            np.testing.assert_allclose(
                desired=energy_initial, actual=energy_integrated, rtol=1.0e-5)

    def test_jacobian(self):
        """
        Generate the cart-double-pole dynamics as a JAX function, and check our Jacobians against
        `jacfwd`.
        """
        func, _ = code_generation.generate_python(
            func=get_cart_double_pole_dynamics(),
            target=code_generation.PythonGeneratorTarget.JAX,
            context={
                "CartPoleParamsNumeric": CartPoleParamsNumeric
            },
            generator_type=CustomPythonGenerator,
        )

        params = CartPoleParamsNumeric(m_b=1.1, m_1=0.23, m_2=0.27, l_1=0.4, l_2=0.22, g=9.81)
        func_jit = jax.jit(
            lambda x: func(params=params, x=x, compute_energy=False, compute_J_x=True))

        # Integrate for a couple of seconds and test the Jacobian at each step:
        x = np.array([0.05, np.pi / 2, -np.pi / 3, 0.0, 0.0, 0.0]).reshape(-1, 1)
        dt = 0.01
        for _ in range(0, 200):
            x = rk4(
                x=x,
                h=dt,
                f=lambda x: func_jit(x)["x_dot"],
            )

            (D_jax,) = jax.jacfwd(lambda x: func_jit(x)["x_dot"], argnums=[0])(x)
            D_sym = func_jit(x)["J_x"]

            # Tolerance is not amazing here, the use of float32 bites us a bit.
            np.testing.assert_allclose(desired=jnp.squeeze(D_jax), actual=D_sym, rtol=2.0e-3)


if __name__ == "__main__":
    jax.config.update('jax_platform_name', 'cpu')
    unittest.main()
