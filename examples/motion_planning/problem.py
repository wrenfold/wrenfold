import argparse
import typing
from dataclasses import dataclass

import wrenfold as wf
from wrenfold import sym, type_annotations, type_info

# Modify to change the planning horizon and time discretization of problem
# A larger N will result in a more accurate trajectory, but will take longer to solve
# Remember to keep consistent with `problem.rs`
# NOTE: Increasing N will slow down the code generation and rust compilation due to generating
# an output hessian that scales quadratically in size.
HORIZON = 4.0
DT = 0.25
N = int(HORIZON / DT)


# A discrete point along our trajectory
# Corresponds to a flat `State` struct in `problem.rs`
# pub struct State {
#     pub position: na::Vector3<f64>,
#     pub velocity: na::Vector3<f64>,
#     pub acceleration: na::Vector3<f64>,
#     pub control: na::Vector3<f64>,
# }
STATE_DIM = 12
State = typing.Annotated[sym.MatrixExpr, type_annotations.Shape(STATE_DIM, 1)]


# A trajectory is a sequence of states
# TODO: When wrenfold supports sequences of custom types,
# a trajectory can be a sequence of `State`s instead of a matrix
Trajectory = typing.Annotated[sym.MatrixExpr, type_annotations.Shape(STATE_DIM * N, 1)]


@dataclass
class Weights:
    """
    Weights for the cost function.
    Associated with the `Weights` struct in `problem.rs`
    """

    final_state: wf.FloatScalar
    dynamics: wf.FloatScalar


@dataclass
class ProblemInfo:
    """
    Information about the problem.
    Associated with the `ProblemInfo` struct in `problem.rs`
    """

    dt: wf.FloatScalar
    mass: wf.FloatScalar


def get_pos(state: State) -> wf.Vector3:
    return state[0:3, :]


def get_vel(state: State) -> wf.Vector3:
    return state[3:6, :]


def get_acc(state: State) -> wf.Vector3:
    return state[6:9, :]


def get_control(state: State) -> wf.Vector3:
    return state[9:12, :]


def dynamics(state: State, mass: wf.FloatScalar, dt: wf.FloatScalar) -> State:
    """
    Given a state, compute the next state after dt seconds
    """
    pos = get_pos(state)
    vel = get_vel(state)
    acc = get_acc(state)
    control = get_control(state)

    return sym.vstack(
        [
            pos + vel * dt + acc * sym.float_constant(0.5) * dt**2,
            vel + acc * dt,
            control * (1.0 / mass),
            control,
        ]
    )


def cost_jacobian(cost: sym.Expr, state: State):
    return sym.vector(cost).jacobian(state).T


def cost_hessian(cost: sym.Expr, state: State):
    return cost_jacobian(cost, state).jacobian(state)


def desired_state_objective(state: State, desired_state: State, weight: wf.FloatScalar):
    """
    Penalize the distance between the current state and the desired state
    """
    cost = weight * (desired_state - state).squared_norm()
    return cost


def dynamics_objective(
    state: State,
    next_state: State,
    weight: wf.FloatScalar,
    mass: wf.FloatScalar,
    dt: wf.FloatScalar,
):
    """
    Penalize the distance between the next state and the state predicted by the dynamics
    """
    cost = weight * (next_state - dynamics(state, mass, dt)).squared_norm()
    return cost


def problem(
    trajectory: Trajectory,
    desired_final_state: State,
    weights: Weights,
    problem_info: ProblemInfo,
) -> sym.Expr:
    """
    Fixed size optimization problem for trajectory optimization
    """
    cost = sym.float_constant(0.0)
    for i in range(0, N):
        index_offset = i * STATE_DIM
        index_next_offset = index_offset + STATE_DIM
        state = trajectory[index_offset:index_next_offset, :]

        # Final state cost
        cost += desired_state_objective(state, desired_final_state, weights.final_state * i**2)

        # Dynamics cost
        if i < N - 1:
            index_next_next_offset = index_next_offset + STATE_DIM
            next_state = trajectory[index_next_offset:index_next_next_offset, :]
            cost += dynamics_objective(
                state, next_state, weights.dynamics, problem_info.mass, problem_info.dt
            )

    return cost


def problem_cost(
    trajectory: Trajectory,
    desired_final_state: State,
    weights: Weights,
    problem_info: ProblemInfo,
):
    """
    Cost function to generate with wrenfold.
    """
    cost = problem(
        trajectory,
        desired_final_state,
        weights,
        problem_info,
    )
    return [
        wf.OutputArg(
            expression=cost,
            name="cost",
        )
    ]


def problem_jacobian(
    trajectory: Trajectory,
    desired_final_state: State,
    weights: Weights,
    problem_info: ProblemInfo,
):
    """
    Cost function jacobian to generate with wrenfold.
    """
    cost = problem(
        trajectory,
        desired_final_state,
        weights,
        problem_info,
    )
    # We don't want to optimize the initial state's pos/vel/acc
    # so we skip them when taking the jacobian
    trajectory_to_optimize = trajectory[9:]
    jacobian = cost_jacobian(cost, trajectory_to_optimize)
    return [
        wf.OutputArg(
            expression=jacobian,
            name="cost_jacobian",
        )
    ]


def problem_hessian(
    trajectory: Trajectory,
    desired_final_state: State,
    weights: Weights,
    problem_info: ProblemInfo,
):
    """
    Cost function hessian to generate with wrenfold.
    """
    cost = problem(
        trajectory,
        desired_final_state,
        weights,
        problem_info,
    )
    # We don't want to optimize the initial state's pos/vel/acc
    # so we skip them when taking the hessian
    trajectory_to_optimize = trajectory[9:]
    hessian = cost_hessian(cost, trajectory_to_optimize)
    return [
        wf.OutputArg(
            expression=hessian,
            name="cost_hessian",
        )
    ]


class CustomRustGenerator(wf.RustGenerator):
    def format_custom_type(self, element: type_info.CustomType) -> str:
        """Place our custom types into the `crate` namespace."""
        if element.python_type in [Weights, ProblemInfo]:
            return f"crate::{element.name}"
        return self.super_format(element)


def main(args: argparse.Namespace):
    code = ""
    for function in [problem_cost, problem_jacobian, problem_hessian]:
        code += wf.generate_function(func=function, generator=CustomRustGenerator())
        code += "\n\n"
    code = CustomRustGenerator.apply_preamble(code)
    wf.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
