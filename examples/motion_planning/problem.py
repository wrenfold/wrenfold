import argparse

from dataclasses import dataclass

from wrenfold import code_generation, sym
from wrenfold.code_generation import OutputArg
from wrenfold.type_annotations import RealScalar, Vector3

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
class State(sym.MatrixExpr):
    SHAPE = (12, 1)


# A trajectory is a sequence of states
# TODO: When wrenfold supports sequences of custom types,
# a trajectory can be a sequence of `State`s instead of a matrix
class Trajectory(sym.MatrixExpr):
    SHAPE = (12 * N, 1)


@dataclass
class Weights:
    """
    Weights for the cost function.
    Associated with the `Weights` struct in `problem.rs`
    """

    final_state: RealScalar
    dynamics: RealScalar


@dataclass
class ProblemInfo:
    """
    Information about the problem.
    Associated with the `ProblemInfo` struct in `problem.rs`
    """

    dt: RealScalar
    mass: RealScalar


def get_pos(state: State) -> Vector3:
    return state[0:3, :]


def get_vel(state: State) -> Vector3:
    return state[3:6, :]


def get_acc(state: State) -> Vector3:
    return state[6:9, :]


def get_control(state: State) -> Vector3:
    return state[9:12, :]


def dynamics(state: State, mass: RealScalar, dt: RealScalar) -> State:
    """
    Given a state, compute the next state after dt seconds
    """
    pos = get_pos(state)
    vel = get_vel(state)
    acc = get_acc(state)
    control = get_control(state)

    return sym.vstack([
        pos + vel * dt + acc * sym.float(0.5) * dt ** 2,
        vel + acc * dt,
        control * (1.0 / mass),
        control,
    ])


def cost_jacobian(cost: sym.Expr, state: State):
    return sym.vector(cost).jacobian(state).T


def cost_hessian(cost: sym.Expr, state: State):
    return cost_jacobian(cost, state).jacobian(state)


def desired_state_objective(state: State, desired_state: State, weight: RealScalar):
    """
    Penalize the distance between the current state and the desired state
    """
    cost = weight * (desired_state - state).squared_norm()
    return cost


def dynamics_objective(
    state: State,
    next_state: State,
    weight: RealScalar,
    mass: RealScalar,
    dt: RealScalar,
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
):
    """
    Fixed size optimization problem for trajectory optimization
    """
    cost = 0.0
    for i in range(0, N):
        index_offset = i * State.SHAPE[0]
        index_next_offset = index_offset + State.SHAPE[0]
        state = trajectory[index_offset:index_next_offset, :]

        # Final state cost
        cost += desired_state_objective(state, desired_final_state, weights.final_state * i ** 2)

        # Dynamics cost
        if i < N - 1:
            index_next_next_offset = index_next_offset + State.SHAPE[0]
            next_state = trajectory[index_next_offset:index_next_next_offset, :]
            cost += dynamics_objective(state, next_state, weights.dynamics, problem_info.mass,
                                       problem_info.dt)

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
    return [OutputArg(
        expression=cost,
        name="cost",
    )]


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
    return [OutputArg(
        expression=jacobian,
        name="cost_jacobian",
    )]


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
    return [OutputArg(
        expression=hessian,
        name="cost_hessian",
    )]


class CustomRustGenerator(code_generation.RustGenerator):

    def format_custom_type(self, element: code_generation.codegen.CustomType) -> str:
        """Place our custom types into the `crate` namespace."""
        if element.python_type in [Weights, ProblemInfo]:
            return f"crate::{element.name}"


def main(args: argparse.Namespace):
    descriptions = [
        code_generation.create_function_description(func=problem_cost),
        code_generation.create_function_description(func=problem_jacobian),
        code_generation.create_function_description(func=problem_hessian),
    ]
    definitions = code_generation.transpile(descriptions=descriptions)
    code = CustomRustGenerator().generate(definitions=definitions)
    code = code_generation.apply_rust_preamble(code)
    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == "__main__":
    main(parse_args())
