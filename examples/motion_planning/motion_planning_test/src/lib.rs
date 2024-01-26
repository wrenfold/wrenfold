pub mod generated;
pub mod problem;

pub use problem::{ProblemInfo, Weights};

#[cfg(test)]
mod tests {
    use crate::problem;
    use argmin::core::{Executor, State};
    use argmin::solver::trustregion::{Steihaug, TrustRegion};
    use nalgebra as na;
    use ndarray::{s, Array1, Array2};

    fn run_problem(
        desired_final_state: problem::State,
    ) -> argmin::core::OptimizationResult<
        problem::Problem,
        TrustRegion<Steihaug<Array1<f64>, f64>, f64>,
        argmin::core::IterState<Array1<f64>, Array1<f64>, (), Array2<f64>, f64>,
    > {
        let initial_state = problem::State::new(
            na::Vector3::new(0.0, 0.0, 0.0),
            na::Vector3::new(0.0, 0.0, 0.0),
            na::Vector3::new(0.0, 0.0, 0.0),
            na::Vector3::new(0.0, 0.0, 0.0),
        );

        let problem = problem::Problem::new(
            desired_final_state,
            problem::Weights {
                final_state: 1.0,
                dynamics: 100.0,
            },
        );

        let init_trajectory: Vec<f64> = (0..problem::N)
            .map(|i| {
                // Lerp state from initial to final as initial guess
                let t = i as f64 / (problem::N - 1) as f64;
                let pos = initial_state
                    .position
                    .lerp(&problem.desired_final_state.position, t);
                let vel = initial_state
                    .velocity
                    .lerp(&problem.desired_final_state.velocity, t);
                let acc = initial_state
                    .acceleration
                    .lerp(&problem.desired_final_state.acceleration, t);
                let control = initial_state
                    .control
                    .lerp(&problem.desired_final_state.control, t);
                let state = problem::State::new(pos, vel, acc, control);
                state.to_flat().as_slice().to_vec()
            })
            .flatten()
            .collect();
        let init_hessian: Array2<f64> = Array2::eye(problem::STATE_DIM * problem::N);

        let subproblem = Steihaug::new();
        let solver = TrustRegion::new(subproblem);

        // Run solver
        let res = Executor::new(problem.clone(), solver)
            .configure(|state| {
                state
                    .param(Array1::from_vec(init_trajectory))
                    .hessian(init_hessian)
                    .max_iters(50)
            })
            .run()
            .unwrap();

        res
    }

    #[test]
    fn test_planning_problem() {
        // Run the planning problem
        let desired_final_state = problem::State::new(
            na::Vector3::new(1.0, 1.0, 5.0),
            na::Vector3::new(0.0, 0.0, 0.0),
            na::Vector3::new(0.0, 0.0, 0.0),
            na::Vector3::new(0.0, 0.0, 0.0),
        );
        let res = run_problem(desired_final_state.clone());

        // Check that we optimize something to the desired final state
        let best = res.state().get_best_param().unwrap().to_owned();
        let last_index = problem::N - 1;
        let slice = best
            .slice(s![
                last_index * problem::STATE_DIM..(last_index + 1) * problem::STATE_DIM
            ])
            .to_owned();
        let state_flat = slice.as_slice().unwrap();
        let state_flat = na::SVector::<f64, 12>::from_vec(state_flat.to_vec());
        let last_state = problem::State::from_flat(&state_flat);
        assert!(
            last_state
                .position
                .metric_distance(&desired_final_state.position)
                < 1e-3
        );
        assert!(
            last_state
                .velocity
                .metric_distance(&desired_final_state.velocity)
                < 1e-3
        );
        assert!(
            last_state
                .acceleration
                .metric_distance(&desired_final_state.acceleration)
                < 1e-3
        );
    }
}
