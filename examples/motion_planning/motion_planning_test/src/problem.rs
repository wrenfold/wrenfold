use argmin::core::{CostFunction, Error, Gradient, Hessian};
use nalgebra as na;
use ndarray::{Array1, Array2};

use crate::gen;

/// Weights for the cost functions
#[derive(Clone)]
pub struct Weights {
    // Weight for the final state cost
    pub final_state: f64,
    // Weight for the dynamics cost
    pub dynamics: f64,
}

#[derive(Clone)]
pub struct ProblemInfo {
    pub dt: f64,
    pub mass: f64,
}

// Modify to change the planning horizon and time discretization of problem
// A larger N will result in a more accurate trajectory, but will take longer to solve
// Remember to keep consistent with `problem.py`
pub const PROBLEM_INFO: ProblemInfo = ProblemInfo {
    dt: 0.25,
    mass: 1.0,
};
pub const HORIZON: f64 = 4.0;
pub const N: usize = (HORIZON / PROBLEM_INFO.dt) as usize;

pub const STATE_DIM: usize = 12;
pub const POS_VEL_ACC_DIM: usize = 9;
pub const TRAJECTORY_DIM: usize = STATE_DIM * N;
// We don't want to optimize the initial state, so we subtract its size
// from the total trajectory size
pub const TRAJECTORY_TO_OPT_DIM: usize = TRAJECTORY_DIM - POS_VEL_ACC_DIM;

pub const MASS: f64 = 1.0;

/// A discrete point along our trajectory
#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct State {
    pub position: na::Vector3<f64>,
    pub velocity: na::Vector3<f64>,
    pub acceleration: na::Vector3<f64>,
    pub control: na::Vector3<f64>,
}

impl State {
    pub fn new(
        position: na::Vector3<f64>,
        velocity: na::Vector3<f64>,
        acceleration: na::Vector3<f64>,
        control: na::Vector3<f64>,
    ) -> Self {
        Self {
            position,
            velocity,
            acceleration,
            control,
        }
    }

    /// Get a flat svector of the state to pass into the generated residuals
    pub fn to_flat(&self) -> na::SVector<f64, STATE_DIM> {
        let mut flat = na::SVector::<f64, STATE_DIM>::zeros();
        flat.fixed_rows_mut::<3>(0).copy_from(&self.position);
        flat.fixed_rows_mut::<3>(3).copy_from(&self.velocity);
        flat.fixed_rows_mut::<3>(6).copy_from(&self.acceleration);
        flat.fixed_rows_mut::<3>(9).copy_from(&self.control);
        flat
    }

    /// Get a state from a flat svector of the state
    pub fn from_flat(flat: &na::SVector<f64, STATE_DIM>) -> Self {
        Self {
            position: flat.fixed_rows::<3>(0).clone_owned(),
            velocity: flat.fixed_rows::<3>(3).clone_owned(),
            acceleration: flat.fixed_rows::<3>(6).clone_owned(),
            control: flat.fixed_rows::<3>(9).clone_owned(),
        }
    }
}

/// The planning problem.
/// We implement the `CostFunction`, `Gradient`, and `Hessian` argmin traits for this struct,
/// where each of the trait functions use a fast generated function from wrenfold.
/// We optimize the trajectory by minimizing the cost function using a trust region method.
#[derive(Clone)]
pub struct Problem {
    // The desired final state of the trajectory
    pub desired_final_state: State,
    // The weights for the cost function
    pub weights: Weights,
}

impl Problem {
    pub fn new(desired_final_state: State, weights: Weights) -> Self {
        Self {
            desired_final_state,
            weights,
        }
    }

    /// Get a flat state from the optimization problem params Array1 at index i
    fn get_state(&self, p: &Array1<f64>, i: usize) -> na::SVector<f64, STATE_DIM> {
        let p = p.as_slice().unwrap();
        let mut state = na::SVector::<f64, STATE_DIM>::zeros();
        let index_offset = i * STATE_DIM;
        state
            .fixed_rows_mut::<3>(0)
            .copy_from_slice(&p[index_offset..index_offset + 3]);
        state
            .fixed_rows_mut::<3>(3)
            .copy_from_slice(&p[index_offset + 3..index_offset + 6]);
        state
            .fixed_rows_mut::<3>(6)
            .copy_from_slice(&p[index_offset + 6..index_offset + 9]);
        state
            .fixed_rows_mut::<3>(9)
            .copy_from_slice(&p[index_offset + 9..index_offset + 12]);
        state
    }

    /// Get a flat trajectory from the optimization problem params Array1
    fn get_trajectory(&self, p: &Array1<f64>) -> na::SVector<f64, TRAJECTORY_DIM> {
        let mut trajectory = na::SVector::<f64, TRAJECTORY_DIM>::zeros();
        for i in 0..N {
            let state = self.get_state(p, i);
            trajectory
                .fixed_rows_mut::<STATE_DIM>(i * STATE_DIM)
                .copy_from(&state.to_owned());
        }
        trajectory
    }
}

impl CostFunction for Problem {
    type Param = Array1<f64>;
    type Output = f64;

    fn cost(&self, p: &Self::Param) -> Result<Self::Output, Error> {
        let mut cost = 0.0;

        let trajectory = self.get_trajectory(p);
        gen::problem_cost(
            &trajectory,
            &self.desired_final_state.to_flat(),
            &self.weights,
            &PROBLEM_INFO,
            &mut cost,
        );
        Ok(cost)
    }
}

impl Gradient for Problem {
    type Param = Array1<f64>;
    type Gradient = Array1<f64>;

    fn gradient(&self, p: &Self::Param) -> Result<Self::Gradient, Error> {
        let mut gradient = na::SVector::<f64, TRAJECTORY_TO_OPT_DIM>::zeros();
        let trajectory = self.get_trajectory(p);
        gen::problem_jacobian(
            &trajectory,
            &self.desired_final_state.to_flat(),
            &self.weights,
            &PROBLEM_INFO,
            &mut gradient,
        );

        let mut gradient_to_use = na::SVector::<f64, TRAJECTORY_DIM>::zeros();
        gradient_to_use
            .fixed_rows_mut::<TRAJECTORY_TO_OPT_DIM>(POS_VEL_ACC_DIM)
            .copy_from(&gradient);
        Ok(Array1::from_vec(gradient_to_use.as_slice().to_vec()))
    }
}

impl Hessian for Problem {
    type Param = Array1<f64>;
    type Hessian = Array2<f64>;

    fn hessian(&self, p: &Self::Param) -> Result<Self::Hessian, Error> {
        let mut hessian = na::SMatrix::<f64, TRAJECTORY_TO_OPT_DIM, TRAJECTORY_TO_OPT_DIM>::zeros();
        let trajectory = self.get_trajectory(p);
        gen::problem_hessian(
            &trajectory,
            &self.desired_final_state.to_flat(),
            &self.weights,
            &PROBLEM_INFO,
            &mut hessian,
        );

        let mut hessian_to_use = na::SMatrix::<f64, TRAJECTORY_DIM, TRAJECTORY_DIM>::zeros();
        hessian_to_use
            .view_mut(
                (POS_VEL_ACC_DIM, POS_VEL_ACC_DIM),
                (TRAJECTORY_TO_OPT_DIM, TRAJECTORY_TO_OPT_DIM),
            )
            .copy_from(&hessian);

        Ok(
            Array2::from_shape_vec(hessian_to_use.shape(), hessian_to_use.as_slice().to_vec())
                .unwrap(),
        )
    }
}
