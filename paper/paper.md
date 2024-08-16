---
title: 'wrenfold: Symbolic code generation for robotics'
tags:
  - Python
  - symbolic math
  - robotics
  - computer vision
  - code generation
authors:
  - name: Gareth Cross
    orcid: 0009-0008-3110-0078
date: 13 August 2024
bibliography: paper.bib
---

# Summary

Real-time robotic software systems often solve one or more numerical optimization problems. For example, accurate estimates of past vehicle motion are typically obtained as the solution of a non-linear optimization or filtering problem [@Barfoot:2024]. Similarly, the behavior of an autonomous system can be determined via a numerical optimization problem that reasons about the relative merits of different future actions [@Lynch_Park:2021].

Problems of this form can be solved using packages like Google Ceres [@Agarwal_Ceres_Solver_2022:2023] or GTSAM [@gtsam:2022]. These optimizers require that the user provide a mathematical objective function and - in some instances - the derivatives of said function with respect to the desired decision variables. In order to achieve real-time deadlines, the optimization is usually implemented in a performant compiled language such as C++.

`wrenfold` is a framework that converts symbolic math expressions (written in Python) into generated code in compiled languages (C++, Rust). The primary goals of the framework are:

* Bridge the gap between expressive prototyping of objective functions in symbolic form, and the performant code required for real-time operation.
* Improve on existing symbolic code generation solutions by supporting a greater variety and complexity of expressions.

# Statement of need

Researchers and engineers working in robotics and related domains (eg. motion planning, control theory, state estimation, computer vision) regularly implement numerical optimization problems. This process presents a number of challenges:

* Robotic systems often feature non-trivial kinematics and dynamics. Describing the motion or sensor models may involve reasoning about complex chains of 3D transformations.
* Many popular optimization methods require derivatives for the objective function. Manually computing derivatives is tedious and error-prone, particularly in the presence of complicated geometry or compounded transformations.
* Performant systems languages like C++ do not necessarily have syntax conducive to the elegant expression of elaborate mathematical functions.

Symbolic code generation can help address these issues:

* Symbolic functions can be be easily composed in Python and reasonably performant [@wrenfold_perf] C++ implementations are obtained automatically. The developer time cost required to experiment with different optimization parameterizations is thereby reduced.
* Correct derivatives require no additional work - they can be obtained directly from the objective function via symbolic differentiation. Common terms that appear in the objective function and its Jacobians are automatically de-duplicated by the code generation step.

The application of symbolic code generation to robotics is not novel. For example, the `MATLAB` symbolic code generation toolbox has been applied directly to motion planning [@Hereid2017FROST:2017]. The `SymForce` framework [@Martiros-RSS-22:2022] couples the `SymEngine` [@symengine] mathematical backend with Python code generation utilities and mathematical primitives specific to robotics. `wrenfold` draws inspiration from the design of `SymForce`, while aiming to support a greater variety and complexity of functions. As a comparison, we improve on the concept with the following contributions:

* **Symbolic functions may incorporate piecewise conditional statements** - these produce if-else logic in the resulting code. This enables a broader range of functions to be generated.
* **Emphasis is placed on ease of adaptability of the generated code.** Math expressions are simplified and converted into an abstract syntax tree (AST). Formatting of any element of the AST (such as function signatures and types) can be individually customized by defining a short Python method.
* **Code generation times are meaningfully improved**, thereby enabling more complex expressions.

# Resources

* GitHub repository: [https://github.com/wrenfold/wrenfold](https://github.com/wrenfold/wrenfold)
* Website: [https://wrenfold.org](https://wrenfold.org)

# References

# Acknowledgements
