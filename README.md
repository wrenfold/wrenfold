# wrenfold

<!--- logo_start --->
<p align="center">
<img src="./docs/source/_static/logo.png" alt="wrenfold logo depicting a bird made from folded paper" width="25%"/>
</p>
<!--- logo_end --->

<!--- badges_start --->
<p align="center">
<a href="https://github.com/wrenfold/wrenfold/actions/workflows/ci.yml?query=branch%3Amain"><img alt="GitHub Actions Workflow Status" src="https://github.com/wrenfold/wrenfold/actions/workflows/ci.yml/badge.svg?branch=main"></a>
<a href="https://pypi.org/project/wrenfold/"><img alt="Python versions badge" src="https://img.shields.io/pypi/pyversions/wrenfold"/></a>
<a href="https://anaconda.org/conda-forge/wrenfold"><img alt="conda-forge version badge" src="https://anaconda.org/conda-forge/wrenfold/badges/version.svg"/></a>
<a href="https://crates.io/crates/wrenfold-traits"><img src="https://img.shields.io/crates/v/wrenfold-traits.svg" alt="crates.io"></a>
<img alt="C++17" src="https://img.shields.io/badge/c++-17-blue" />
<a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-blue.svg"></a>
</p>
<!--- badges_end --->

<p align="center">
<strong>
<a href="https://wrenfold.org">Documentation</a>
</strong>
</p>

---

<!--- intro_start --->
`wrenfold` is a framework for converting symbolic mathematical expressions (written in python) into generated code in compiled languages (C++, Rust). It aims to bridge the gap between prototyping of functions in expressive symbolic form, and performant production code. wrenfold is particularly relevant to domains where numerical optimization is employed to solve differentiable objective functions, such as robotics or computer vision.

Using wrenfold, mathematical functions can be expressed and composed succinctly in python:

```python
from wrenfold import code_generation, sym
from wrenfold.type_annotations import Vector3

def angular_distance(a: Vector3, b: Vector3):
    """
    A simple example function: We compute the angle between two vectors. The angle is returned, and
    the Jacobian with respect to `a` is passed as an output argument. This might be a cost in an
    optimization, for instance.
    """
    dot = (a.T * b)[0]
    cos_theta = dot / (a.norm() * b.norm())
    theta = sym.acos(cos_theta)
    theta_D_a = sym.jacobian([theta], a)

    # Our generated function will return `theta`, and pass `theta_D_a` as an output arg.
    return (
        code_generation.ReturnValue(theta),
        code_generation.OutputArg(theta_D_a, "theta_D_a"),
    )
```

And corresponding compilable code can be obtained easily:

```python
# CppGenerator can be swapped out for RustGenerator to obtain Rust. You can implement your own
# custom generator to target a new language - or override methods on the provided generators in
# order to customize the output code to your liking.
cpp = code_generation.generate_function(angular_distance, code_generation.CppGenerator())
print(cpp)
```
```cpp
template <typename Scalar, typename T0, typename T1, typename T2>
Scalar angular_distance(const T0& a, const T1& b, T2&& theta_D_a) {
  auto _a = wf::make_input_span<3, 1>(a);
  auto _b = wf::make_input_span<3, 1>(b);
  auto _theta_D_a = wf::make_output_span<1, 3>(theta_D_a);

  const Scalar v007 = _b(2, 0);
  const Scalar v006 = _a(2, 0);
  const Scalar v004 = _b(1, 0);

  // ... Output code is truncated for brevity.

  const Scalar v009 = v000 * v001 + v003 * v004 + v006 * v007;
  const Scalar v021 = v001 * v001 + v004 * v004 + v007 * v007;

  // ...

  _theta_D_a(0, 0) = (v000 * v072 + v001 * v017) * v073;
  _theta_D_a(0, 1) = (v003 * v072 + v004 * v017) * v073;
  _theta_D_a(0, 2) = (v006 * v072 + v007 * v017) * v073;
  return std::acos(v009 * v017 * v023);
}
```

wrenfold draws inspiration from [SymForce](https://symforce.org), but differs in a few key ways:

* **Improved flexibility**: Symbolic expressions can include conditional logic. This enables a broader range of functions to be generated.
* **Ease of integration**: wrenfold aims to make it straightforward to customize the code-generation step to suit your project. For example, you can [use existing types in your codebase in generated method signatures](https://wrenfold.org/reference/custom_types.html).
* **Faster code generation**: Faster code generation translates to quicker iteration on experiments. The generation cost should ideally be negligible compared to compile time for the code itself.
* **Narrower scope**: wrenfold does not implement a numerical optimizer. Rather we aim to make it simple to integrate generated code into your project's existing preferred optimizer (see the [extended examples](https://github.com/wrenfold/wrenfold-extra-examples)). It should be relatively straightforward to use wrenfold functions with GTSAM, Ceres, the SymForce optimizer, or your own custom implementation.

wrenfold is primarily written in C++, and exposes a python API via [pybind11](https://pybind11.readthedocs.io). It can presently generate code in C++17 and Rust.
<!--- intro_end --->

## Motivation

<!--- motivation_start --->
Why use symbolic code generation for mathematical functions? The [SymForce paper](https://arxiv.org/abs/2204.07889) outlines some of the rationale. In our opinion, the two main arguments are:

* **Faster iteration**:
  - Functions can be written quickly and expressively in python, enabling rapid prototyping. Over time, users acquire a library of composable expressions that can be combined easily to form new symbolic functions.
  - Derivatives are obtained automatically, without spending time debugging manually chain-ruled Jacobians.
* **Improved runtime performance**:
  - The performance of generated methods is [often competitive with handwritten implementations](https://wrenfold.org/performance.html), and can meaningfully exceed results obtained with runtime auto-diff.
  - Generated methods are fast enough to deploy on a production robot, enabling a quicker pipeline from offline prototyping to real-world testing.
  - **A prudent caveat for any performance related claim**: Your mileage may vary depending on expression complexity and the degree of effort exerted in optimizing different implementations.

<!--- motivation_end --->

## Getting started

Install wrenfold from [PyPi](https://pypi.org/project/wrenfold/):

```bash
pip install wrenfold
```

wrenfold is also available on [conda-forge](https://anaconda.org/conda-forge/wrenfold):

<a href="https://anaconda.org/conda-forge/wrenfold"><img alt="conda-forge platforms" src="https://anaconda.org/conda-forge/wrenfold/badges/platforms.svg"/></a>

```bash
conda install -c conda-forge wrenfold
```

To get started:
- Refer to the [quick start guide](https://wrenfold.org/quick_start.html) and [user guide](https://wrenfold.org/reference/index.html).
- Take a look at some of the examples:
  - There are some examples of symbolic expressions [in this repository](examples). For instance, an implementation of [imu integration](examples/imu_integration/imu_integration.py).
  - Each example includes a unit test that invokes the generated code.
- The [wrenfold-extra-examples](https://github.com/wrenfold/wrenfold-extra-examples) repository includes examples of integrating generated code into existing optimizers like [GTSAM](http://gtsam.org) and [Ceres](http://ceres-solver.org).

## Building from source

See [Building from source](https://wrenfold.org/building.html).

## Project status

wrenfold was originally created by [me](https://github.com/gareth-cross) after determining there were opportunities to push the symbolic code-generation concept further (see motivations section).

The project began as a part-time hobby for fun, and has evolved into a more full-featured framework over time. wrenfold is in the early stages of receiving feedback from a wider audience. There will be rough edges and undoubtedly some bugs. If you find something broken or missing, please consider [filing a ticket](https://github.com/wrenfold/wrenfold/issues/new/choose). I aim to continue developing and expanding the framework. For details of upcoming work, see the [planned features list](https://github.com/wrenfold/wrenfold/issues?q=is%3Aissue+is%3Aopen+label%3Afeature).

If you are interested in collaboration opportunities or have general questions, please [reach out](mailto:gcross.code@icloud.com?subject=Wrenfold).
