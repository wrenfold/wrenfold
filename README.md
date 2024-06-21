# wrenfold

<!--- logo_start --->
<p align="center">
<img src="./docs/source/_static/logo.png" alt="wrenfold logo depicting a bird made from folded paper" width="25%"/>
</p>
<!--- logo_end --->

<!--- badges_start --->
<p align="center">
<a href="https://github.com/wrenfold/wrenfold/actions/workflows/ci.yml?query=branch%3Amain"><img alt="GitHub Actions Workflow Status" src="https://github.com/wrenfold/wrenfold/actions/workflows/ci.yml/badge.svg?branch=main"></a>
<img alt="C++17" src="https://img.shields.io/badge/c++-17-blue" />
<a href="https://opensource.org/licenses/MIT">
  <img src="https://img.shields.io/badge/License-MIT-blue.svg">
</a>
</p>
<!--- badges_end --->

---

<!--- intro_start --->
`wrenfold` is a framework for converting symbolic mathematical expressions (written in python) into generated code in compiled languages (C++, Rust). It aims to bridge the gap between prototyping of functions in expressive symbolic form, and performant production code. wrenfold is particularly relevant to domains where numerical optimization is employed to solve differentiable objective functions, such as robotics or computer vision.

Using wrenfold, mathematical functions can be expressed and composed succinctly in python:

```python
from wrenfold import sym
from wrenfold.type_annotations import Vector3
from wrenfold import code_generation

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

And corresponding compilable code can be quickly obtained:

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

* **Improved flexibility**: Symbolic expressions can include conditional logic.
* **Ease of integration**: wrenfold aims to make it straightforward to customize the code-generation step to suit your project. For example, you can [use existing types in your codebase in generated method signatures](https://wrenfold.org/tutorial/custom_types.html).
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
  - The performance of generated methods is often competitive with handwritten implementations, and can meaningfully exceed results obtained with runtime auto-diff.
  - **A prudent caveat for any performance related claim**: Your mileage may vary depending on expression complexity and the degree of effort exerted in optimizing different implementations.

<!--- motivation_end --->

## Installation

<!--- github_wheel_install_start --->
Prior to public release, pre-built python wheels may be obtained from the [GitHub Releases Page](https://github.com/wrenfold/wrenfold/releases). Select the `whl` file appropriate to your OS and python version. For example, for python 3.10 on arm64 OSX you would download and install `wrenfold-0.0.3-cp310-cp310-macosx_11_0_arm64.whl`:

```
pip install wrenfold-0.0.3-cp310-cp310-macosx_11_0_arm64.whl
```

Then test that wrenfold can be imported:
```python
>>> from wrenfold import sym
>>> x, y = sym.symbols('x, y')
>>> f = sym.cos(x * y)
>>> g = f.diff(x)
>>> g
-y * sin(x * y)
```

<!--- github_wheel_install_end --->

## Getting started

To get started:
- Refer to the tutorial material on the [website](https://wrenfold.org/installation.html).
- Take a look at some of the examples:
  - There are some examples of symbolic expressions [in this repository](examples). For instance, an implementation of [imu integration](examples/imu_integration/imu_integration.py).
  - Each example includes a unit test that invokes the generated code.
- The [wrenfold-extra-examples](https://github.com/wrenfold/wrenfold-extra-examples) repository includes examples of integrating generated code into existing optimizers like [GTSAM](http://gtsam.org) and [Ceres](http://ceres-solver.org).

## Project status

wrenfold was originally created by [me](https://github.com/gareth-cross) in response to pain points I experienced while integrating SymForce into a third-party codebase (see motivations section). In addition to addressing these concerns, I feel that the concept of symbolic code-generation can be pushed further over time.

The project began as a part-time hobby, and has evolved into a more full-featured framework over time. Having only recently been released publicly, wrenfold is in the early stages of receiving feedback from a wider audience. There will be rough edges and undoubtedly some bugs. If you find something broken or missing, please consider [filing a ticket](https://github.com/wrenfold/wrenfold/issues/new/choose). I aim to continue developing and expanding the framework. For details of upcoming work, see the [planned features list](https://github.com/wrenfold/wrenfold/issues?q=is%3Aissue+is%3Aopen+label%3Afeature).

If you are interested in collaboration opportunities or have general questions, please [reach out](mailto:gcross.code@icloud.com?subject=Wrenfold).

## Building and installing from source

<!--- source_build_start --->
First, clone the repository and the submodules in the dependencies directory:
```bash
git clone https://github.com/wrenfold/wrenfold.git
cd wrenfold
git submodule update --init --recursive
```

### Building via pip

If you wish to build and install **only** the wrenfold library itself (skipping tests and examples), the recommended path is to use pip. wrenfold uses [scikit-build-core](https://scikit-build-core.readthedocs.io/en/latest/) for python packaging.

Activate your python virtual environment and execute the following in the repo root:
```bash
pip install . --verbose
```

### Building with cmake

Building directly with cmake is the recommend path if you need to iterate on the source code.

The following tools are required to build from source:
- cmake >= 3.20
- ninja >= 1.5 (other build systems are not explicitly tested at this time)
- python >= 3.8
- mypy (required for [stubgen](https://mypy.readthedocs.io/en/stable/stubgen.html))

Additionally, to build and run tests you will need:
- numpy
- SymPy
- The rust compiler toolchain (`cargo` and `rustc`).
- On linux and windows: `pkg-config` and `openblas`.

To build documentation:
- [sphinx](https://www.sphinx-doc.org/)
- [furo](https://github.com/pradyunsg/furo)
- [doxygen](https://www.doxygen.nl)
- [breathe](https://breathe.readthedocs.io/en/latest/)
- [myst](https://myst-parser.readthedocs.io/)

The following command will configure a `conda` environment suitable for building and testing:
```bash
conda create -n wf
conda env update -n wf --file environment.yml
conda env update -n wf --file <PLATFORM>-environment.yml
conda activate wf
```
Where `<PLATFORM>` is one of `mac`, `linux`, or `windows`. The rust tools must be installed separately with [rustup](https://rustup.rs).

#### Compilation

When building on Windows, make sure you are executing commands from the [Visual Studio Command Prompt](https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022), *or* in a shell with MSVC on the path.

To configure with cmake and build the library + all tests and examples, run:
```bash
cd <path to wrenfold repo>
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -Wno-deprecated -G Ninja
cmake --build .
```

The C++ and python tests are executed via `ctest`. Rust tests must be compiled and run separately after the cmake build step:
```bash
cargo test --tests --release
```
Cargo does not presently invoke `cmake --build` if code generators are stale. To force rust code to be re-generated, run `cmake --build --target wf_rust_generation`.

#### Configuring the python path for development

If you would like to iterate on python examples or tests, you will need to configure the python path to point to the wrenfold repository:
```bash
export REPO_ROOT=$(pwd)
export PYTHONPATH="$REPO_ROOT/components/python:$REPO_ROOT/build/components/wrapper"
```
Or, for PowerShell:
```pwsh
$env:REPO_ROOT = (Get-Location).path
$env:PYTHONPATH = "$env:REPO_ROOT\components\python;$env:REPO_ROOT\build\components\wrapper"
```
<!--- source_build_end --->
