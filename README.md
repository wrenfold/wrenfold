
# wrenfold

`wrenfold` is a framework for converting symbolic mathematical expressions (written in python) into generated code in compiled languages (C++, Rust). It is inspired by frameworks like [symforce](https://github.com/symforce-org/symforce), while aiming to support a wider variety of expressions and delivering improved code-generation execution times. wrenfold is written in C++, and exposes a python API via pybind11.

## Cloning and building

Clone the repository and the submodules in the dependencies directory:
```bash
git clone https://github.com/gareth-cross/wrenfold.git
cd wrenfold
git submodule update --init --recursive
```

### Setting up a development environment

The following tools are required to build from source:
- cmake >= 3.20
- ninja (other build systems are not explicitly tested at this time)
- python >= 3.8
- mypy (required for [stubgen](https://mypy.readthedocs.io/en/stable/stubgen.html))

Additionally, to build and run tests you will need:
- numpy
- [sympy](https://www.sympy.org/)
- The rust compiler toolchain (`cargo` and `rustc`): [Installation](https://rustup.rs) link.
- On linux+windows: `pkg-config` and `openblas`.

To build documentation:
- [sphinx](https://www.sphinx-doc.org/)
- [furo](https://github.com/pradyunsg/furo)

The following command will configure a `conda` environment suitable for building+testing:
```bash
conda create -n wf python=3.8 cmake ninja mypy numpy sympy pkg-config openblas
conda activate wf
```

On linux, I recommend adding the `compilers` package as well:
```
conda install compilers=1.7.0
```

### Building for development

The following instructions are suitable for building the project when you intend to actively iterate on wrenfold. When building on Windows, make sure you are running commands from the [Visual Studio Command Prompt](https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022), or in a shell with MSVC on the path.

```bash
cd <path to wrenfold repo>
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -Wno-deprecated -G Ninja
cmake --build .
```

This will build the wrenfold library and C++ tests. The C++ and python tests are executed via `ctest`. Rust tests must be compiled and run separately after the cmake build step:
```bash
cargo test --tests --release
```
Cargo does not presently invoke `cmake --build` if code-generators are stale. To force rust code to be re-generated, run `cmake --build --target wf_rust_generation`.

### Configuring the python path for development

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

### Building via pip

The following steps are suitable when building and installing wrenfold as a pip package. wrenfold uses [scikit-build-core](https://scikit-build-core.readthedocs.io/en/latest/) for python packaging.

Activate your virtual environment, and execute the following from the repo root:
```bash
pip install . --verbose
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

### Tested configurations

So far I have tested the following build environments:
- Windows 11 with MSVC 2022 version `19.36`
- OSX Ventura (arm64) with Apple clang 15.0.0
- Ubuntu 20.04/22.04 with gcc 12.3

## Roadmap

There is a brief short-term roadmap on [the wiki](https://github.com/gareth-cross/wrenfold/wiki/Short-term-roadmap).

Known issues and limitations:
- Input and output types of generated functions must be floating point values. This limitation is actually pretty straightforward to resolve, but some plumbing of type information is required.
- There is no mechanism for including docstrings on generated methods yet.
- The `subs` operation is relatively slow at the moment, compared to other operations. There is some low hanging fruit here that can be leveraged to improve this.
- Relationals like `<` or `==` can be used to create boolean expressions, but we currently lack logical boolean operators `&&` and `||`.
- The type of expressions has limited visibility in python. You can inspect the `type_name` field to access the underlying type as a string, but not much more than that.
- Printing methods to strings (via `repr` for example) tends to produce a wall of impenetrable text, particularly for matrix types. Ideally these strings would be truncated automatically.
- Conversion from Eigen to span does not type-check adequately. You can pass an integer matrix to an argument that expects floating point.
- Certain built-in functions are missing, such as `ceil`, `round`, and `mod`.
