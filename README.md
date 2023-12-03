
# wrenfold

`wrenfold` is a framework for converting mathematical expressions (written in python) into generated code in compiled languages (C++, Rust). It is inspired by frameworks like [symforce](https://github.com/symforce-org/symforce), while aiming to support a wider variety of expressions and delivering improved code-generation execution times.

## Cloning and building

Clone the repository the submodules in the dependencies directory:
```bash
git clone https://github.com/gareth-cross/wrenfold.git
cd wrenfold
git submodule update --init --recursive
```

### Setting up a development environment

The following tools are required to build from source:
- cmake >= 3.19
- ninja (other build systems are not explicitly tested at this time)
- python >= 3.8
- mypy (required for [stubgen](https://mypy.readthedocs.io/en/stable/stubgen.html))

Additionally, to build and run tests you will need:
- numpy
- The rust compiler toolchain (`cargo` and `rustc`)

The following command will configure a `conda` environment suitable for building:
```bash
conda create -n wf_test python=3.8 cmake ninja mypy numpy
conda activate wf_test
```

### Building for development

The following instructions are suitable for building the project when you intend to actively iterate on wrenfold. When building on Windows, make sure you are running commands from the [Visual Studio Command Prompt](https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022), or in a shell with MSVC on the path.

```bash
cd <path to wrenfold repo>
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -Wno-deprecated -G Ninja
cmake --build . --config RelWithDebInfo -j12
```

Tests can then be run from the build directory by executing `ctest`. If you would like to manually invoke a single python test, configure the python path as follows:
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
- Windows 11 with MSVC 2022 version `14.37.32822`
- OSX Ventura (arm64) with Apple clang 15.0.0
- Ubuntu 20.04 with gcc 9.4

## Roadmap

Known issues and limitations:
- Input and output types of generated functions must be floating point values. This limitation is actually pretty straightforward to resolve, but some plumbing of type information is required.
- Numeric integer operations are unsafe. Integer overflow will silently occur if you multiply sufficiently large values.
- There is no mechanism for including docstrings on generated methods yet.
- `MatrixExpr` objects are read-only. You can access `[row, col]` elements and slices, but not write to them.
- The `subs` operation is relatively slow at the moment, compared to other operations. There is some low hanging fruit here that can be leveraged to improve this.
- Relationals like `<` or `==` can be used to create boolean expressions, but we currently lack logical boolean operators `&&` and `||`.
- The type of expressions has limited visibility in python. You can inspect the `type_name` field to access the underlying type as a string, but not much more than that.
- Printing methods to strings (via `repr` for example) tends to produce a wall of impenetrable text, particularly for matrix types. Ideally these strings would be truncated automatically.

There are also some high-priority features that I would like to get done fairly soon:
- Conversion of expressions to and from sympy.
- Customization of code-generation from python. Right now code is emitted in C++, but it would be convenient to be able to customize formatting of individual AST elements.
- Add boolean logic operators.
- Investigate automatic vectorization of floating point operations.
