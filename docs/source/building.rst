Building from source
====================

The following guide is intended for developers looking to iterate on the wrenfold source code. The
recommended path for most users is to :doc:`install a wheel <quick_start>`.

Download the code
-----------------

First, clone the repository and the submodules in the dependencies directory:

.. code:: bash

    git clone https://github.com/wrenfold/wrenfold.git
    cd wrenfold
    git submodule update --init --recursive

Building via pip
----------------

There are two methods for building wrenfold from source. Building via pip is the easier method. This creates **only the wrenfold library itself** (skipping all tests and examples). wrenfold uses `scikit-build-core <https://scikit-build-core.readthedocs.io/en/latest/>`_ for python packaging.

With your python virtual environment active, execute the following from the repository root:

.. code:: bash

    pip install . --verbose

Building with cmake
-------------------

Requirements
^^^^^^^^^^^^

Building directly with cmake is the recommend path if you need to iterate on the source code.

You will need the following tools:

  * cmake >= 3.20
  * ninja >= 1.5 (other build systems are not explicitly tested at this time)
  * python >= 3.9
  * mypy (required for `stubgen <https://mypy.readthedocs.io/en/stable/stubgen.html>`_)

To run python tests you will additionally need:

  * numpy
  * SymPy

To build and run Rust tests you will need:

  * The rust compiler toolchain
  * On linux and windows: ``pkg-config`` and ``openblas``.

To build documentation:

  * `sphinx <https://www.sphinx-doc.org/>`_
  * `furo <https://github.com/pradyunsg/furo>`_
  * `doxygen <https://www.doxygen.nl>`_
  * `breathe <https://breathe.readthedocs.io/en/latest/>`_
  * `myst <https://myst-parser.readthedocs.io/>`_

The repo includes conda configuration files that can be used to create a conda environment suitable for building and testing:

.. code:: bash

    conda create -n wf
    conda env update -n wf --file environment.yml
    conda env update -n wf --file <PLATFORM>-environment.yml
    conda activate wf

Where ``<PLATFORM>`` is one of ``mac``, ``linux``, or ``windows``. The rust tools must be installed separately with `rustup <https://rustup.rs>`_.

Compilation
^^^^^^^^^^^

.. tip::

    When building on Windows, make sure you are executing commands from the `Visual Studio Command Prompt <https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022>`_, *or* in a shell with MSVC on the path.

To configure with cmake and build the library + all tests and examples, execute:

.. code:: bash

    cd <path to wrenfold repo>
    mkdir build
    cd build
    cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -Wno-deprecated -G Ninja
    cmake --build .

The C++ and python tests are executed via ``ctest``.

Running rust tests
^^^^^^^^^^^^^^^^^^

Rust tests must be compiled and run separately after the cmake build step:

.. code:: bash

    cargo test --tests --release

Cargo does not presently invoke ``cmake --build`` if code generators are stale. To force rust code to be re-generated, run ``cmake --build --target wf_rust_generation``.

Configuring the python path for development
-------------------------------------------

If you would like to iterate on python examples or tests, you will need to configure the python path to point to the wrenfold repository. In bash:

.. code:: bash

    export REPO_ROOT=$(pwd)
    export PYTHONPATH="$REPO_ROOT/components/python:$REPO_ROOT/build/components/wrapper"

Or, for PowerShell:

.. code:: PowerShell

    $env:REPO_ROOT = (Get-Location).path
    $env:PYTHONPATH = "$env:REPO_ROOT\components\python;$env:REPO_ROOT\build\components\wrapper"
