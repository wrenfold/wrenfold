Quick start guide
=================

Installing the python package
-----------------------------

Python wheels are available on `PyPi <https://pypi.org/project/wrenfold/>`_. Install the latest
version of wrenfold with:

.. code:: bash

   pip install wrenfold

**Alternatively**, python wheels may also be obtained from the
`GitHub Releases Page <https://github.com/wrenfold/wrenfold/releases>`_. Select the ``whl`` file
appropriate to your OS and python version. For example, for python 3.10 on arm64 OSX you would
download and install ``wrenfold-0.0.6-cp310-cp310-macosx_11_0_arm64.whl``:

.. code:: bash

   pip install wrenfold-0.0.6-cp310-cp310-macosx_11_0_arm64.whl

Then open a python REPL and test that wrenfold can be imported:

.. code:: python

   from wrenfold import sym

   x, y = sym.symbols('x, y')
   f = sym.cos(x * y)
   g = f.diff(x)
   print(g)  # prints: -y * sin(x * y)

Generating your first function
------------------------------

To illustrate the wrenfold workflow, we will generate a small example function and then invoke the
generated code in C++ and Rust. To keep the code brief, we will choose something very simple for our
example - the `Rosenbrock <https://en.wikipedia.org/wiki/Rosenbrock_function>`_ function:

.. math::

   f\left(x, y\right) = \left(a - x\right)^2 + b\cdot\left(y - x^2\right)^2

First we express the function as a python function that manipulates wrenfold types:

.. literalinclude:: quick_start_files/quick_start_script.py
    :language: python
    :start-after: function_def_start
    :end-before: function_def_end

The argument type annotations let wrenfold know what dimensions and numerical types to expect for
the input arguments to :math:`f`. Our python function returns two outputs:

  #. The first is the value of the Rosenbrock function, :math:`f`. We specify that this will be the
     *return value* of the generated C++ function.
  #. The second is the Jacobian of :math:`f`, taken with respect to :math:`\left[x, y\right]`, which
     will be an *output argument* of the generated C++ function.

To generate some actual code, we run:

.. literalinclude:: quick_start_files/quick_start_script.py
    :language: python
    :start-after: transpilation_start
    :end-before: transpilation_end

Which produces:

.. literalinclude:: quick_start_files/rosenbrock.h
    :language: cpp

A couple of observations about this output:

  * The vector-valued input and output arguments were mapped to generic types. The generated function
    constructs n-dimensional spans in order to read/write to the vectors ``xy`` and ``f_D_xy``.
  * Terms that appear in both ``f`` and ``f_D_xy`` were extracted and shared between both outputs.
  * Our generated code depends on the `wrenfold runtime <https://github.com/wrenfold/wrenfold/tree/main/components/runtime>`_,
    a small header-only library that provides the :class:`wf::span` type.


.. tip::

   The runtime is also installed by the python wheel. It can be found at
   ``<VENV ROOT>/include/site/python3.XX/wrenfold``.


Calling the generated C++
-------------------------

Next, we will create a simple C++ program that evaluates our generated function. wrenfold functions
can be made to work with :doc:`any dense matrix representation <reference/integrating_code>`. In
this tutorial we will use `Eigen <https://eigen.tuxfamily.org/>`_, as it is one of the most popular
choices and wrenfold supports it out of the box.

Our C++ file looks like:

.. literalinclude:: quick_start_files/main.cpp
    :language: cpp

To compile it we need to provide include paths for Eigen and the wrenfold runtime headers. You may
need to adjust these paths for your system.

.. code:: bash

   g++ -std=c++17 -I/usr/local/include/eigen3 -I<WRENFOLD REPO>/components/runtime main.cpp
   ./a.out

And sure enough our test program outputs::

   f = 0
   f_D_xy = [0, 0]

.. tip::

   The wrenfold repo contains
   `a more complete version <https://github.com/wrenfold/wrenfold/tree/main/examples/rosenbrock>`_
   of this example.

Calling generated Rust
----------------------

By swapping out ``CppGenerator`` for ``RustGenerator``, we can emit rust code instead:

.. literalinclude:: quick_start_files/quick_start_script.py
    :language: python
    :start-after: rust_transpilation_start
    :end-before: rust_transpilation_end

Which produces:

.. literalinclude:: quick_start_files/rosenbrock_crate/src/generated.rs
   :language: rust

Like the C++ example, the generated rust code has a small runtime dependency. In this instance, it
is a set of traits for constraining the input and output arguments. These traits are found in the
`wrenfold-traits <https://crates.io/crates/wrenfold-traits>`_ crate. The ``wrenfold-traits`` crate
includes default implementations for use with
`nalgebra <https://docs.rs/nalgebra/latest/nalgebra/>`_.

To test our generated function, we will create a small test crate with the following ``Cargo.toml``:

.. literalinclude:: quick_start_files/rosenbrock_crate/Cargo.toml
   :language: toml

.. tip::

   Note that we enabled ``features = ["nalgebra"]`` for the ``wrenfold-traits`` crate.

And the following ``lib.rs``:

.. literalinclude:: quick_start_files/rosenbrock_crate/src/lib.rs
   :language: rust
