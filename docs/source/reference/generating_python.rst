Generating Python
=================

When iterating on a function design, it is sometimes useful to be able to evaluate the function
in a python script or notebook. To that end, wrenfold can generate python functions that invoke the
NumPy/PyTorch, and `JAX <https://jax.readthedocs.io>`__ APIs.

To directly generate a python callable, we can use :func:`~wrenfold.code_generation.generate_python`:

.. code:: python

    # `step_np_func` will be a callable that accepts scalar `x`:
    # `step_code` is a string of python code.
    step_np_func, step_code = code_generation.generate_python(func=step_clamped)

    y, df = step_np_func(x=0.32, compute_df=True)

    print(y)  # prints: 0.241664
    print(df) # prints: array([[1.3055999], [2.16]], dtype=float64)

The listing above produces a callable ``step_np_func`` that operates on NumPy types. Note that
optional output arguments are converted into optional *return values* when targeting Python. We pass
``compute_df=True`` to request that output ``df`` be computed, and it is returned in the output
tuple.

When targeting JAX or PyTorch, it is advantageous to leave conditionals like :func:`wrenfold.sym.where`
in ternary form and convert them to `jax.where <https://jax.readthedocs.io/en/latest/_autosummary/jax.numpy.where.html>`__
or `torch.where <https://pytorch.org/docs/stable/generated/torch.where.html>`__ instead of Python
conditional logic. For example:

.. code:: python

    # Jax does not support float64, so we make sure we generate code that uses float32 types.
    geneator = code_generation.PythonGenerator(
        target=code_generation.PythonGeneratorTarget.JAX,
        float_width=code_generation.PythonGeneratorFloatWidth.Float32
    )

    step_jax_func, step_code = code_generation.generate_python(
        func=step_clamped, generator=generator, convert_ternaries=False)

    print(step_code)

.. code:: python

    # The generated JAX function:
    def step_clamped(x: jnp.ndarray, compute_df: bool) -> T.Tuple[jnp.ndarray, jnp.ndarray]:
        v002 = x
        v006 = jnp.where(
            (v002 < jnp.asarray(0, dtype=jnp.float32)),
            jnp.asarray(0, dtype=jnp.float32),
            v002,
        )
        # ... output truncated ...
        return (
            v009
            * v009
            * (
                jnp.asarray(3, dtype=jnp.float32) + jnp.asarray(2, dtype=jnp.float32) * v043
            ),
            df,
        )

By leaving conditionals in ternary form (or "element selection" form), we retain the ability to
use them during back-propagation. This behavior can be disabled (thereby producing if-statements) by
specifying ``convert_ternaries=True``.

.. tip::

    For longer form examples of python generation, see
    `jax_camera_model <https://github.com/wrenfold/wrenfold/tree/main/examples/jax_camera_model>`__
    and `cart-pole <https://github.com/wrenfold/wrenfold/tree/main/examples/cart_pole>`__.


JIT compilation with Numba
--------------------------

wrenfold-generated NumPy code is intended to be compatible with
`numba JIT compilation <https://numba.pydata.org/numba-doc/dev/reference/jit-compilation.html>`__.
To JIT a generated function, use the ``PythonGeneratorTarget.Numpy`` target and then pass the output
function to ```numba.njit``:

.. code:: python

    import numba

    # NumPy is the default argument for `generator`, but in this instance we will pass a customized
    # instance that uses float32 for all numeric types:
    generator = code_generation.PythonGenerator(
        float_width=code_generation.PythonGeneratorFloatWidth.Float32
    )

    step_jax_func, _ = code_generation.generate_python(func=step_clamped, generator=generator)

    jit_compiled_func = numba.njit(step_jax_func)
