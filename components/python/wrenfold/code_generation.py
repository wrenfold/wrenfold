"""
Utility functions to support code-generation.
"""
import dataclasses
import inspect
import pathlib
import typing as T

from pywrenfold.gen import (
    Argument,
    ArgumentDirection,
    BaseGenerator,
    CppGenerator,
    FunctionDescription,
    OptimizationParams,
    OutputKey,
    PythonGenerator,
    PythonGeneratorFloatWidth,
    PythonGeneratorTarget,
    RustGenerator,
    cse_function_description,
    transpile,
)

from . import custom_types, sym, type_annotations, type_info


@dataclasses.dataclass
class ReturnValue:
    """
    Designate a return value in the result of symbolic function invocation.

    Attributes:
      expression: The returned value. This may be an expression, or an instance of a user-provided
        custom type.
    """
    expression: T.Union[sym.Expr, sym.MatrixExpr, T.Any]


@dataclasses.dataclass
class OutputArg:
    """
    Designate an output argument in the result of a symbolic function invocation.

    Attributes:
      expression: Value of the output argument. This may be an expression, or an instance of a
        user-provided custom type.
      name: Name of the argument.
      is_optional: Specify whether the output argument is optional or not.
    """
    expression: T.Union[sym.Expr, sym.MatrixExpr, T.Any]
    name: str
    is_optional: bool = False


# Union of ReturnValue and OutputArg
ReturnValueOrOutputArg = T.Union[ReturnValue, OutputArg]

# Things that can be returned from symbolic python functions.
CodegenFuncInvocationResult = T.Union[sym.Expr, sym.MatrixExpr, T.Sequence[ReturnValueOrOutputArg]]


def create_function_description(func: T.Callable[..., CodegenFuncInvocationResult],
                                name: T.Optional[str] = None) -> FunctionDescription:
    """
    Accept a python function that manipulates symbolic mathematical expressions, and convert it
    to a :class:`wrenfold.code_generation.FunctionDescription` object. The provided function is
    invoked, and its output expressions are captured and stored, along with a signature that
    carries type information required to emit code.

    Tip:
      The provided callable must be type annotated so that wrenfold can deduce the type of input
      expressions required to invoke it. See :doc:`type_annotations` for built-in types that can be
      used to annotate functions. The function should return either:

        * A sequence of :class:`wrenfold.code_generation.ReturnValue` or
          :class:`wrenfold.code_generation.OutputArg` objects, OR
        * A single :class:`wrenfold.sym.Expr` or :class:`wrenfold.sym.MatrixExpr` object, which will
          be interpreted as a ``ReturnValue``.

    Args:
      func: A symbolic python function with type-annotated arguments.
      name: String name of the function.

    Returns:
      An instance of :class:`wrenfold.code_generation.FunctionDescription`.

    Example:
      >>> from wrenfold.type_annotations import FloatScalar
      >>> from wrenfold import code_generation
      >>>
      >>> def foo(x: FloatScalar, y: FloatScalar):
      >>>     # One return value, and one output argument named `z`:
      >>>     return [code_generation.ReturnValue(x * y), code_generation.OutputArg(x + y, "z")]
      >>> description = code_generation.create_function_description(func=foo)
      >>> print(description)
      FunctionDescription('foo', 3 args)

      The description can then be transpiled to a target language:

      >>> definition = code_generation.transpile(description)
      >>> print(definition)
      FunctionDefinition('foo', <3 arguments>, <7 elements>)
      >>> code = code_generation.CppGenerator().generate(definition=definition)
      >>> print(code)

      .. code-block:: cpp
        :linenos:

        template <typename Scalar>
        Scalar foo(const Scalar x, const Scalar y, Scalar& z)
        {
            // Operation counts:
            // add: 1
            // multiply: 1
            // total: 2

            const Scalar v01 = y;
            const Scalar v00 = x;
            const Scalar v04 = v00 + v01;
            const Scalar v02 = v00 * v01;
            z = v04;
            return v02;
        }
    """
    spec = inspect.getfullargspec(func=func)
    description = FunctionDescription(name=name or func.__name__)

    cached_types: T.Dict[T.Type, type_info.CustomType] = dict()
    kwargs = dict()
    for arg_name in spec.args:
        if arg_name not in spec.annotations:
            raise KeyError(f'Missing type annotation for argument: {arg_name}')
        # Map argument types to something the code-generation logic can understand:
        annotated_type = spec.annotations[arg_name]
        arg_type = custom_types.convert_to_internal_type(
            python_type=annotated_type, cached_custom_types=cached_types)

        input_expression = description.add_input_argument(arg_name, arg_type)
        if isinstance(arg_type, type_info.CustomType):
            if issubclass(annotated_type, type_annotations.Opaque):
                kwargs[arg_name] = annotated_type(provenance=input_expression)
            else:
                arg_elements = sym.create_compound_expression_elements(
                    provenance=input_expression, num=arg_type.total_size)
                kwargs[arg_name], _ = custom_types.map_expressions_into_custom_type(
                    expressions=arg_elements, custom_type=annotated_type)
        else:
            kwargs[arg_name] = input_expression

    # run the function
    result_expressions: CodegenFuncInvocationResult = func(**kwargs)

    if isinstance(result_expressions,
                  (sym.Expr, sym.MatrixExpr)) or dataclasses.is_dataclass(result_expressions):
        # if one thing was returned, interpret it as the return value:
        result_expressions = (ReturnValue(expression=result_expressions),)

    for val in result_expressions:
        if isinstance(val, ReturnValue):
            if dataclasses.is_dataclass(val.expression):
                custom_type = custom_types.convert_to_internal_type(
                    python_type=type(val.expression), cached_custom_types=cached_types)
                description.set_return_value(
                    custom_type=custom_type,
                    expressions=custom_types.map_expressions_out_of_custom_type(
                        instance=val.expression))
            else:
                description.set_return_value(val.expression)
        elif isinstance(val, OutputArg):
            if dataclasses.is_dataclass(val.expression):
                custom_type = custom_types.convert_to_internal_type(
                    python_type=type(val.expression), cached_custom_types=cached_types)
                description.add_output_argument(
                    name=val.name,
                    is_optional=val.is_optional,
                    custom_type=custom_type,
                    expressions=custom_types.map_expressions_out_of_custom_type(
                        instance=val.expression))
            else:
                description.add_output_argument(
                    name=val.name, is_optional=val.is_optional, value=val.expression)
        else:
            raise TypeError(f"Returned values must be: {ReturnValueOrOutputArg}, got: {type(val)}")

    return description


def generate_function(func: T.Callable[..., CodegenFuncInvocationResult],
                      generator: T.Union[CppGenerator, RustGenerator, PythonGenerator,
                                         BaseGenerator],
                      name: T.Optional[str] = None,
                      optimization_params: T.Optional[OptimizationParams] = None,
                      convert_ternaries: bool = True) -> str:
    """
    Accept a python function that manipulates symbolic mathematical expressions, and convert it
    to code in the language emitted by ``generator``. This is a three-step process:

        #. The signature of the provided function is inspected to generate symbolic input arguments.
           Next, it is invoked and the symbolic outputs are recorded.
        #. The expression tree is flattened and optimized by :func:`wrenfold.code_generation.transpile`.
           Duplicate operations are eliminated during this step, and conditionals are converted to
           control flow. The simplified output is converted to a syntax tree.
        #. Lastly, the syntax is passed to the provided ``generator`` to emit usable code.

    Tip:

      For examples of the types of functions that wrenfold can generate, see the
      `wrenfold repo <https://github.com/wrenfold/wrenfold/tree/main/examples>`_.

    Args:
        func: A symbolic python function with type-annotated arguments. See
          :func:`wrenfold.code_generation.create_function_description` for notes on the expected
          signature.
        generator: Instance of a code generator, eg. :class:`wrenfold.code_generation.CppGenerator`.
        name: Name of the function. If unspecified, ``func.__name__`` will be used.
        optimization_params: Parameters governing simplifications/optimizations applied to the
          output code.
        convert_ternaries: Whether to convert ternary :func:`wrenfold.sym.where` statements to
          if-else control flow. Defaults to true. You likely want to set this to False when
          targeting python frameworks that need to trace control-flow, for example PyTorch or JAX.

    Returns:
      * A string of generated code.

    Example:
      >>> from wrenfold.type_annotations import FloatScalar
      >>> from wrenfold import code_generation
      >>>
      >>> def foo(x: FloatScalar, y: FloatScalar):
      >>>     # One return value, and one output argument named `z`:
      >>>     return [code_generation.ReturnValue(x * y), code_generation.OutputArg(x + y, "z")]
      >>>
      >>> code = code_generation.generate_function(func=foo)
      >>> print(code)

      .. code-block:: cpp
        :linenos:

        template <typename Scalar>
        Scalar foo(const Scalar x, const Scalar y, Scalar& z)
        {
            // Operation counts:
            // add: 1
            // multiply: 1
            // total: 2

            const Scalar v01 = y;
            const Scalar v00 = x;
            const Scalar v04 = v00 + v01;
            const Scalar v02 = v00 * v01;
            z = v04;
            return v02;
        }
    """
    description = create_function_description(func=func, name=name)
    func_ast = transpile(
        description, optimization_params=optimization_params, convert_ternaries=convert_ternaries)
    return generator.generate(definition=func_ast)


def generate_python(
    func: T.Callable[..., CodegenFuncInvocationResult],
    target: PythonGeneratorTarget,
    convert_ternaries: T.Optional[bool] = None,
    context: T.Optional[T.Dict[str, T.Any]] = None,
    import_target_module: bool = True,
    generator_type: T.Callable[[PythonGeneratorTarget], BaseGenerator] = PythonGenerator
) -> T.Tuple[T.Callable, str]:
    """
    Code-generate a symbolic function as python code, then ``exec`` the code and return a python
    function that implements the symbolic function numerically.

    Args:
        func: A symbolic python function with type-annotated arguments. See
          :func:`wrenfold.code_generation.create_function_description` for notes on the expected
          signature.
        target: Which Python API to target (ie. NumPy, PyTorch, etc).
        convert_ternaries: Whether to convert :func:`wrenfold.sym.where` expressions to Python
          control flow. For frameworks like PyTorch and JAX, we need to leave conditionals in a
          traceable format (ie. ``th.where`` calls). By default, if ``convert_ternaries=None``,
          wrenfold will not convert ``sym.where`` calls to if-else statements when targeting PyTorch
          and JAX. This allows generated functions to be batched and JIT compiled.
        context: Dict of key-value pairs that will be passed to
          `exec <https://docs.python.org/3/library/functions.html#exec>`_ in the ``globals`` arg.
        import_target_module: If true (the default), import the target API. See the warning below.
        generator_type: By default this is :class:`wrenfold.code_generation.PythonGenerator`. You
          may specify a different function to call to construct the code generator.

    Returns:
      * A callable python function that implements ``func`` numerically.
      * A string containing the corresponding python code.

    Warning:

      By default, wrenfold will automatically import the appropriate framework specified by
      ``target``:

        * If the target is ``NumPy``, ``numpy`` will be imported as ``np``.
        * If the target is ``JAX``, ``jax.numpy`` will be imported as ``jnp``.
        * If the target is ``PyTorch``, ``torch`` will be imported as ``th``.

      To suppress the default import behavior, specify ``import_target_module=False``. You will then
      need to pass your own import in the ``context`` dict.

    Tip:

      Code-generation is performed using the :class:`wrenfold.code_generation.PythonGenerator`
      class. Because python lacks formal "output arguments", any symbolic outputs tagged as
      :class:`wrenfold.code_generation.OutputArg` will instead be **returned** from the generated
      function in a dict of key-value pairs. The example listing below illustrates this behavior.

      **Additionally**, remember that your target framework may not be able to reason about your
      custom types. For example, `jax.jit <https://jax.readthedocs.io/en/latest/_autosummary/jax.jit.html>`_
      only operates on Jax arrays and standard python types (tuples, lists, dict, etc).

    Example:
      >>> import numpy as np
      >>> import jax
      >>>
      >>> from wrenfold import code_generation, sym
      >>> from wrenfold.type_annotations import Vector3
      >>>
      >>> def foo(x: Vector3, y: Vector3):
      >>>     # A simple test function, with one return value and one output argument.
      >>>     # In-practice, you would probably be generating something more complicated than this.
      >>>     dot, = x.T * y
      >>>     f = sym.tanh(dot)
      >>>     J = sym.jacobian([f], x)
      >>>     return [code_generation.ReturnValue(f), code_generation.OutputArg(J, "J")]
      >>>
      >>> # Generate python code:
      >>> func, code = code_generation.generate_python(
      >>>   foo, target=code_generation.PythonGeneratorTarget.JAX)
      >>> print(code) # See python listing below.
      >>>
      >>> # Generate a batched and JIT compiled version of our function using JAX.
      >>> # Here we batch over both `x` and `y`.
      >>> batched_func = jax.vmap(func, in_axes=(0, 0), out_axes=0)
      >>> compiled_func = jax.jit(batched_func)
      >>>
      >>> # Execute the function on NumPy tensors.
      >>> # `output1` contains the return value, while `output2` contains all the output arguments.
      >>> x = np.random.uniform(size=(10, 3))
      >>> y = np.random.uniform(size=(10, 3))
      >>> output1, output2 = compiled_func(x, y)
      >>>
      >>> print(output1) # produces: [0.73317385 0.45288894, ...]

      .. code-block:: python
        :linenos:

        # The generated code for `foo`:
        def foo(x: jnp.ndarray, y: jnp.ndarray) -> T.Tuple[jnp.ndarray, T.Dict[str, jnp.ndarray]]:
            x = x.reshape(3, 1)
            y = y.reshape(3, 1)
            v009 = y[2, 0]
            v008 = x[2, 0]
            v006 = y[1, 0]
            v005 = x[1, 0]
            v003 = x[0, 0]
            v000 = y[0, 0]
            v012 = jnp.tanh(v000 * v003 + v005 * v006 + v008 * v009)
            v016 = jnp.asarray(1, dtype=jnp.float32) + -(v012 * v012)
            J = jnp.array([
                v000 * v016,
                v006 * v016,
                v009 * v016]).reshape(1, 3)
            return (
                v012,
                dict(J=J)
            )
    """
    generator = generator_type(target)
    if convert_ternaries == None:
        # Convert ternaries to conditional if-else blocks when targeting NumPy.
        convert_ternaries = target == PythonGeneratorTarget.NumPy

    code = generate_function(func, generator=generator, convert_ternaries=convert_ternaries)

    # Execute the generated code and return the specified function. This is a fair bit easier
    # than writing it out and using importlib, which fails anyway on windows when we write
    # the file to /tmp: https://stackoverflow.com/questions/66884520/
    # We pass the function out via a dict: https://github.com/python/cpython/issues/118888

    globals_in = globals()
    if context is not None:
        globals_in.update(context)

    # Local imports here because these are optional runtime dependencies.
    if import_target_module:
        if target == PythonGeneratorTarget.JAX:
            import jax.numpy as jnp
            globals_in['jnp'] = jnp
        elif target == PythonGeneratorTarget.PyTorch:
            import torch as th
            globals_in['th'] = th
        elif target == PythonGeneratorTarget.NumPy:
            import numpy as np
            globals_in['np'] = np

    locals_in_out = dict()
    try:
        # Security: exec is not great, but ultimately the code executed here is constrained in scope
        # and functionality.
        exec(code, globals_in, locals_in_out)
    except:
        print('Encountered exception while evaluating:')
        print(code)
        raise
    return locals_in_out[func.__name__], code


def mkdir_and_write_file(code: str, path: T.Union[str, pathlib.Path]) -> None:
    """
    Write ``code`` to the specified path. Create intermediate directories as required.

    Args:
      code: String containing file contents.
      path: Path to the destination file.
    """
    if isinstance(path, str):
        path = pathlib.Path(path)
    if not path.parent.exists():
        path.parent.mkdir(parents=True)
    with open(path, 'wb') as handle:
        # Encode to UTF8 and write binary so we get \n and not \r\n
        handle.write(code.encode('utf-8'))
        handle.flush()
