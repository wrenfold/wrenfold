# c_generation

This is an example of defining a custom code generator in Python. The target language is C99. The generator is defined in
[c_code_generator.py](./c_code_generator.py).

This example can be invoked by running:

```bash
python -m examples.c_generation.c_generation <path to output header>
```

At runtime, the generated code depends on two simple span types defined in [c_span_types.h](./c_span_types.h). By overriding methods on `CCodeGenerator` (such as `format_function_signature`), you can customize the types used to pass input and output matrices to the generated function.
