Adding a new target language
============================

wrenfold can be extended to target new languages by customizing the code generation step. For a
complete demonstration, refer to the
`python_generation <https://github.com/wrenfold/wrenfold/blob/main/examples/python_generation/python_generation.py>`__
example. This section provides a brief overview.

To begin with, one must subclass the :class:`wrenfold.code_generation.BaseGenerator` type in python.
**At a minimum**, a ``format_function_definition`` method must be defined for the
:class:`wrenfold.code_generation.FunctionDefinition` type (the top level AST node).

Most nodes in the AST will have children - for example :class:`wrenfold.ast.Add` has ``left`` and
``right`` members enumerating the two operands of a binary addition. To facilitate recursive
formatting, the ``BaseGenerator`` type provides an overloaded
:func:`wrenfold.code_generation.BaseGenerator.format` method that automatically delegates to the
appropriately named formatter on your subclass.

For example, when printing ``ast.Add`` we can avoid inspecting the type of ``add.left``, and simply
pass it to ``self.format(...)``.

.. code:: python

    from wrenfold import ast
    from wrenfold.code_generation import BaseGenerator

    class CustomGenerator(BaseGenerator):
        """A generator for a new language."""

        def format_add(self, add: ast.Add) -> str:
            # Calling `format(...)` will automatically delegate to the appropriate overload.
            # If the argument is an `ast.Add`, it will recurse back into this method, for example.
            return ' + '.join(self.format(x) for x in add.args)

        # ... more overloads ...

        def format_compare(self, compare: ast.Compare) -> str:
            if compare.operation == RelationalOperation.LessThan:
                op = '<'
            elif compare.operation == RelationalOperation.LessThanOrEqual:
                op = '<='
            elif compare.operation == RelationalOperation.Equal:
                op = '=='
            else:
                raise NotImplementedError(f"Unknown operation: {compare.operation}")
            return f'{self.format(compare.left)} {op} {self.format(compare.right)}'

.. note::

    There is no hard requirement that you use the ``BaseGenerator`` infrastructure to implement
    code-generation. :func:`wrenfold.code_generation.transpile` directly returns the top-level
    ``FunctionDefinition`` object, which can be traversed by alternative means if desired.


With a ``BaseGenerator`` subclass in hand, we can generate code:

.. code:: python

    def some_symbolic_func(x: type_annotations.FloatScalar):
        return x * 2

    # Here we replace the built-in generator with `CustomGenerator`:
    code = code_generation.generate_function(some_symbolic_func, generator=CustomGenerator())
