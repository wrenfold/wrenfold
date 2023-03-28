"""Utility functions to support code-generation."""
import inspect
import itertools
import typing as T

from . import sym
from sym_wrapper import pycodegen as codegen

AstType = T.Union[codegen.Add, codegen.Assignment, codegen.Call, codegen.Declaration,
                  codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                  codegen.Multiply, codegen.OutputBlock, codegen.ReturnValueBlock,
                  codegen.VariableRef]


class format_override:

    def __init__(self, fn: T.Callable):
        self._fn = fn

    def __set_name__(self, owner: T.Type, name: str):
        # do something with owner, i.e.
        assert isinstance(owner, type)
        assert issubclass(owner,
                          CodeGenerator), f'Class does not inherit from CodeGenerator: {owner}'

        # Determine the argument type:
        spec = inspect.getfullargspec(func=self._fn)
        assert len(
            spec.args
        ) == 2, f'Methods decorated with @format_override should have two args. Method `{self._fn}` has args: {spec.args}'
        _, arg_name = spec.args

        assert arg_name in spec.annotations, f'Argument `{arg_name}` of method `{self._fn}` is missing a type annotation.'
        arg_type = spec.annotations[arg_name]

        # And then create a new method with the correct name
        full_name = f'_format_{arg_type.__name__}'
        print(f'setting {full_name}')
        setattr(owner, full_name, self._fn)


class CodeGenerator:

    def __init__(self) -> None:
        self._format_methods: T.Dict[T.Type, T.Callable] = dict()


class PythonCodeGenerator(CodeGenerator):

    def __init__(self) -> None:
        pass

    def format_add(self, x: codegen.Add) -> str:
        """Format one of the AST types to python."""
        pass

    def format_assignment(self, x: codegen.Assignment) -> str:
        pass


def codegen_function(func: T.Callable, name: T.Optional[str] = None):
    """Code-generate the provided function."""
    spec = inspect.getfullargspec(func=func)
    kwargs = dict()
    for arg_index, arg_name in enumerate(spec.args):
        if arg_name not in spec.annotations:
            raise KeyError(f'Missing type annotation for argument: {arg_name}')
        type_annotation = spec.annotations[arg_name]
        if issubclass(type_annotation, sym.Expr):
            kwargs[arg_name] = codegen.create_function_argument(arg_index)
        elif issubclass(type_annotation, sym.MatrixExpr):
            kwargs[arg_name] = codegen.create_matrix_function_argument(
                arg_index, *type_annotation.SHAPE)
        else:
            raise TypeError(f'Invalid type used in annotation: {type_annotation}')

    # run the function
    return_value = func(**kwargs)
    if not isinstance(return_value, tuple):
        return_value = (return_value,)

    # build the function signature:
    signature = codegen.FunctionSignature(name or func.__name__)

    output_expressions = []
    for val in return_value:
        assert isinstance(val, (sym.Expr, sym.MatrixExpr)), f"Invalid type: {type(val)}"
        if isinstance(val, sym.Expr):
            # If return value is Expr, see if it can be coerced to matrix
            try:
                val = sym.MatrixExpr(val)
            except sym.TypeError:
                pass

        if isinstance(val, sym.Expr):
            signature.add_return_value(codegen.ScalarType())
            output_expressions.append(val)
        elif isinstance(val, sym.MatrixExpr):
            signature.add_return_value(codegen.MatrixType(*val.shape))
            flat = itertools.chain.from_iterable(val.to_list())
            output_expressions.extend(list(flat))

    for arg_index, arg_name in enumerate(spec.args):
        type_annotation = spec.annotations[arg_name]
        if issubclass(type_annotation, sym.Expr):
            signature.add_input_arg(name=arg_name, type=codegen.ScalarType())
        elif issubclass(type_annotation, sym.MatrixExpr):
            signature.add_input_arg(name=arg_name, type=codegen.MatrixType(*type_annotation.SHAPE))

    #
    builder = codegen.IrBuilder(output_expressions)
    builder.eliminate_duplicates()
    ast = builder.create_ast(signature)
