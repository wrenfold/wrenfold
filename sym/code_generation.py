"""Utility functions to support code-generation."""
import collections
import inspect
import itertools
import string
import typing as T

import numpy as np

from . import sym
from sym_wrapper import pycodegen as codegen

AstVariantTuple = (codegen.Add, codegen.Call, codegen.ConstructMatrix, codegen.Declaration,
                   codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                   codegen.Multiply, codegen.OutputExists, codegen.ReturnValue, codegen.VariableRef,
                   codegen.FunctionSignature)

AstTypeTuple = (codegen.ScalarType, codegen.MatrixType)

AstVariantAnnotation = T.Union[codegen.Add, codegen.Call, codegen.ConstructMatrix,
                               codegen.Declaration, codegen.FloatConstant, codegen.InputValue,
                               codegen.IntegerConstant, codegen.Multiply, codegen.OutputExists,
                               codegen.ReturnValue, codegen.VariableRef]


class CustomStringFormatter(string.Formatter):

    def __init__(self, generator: 'CodeGenerator') -> None:
        super().__init__()
        self._generator = generator
        self._type_to_method: T.Dict[type, T.Callable] = dict()

    def _get_formatter(self, arg_type: T.Type) -> T.Callable:
        if arg_type not in self._type_to_method:
            format_method_name = f'format_{arg_type.__name__}'
            if not hasattr(self._generator, format_method_name):
                raise KeyError(f"Code generator is missing formatting method: {format_method_name}")
            self._type_to_method[arg_type] = getattr(self._generator, format_method_name)
        return self._type_to_method[arg_type]

    def format_field(self, value: T.Any, format_spec: str) -> T.Any:
        """Override Formatter.format_field so we can intercept code-gen types."""
        if isinstance(value, AstVariantTuple) or isinstance(value, AstTypeTuple):
            return self.format_ast(value=value)
        return super().format_field(value=value, format_spec=format_spec)

    def format_ast(
            self, value: T.Union[AstVariantAnnotation, codegen.ScalarType,
                                 codegen.MatrixType]) -> str:
        format_method = self._get_formatter(arg_type=type(value))
        return format_method(self, value)

    @staticmethod
    def indent(string: str, amount: int = 2) -> str:
        """Indent the provided string by `amount` spaces."""
        assert amount >= 0, f'amount = {amount}'
        return (' ' * amount) + string.replace('\n', '\n' + ' ' * amount).rstrip(' ')


class CodeGenerator:

    def __init__(self) -> None:
        self._type_to_method: T.Dict[type, T.Callable] = dict()

    def get_formatter(self, arg_type: T.Type) -> T.Callable:
        if arg_type not in self._type_to_method:
            format_method_name = f'format_{arg_type.__name__}'
            if not hasattr(self, format_method_name):
                raise KeyError(f"Code generator is missing formatting method: {format_method_name}")
            self._type_to_method[arg_type] = getattr(self, format_method_name)
        return self._type_to_method[arg_type]

    def generate(self, signature: codegen.FunctionSignature,
                 body: T.List[AstVariantAnnotation]) -> str:
        formatter = CustomStringFormatter(generator=self)
        result = formatter.format('{}\n', signature)
        reshape_statements = []
        for arg in signature.input_args:
            if isinstance(arg.type, codegen.MatrixType):
                statement = f'{arg.name} = {arg.name}.reshape(({arg.type.num_rows}, {arg.type.num_cols}))'
                reshape_statements.append(statement)
        if reshape_statements:
            result += formatter.indent('\n'.join(reshape_statements))
            result += '\n'
        result += formatter.indent('\n'.join(formatter.format_ast(v) for v in body))
        return result


class PythonCodeGenerator(CodeGenerator):

    def __init__(self) -> None:
        super(PythonCodeGenerator, self).__init__()

    def format_ScalarType(self, fmt: CustomStringFormatter, x: codegen.ScalarType) -> str:
        return 'float'

    def format_MatrixType(self, fmt: CustomStringFormatter, x: codegen.MatrixType) -> str:
        return 'np.ndarray'

    def format_Add(self, fmt: CustomStringFormatter, x: codegen.Add) -> str:
        """Format one of the AST types to python."""
        return fmt.format('{} + {}', x.left, x.right)

    def format_Call(self, fmt: CustomStringFormatter, x: codegen.Call) -> str:
        if isinstance(x.function, codegen.BinaryFunctionName):
            if x.function == codegen.BinaryFunctionName.Pow:
                return fmt.format('np.power({}, {})', x.args[0], x.args[1])
        elif isinstance(x.function, codegen.UnaryFunctionName):
            pass

        raise KeyError(f'Unsupported function name: {x.function}')

    def format_Conditional(self, fmt: CustomStringFormatter, x: codegen.Conditional) -> str:
        result += fmt.format('if {}:\n', x.condition) + fmt.indent('\n'.join(
            fmt.format_ast(v) for v in x.if_branch))
        if len(x.else_branch) > 0:
            result += fmt.indent('else:\n') + fmt.indent('\n'.join(
                fmt.format_ast(v) for v in x.else_branch))
        return result

    def format_Declaration(self, fmt: CustomStringFormatter, x: codegen.Declaration) -> str:
        """Format declaration."""
        return fmt.format('{name}: {type} = {value}', name=x.name, type=x.type, value=x.value)

    def format_FloatConstant(self, fmt: CustomStringFormatter, x: codegen.FloatConstant) -> str:
        return '{:.16}'.format(x.value)

    def format_FunctionSignature(self, fmt: CustomStringFormatter,
                                 x: codegen.FunctionSignature) -> str:
        args = []
        for arg in x.input_args + x.output_args:
            if arg.is_optional:
                args.append(fmt.format('{}: T.Optional[{}]', arg.name, arg.type))
            else:
                args.append(fmt.format('{}: {}', arg.name, arg.type))

        return_types = x.return_values
        if len(return_types) == 1:
            return_type_str = fmt.format('{}', return_types[0])
        else:
            return_type_str = 'T.Tuple[{}]'.format(', '.join(
                fmt.format_ast(v) for v in return_types))

        return f'def {x.name}({", ".join(args)}) -> {return_type_str}:'

    def format_InputValue(self, fmt: CustomStringFormatter, x: codegen.InputValue) -> str:
        argument = x.argument
        if isinstance(argument.type, codegen.ScalarType):
            return argument.name
        elif isinstance(argument.type, codegen.MatrixType):
            return fmt.format('{}[{}, {}]', argument.name,
                              *argument.type.compute_indices(x.element))
        else:
            raise TypeError(f'Unsupported type: {argument.type}')

    def format_IntegerConstant(self, fmt: CustomStringFormatter, x: codegen.IntegerConstant) -> str:
        return str(x.value)

    def format_Multiply(self, fmt: CustomStringFormatter, x: codegen.Multiply) -> str:
        return fmt.format('{} * {}', x.left, x.right)

    def format_OutputExists(self, fmt: CustomStringFormatter, x: codegen.OutputExists) -> str:
        assert x.argument.is_optional, 'Argument must be optional'
        return f'{x.argument.name} is not None'

    def format_ReturnValue(self, fmt: CustomStringFormatter, x: codegen.ReturnValue) -> str:
        values = x.values
        if len(values) > 1:
            return fmt.format('return {}', values[0])
        else:
            joined = ', '.join(fmt.format_ast(v) for v in values)
            return f'return {joined}'

    def format_VariableRef(self, fmt: CustomStringFormatter, x: codegen.VariableRef) -> str:
        return x.name


def codegen_function(func: T.Callable,
                     name: T.Optional[str] = None) -> T.List[AstVariantAnnotation]:
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

    # Convert to ast that we can emit:
    builder = codegen.IrBuilder(output_expressions)
    builder.eliminate_duplicates()
    ast = builder.create_ast(signature)
    return (signature, ast)


def create_numeric_evaluator(func: T.Callable) -> T.Callable:
    spec = inspect.getfullargspec(func=func)
    function_args = collections.OrderedDict()
    for arg_index, arg_name in enumerate(spec.args):
        if arg_name not in spec.annotations:
            raise KeyError(f'Missing type annotation for argument: {arg_name}')
        type_annotation = spec.annotations[arg_name]
        if issubclass(type_annotation, sym.Expr):
            function_args[arg_name] = codegen.create_function_argument(arg_index)
        elif issubclass(type_annotation, sym.MatrixExpr):
            function_args[arg_name] = codegen.create_matrix_function_argument(
                arg_index, *type_annotation.SHAPE)
        else:
            raise TypeError(f'Invalid type used in annotation: {type_annotation}')

    # run the function
    symbolic_return_values = func(**function_args)
    if not isinstance(symbolic_return_values, tuple):
        symbolic_return_values = (symbolic_return_values,)

    def subs(
        name: str, output: T.Union[sym.Expr, sym.MatrixExpr], symbolic: T.Union[sym.Expr,
                                                                                sym.MatrixExpr],
        numeric: T.Union[float,
                         np.ndarray]) -> T.Union[sym.Expr, sym.MatrixExpr, float, np.ndarray]:
        if isinstance(symbolic, sym.MatrixExpr):
            assert isinstance(numeric, np.ndarray), f'Input {name} must be an array'
            numeric = numeric.reshape(symbolic.shape)
            result = output
            for i in range(0, symbolic.shape[0]):
                for j in range(0, symbolic.shape[1]):
                    result = result.subs(target=symbolic[i, j], substitute=numeric[i, j])
            return result.eval()
        else:
            assert isinstance(numeric, (int, float)), f'Input {name} must be integer or float'
            return output.subs(target=symbolic, substitute=numeric).eval()

    # Create a function that substitutes the appropriate values
    def evaluator(*args, **kwargs):
        # Put any position args into kwargs:
        num_args = len(args) + len(kwargs)
        assert num_args <= len(
            function_args), f'Expected {len(function_args)} arguments, received {num_args}'
        for arg_name, arg_val in zip(function_args, args):
            assert arg_name not in kwargs, f'Specified argument twice: {arg_name}'
            kwargs[arg_name] = arg_val

        numeric_return_values: T.List[T.Union[float, np.ndarray]] = []
        # iterate over all return values and substitute all args (This is inefficient)
        for return_val in symbolic_return_values:
            for arg_name in kwargs:
                symbolic_arg = function_args[arg_name]
                numeric_value = kwargs[arg_name]
                return_val = subs(
                    name=arg_name, output=return_val, symbolic=symbolic_arg, numeric=numeric_value)

            numeric_return_values.append(return_val)

        if len(numeric_return_values) == 1:
            return numeric_return_values[0]
        return tuple(numeric_return_values)

    return evaluator
