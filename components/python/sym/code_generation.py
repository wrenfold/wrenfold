"""Utility functions to support code-generation."""
import collections
import dataclasses
import inspect
import string
import typing as T

import numpy as np

from . import sym
from . import codegen

AstVariantTuple = (codegen.Add, codegen.AssignTemporary, codegen.AssignOutputArgument,
                   codegen.Branch, codegen.Call, codegen.Cast, codegen.Compare,
                   codegen.ConstructReturnValue, codegen.Declaration, codegen.FloatConstant,
                   codegen.InputValue, codegen.IntegerConstant, codegen.Multiply,
                   codegen.OptionalOutputBranch, codegen.VariableRef, codegen.FunctionSignature)

AstTypeTuple = (codegen.ScalarType, codegen.MatrixType)

AstVariantAnnotation = T.Union[codegen.Add, codegen.AssignTemporary, codegen.AssignOutputArgument,
                               codegen.Branch, codegen.Call, codegen.Cast, codegen.Compare,
                               codegen.ConstructReturnValue, codegen.Declaration,
                               codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                               codegen.Multiply, codegen.OptionalOutputBranch, codegen.VariableRef,]


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
    def indent(input_string: str, amount: int = 2) -> str:
        """Indent the provided string by `amount` spaces."""
        assert amount >= 0, f'amount = {amount}'
        return (' ' * amount) + input_string.replace('\n', '\n' + ' ' * amount).rstrip(' ')


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
        for arg in signature.arguments:
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
        if x.numeric_type == codegen.NumericType.Bool:
            return 'bool'
        elif x.numeric_type == codegen.NumericType.Integer:
            return 'int'
        elif x.numeric_type == codegen.NumericType.Real:
            return 'float'
        elif x.numeric_type == codegen.NumericType.Complex:
            raise TypeError("Complex is not supported here")

    def format_MatrixType(self, fmt: CustomStringFormatter, x: codegen.MatrixType) -> str:
        return 'np.ndarray'

    def format_Add(self, fmt: CustomStringFormatter, x: codegen.Add) -> str:
        """Format one of the AST types to python."""
        return fmt.format('{} + {}', x.left, x.right)

    def format_AssignTemporary(self, fmt: CustomStringFormatter, x: codegen.AssignTemporary) -> str:
        return fmt.format('{} = {}', x.left, x.right)

    def format_Branch(self, fmt: CustomStringFormatter, x: codegen.Branch) -> str:
        result = fmt.format('if {}:\n', x.condition) + fmt.indent('\n'.join(
            fmt.format_ast(v) for v in x.if_branch))
        if len(x.else_branch) > 0:
            result += '\nelse:\n' + fmt.indent('\n'.join(fmt.format_ast(v) for v in x.else_branch))
        return result

    def format_Call(self, fmt: CustomStringFormatter, x: codegen.Call) -> str:
        funcs = {
            codegen.StandardLibraryMathFunction.Cos: "np.cos",
            codegen.StandardLibraryMathFunction.Sin: "np.sin",
            codegen.StandardLibraryMathFunction.Log: "np.log",
            codegen.StandardLibraryMathFunction.Sqrt: "np.sqrt",
            codegen.StandardLibraryMathFunction.Tan: "np.tan",
            codegen.StandardLibraryMathFunction.Powi: "np.power",
            codegen.StandardLibraryMathFunction.Powf: "np.power",
            codegen.StandardLibraryMathFunction.Arctan2: "np.atan2"
        }
        return fmt.format("{}({})", funcs[x.function], ', '.join(fmt.format_ast(v) for v in x.args))

    def format_Cast(self, fmt: CustomStringFormatter, x: codegen.Cast) -> str:
        if x.destination_type == codegen.NumericType.Bool:
            type_name = 'bool'
        elif x.destination_type == codegen.NumericType.Integer:
            type_name = 'int'
        elif x.destination_type == codegen.NumericType.Real:
            type_name = 'float'
        elif x.destination_type == codegen.NumericType.Complex:
            raise TypeError("Complex is not supported here")
        else:
            type_name = f'<INVALID ENUM VALUE: {x.destination_type}>'
        return fmt.format("{}({})", type_name, x.arg)

    def format_Compare(self, fmt: CustomStringFormatter, x: codegen.Compare) -> str:
        if x.operation == codegen.RelationalOperation.LessThan:
            op = '<'
        elif x.operation == codegen.RelationalOperation.LessThanOrEqual:
            op = '<='
        else:
            assert x.operation == codegen.RelationalOperation.Equal
            op = '=='
        return fmt.format('{} {} {}', x.left, op, x.right)

    def format_ConstructReturnValue(self, fmt: CustomStringFormatter,
                                    x: codegen.ConstructReturnValue) -> str:
        values = x.args
        if len(values) > 1:
            return fmt.format('return {}', values[0])
        else:
            joined = ', '.join(fmt.format_ast(v) for v in values)
            return f'return {joined}'

    def format_Declaration(self, fmt: CustomStringFormatter, x: codegen.Declaration) -> str:
        """Format declaration."""
        if x.value is not None:
            return fmt.format('{name}: {type} = {value}', name=x.name, type=x.type, value=x.value)
        else:
            return str()

    def format_FloatConstant(self, fmt: CustomStringFormatter, x: codegen.FloatConstant) -> str:
        return '{:.16}'.format(x.value)

    def format_FunctionSignature(self, fmt: CustomStringFormatter,
                                 x: codegen.FunctionSignature) -> str:
        args = []
        for arg in x.arguments:
            if arg.is_optional:
                args.append(fmt.format('{}: T.Optional[{}]', arg.name, arg.type))
            else:
                args.append(fmt.format('{}: {}', arg.name, arg.type))

        return_type = x.return_type
        if return_type is None:
            return_type_str = 'None'
        else:
            return_type_str = fmt.format('{}', return_type)

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

    def format_OptionalOutputBranch(self, fmt: CustomStringFormatter,
                                    x: codegen.OptionalOutputBranch) -> str:
        import ipdb
        ipdb.set_trace()
        assert x.argument.is_optional, 'Argument must be optional'

        result = fmt.format('if {} is not None:\n', x.argument.name) + fmt.indent('\n'.join(
            fmt.format_ast(v) for v in x.statements))
        return result

    def format_VariableRef(self, fmt: CustomStringFormatter, x: codegen.VariableRef) -> str:
        return x.name


@dataclasses.dataclass
class ReturnValue:
    """Designate a return value in the result of symbolic function invocation."""
    expression: T.Union[sym.Expr, sym.MatrixExpr]


@dataclasses.dataclass
class OutputArg:
    """Designate an output argument in the result of a symbolic function invocation."""
    expression: T.Union[sym.Expr, sym.MatrixExpr]
    name: str
    is_optional: bool = False


ReturnValueOrOutputArg = T.Union[ReturnValue, OutputArg]

CodegenFuncInvocationResult = T.Union[sym.Expr, sym.MatrixExpr, T.Iterable[ReturnValueOrOutputArg]]


def codegen_function(
    func: T.Callable[..., CodegenFuncInvocationResult],
    name: T.Optional[str] = None
) -> T.Tuple[codegen.FunctionSignature, T.List[AstVariantAnnotation]]:
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

    # add the input arguments:
    signature = codegen.FunctionSignature(name or func.__name__)
    for arg_index, arg_name in enumerate(spec.args):
        type_annotation = spec.annotations[arg_name]
        if issubclass(type_annotation, sym.Expr):
            signature.add_argument(
                name=arg_name,
                type=codegen.ScalarType(codegen.NumericType.Real),
                direction=codegen.ArgumentDirection.Input)
        elif issubclass(type_annotation, sym.MatrixExpr):
            signature.add_argument(
                name=arg_name,
                type=codegen.MatrixType(*type_annotation.SHAPE),
                direction=codegen.ArgumentDirection.Input)

    # run the function
    result_expressions: CodegenFuncInvocationResult = func(**kwargs)
    if isinstance(result_expressions, (sym.Expr, sym.MatrixExpr)):
        # if one thing was returned, interpret it as the return value:
        result_expressions = (ReturnValue(expression=result_expressions),)

    if len([val for val in result_expressions if isinstance(val, ReturnValue)]) > 1:
        raise RuntimeError("Only one ReturnValue is permitted.")

    output_expressions: T.List[codegen.ExpressionGroup] = []
    for val in result_expressions:
        if not isinstance(val, (ReturnValue, OutputArg)):
            raise TypeError(f"Returned values must be: {ReturnValueOrOutputArg}, got: {type(val)}")

        if isinstance(val.expression, sym.Expr):
            output_type = codegen.ScalarType(codegen.NumericType.Real)
            group_expressions = [val.expression]
        else:
            assert isinstance(val.expression, sym.MatrixExpr), "Expected MatrixExpr"
            output_type = codegen.MatrixType(*val.expression.shape)
            group_expressions = val.expression  # pass matrix directly

        if isinstance(val, ReturnValue):
            output = codegen.ExpressionGroup(
                expressions=group_expressions,
                output_key=codegen.OutputKey(codegen.ExpressionUsage.ReturnValue, ""))
            # set the function signature return type
            signature.set_return_type(output_type)
        else:
            usage = codegen.ExpressionUsage.OptionalOutputArgument if val.is_optional else \
                codegen.ExpressionUsage.OutputArgument
            output = codegen.ExpressionGroup(
                group_expressions, output_key=codegen.OutputKey(usage, val.name))
            # add an argument:
            signature.add_argument(
                name=val.name,
                type=output_type,
                direction=codegen.ArgumentDirection.OptionalOutput
                if val.is_optional else codegen.ArgumentDirection.Output)

        output_expressions.append(output)

    # Convert to ast that we can emit:
    ast = codegen.generate_func(signature=signature, expressions=output_expressions)
    return signature, ast


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
        for func_arg_name, arg_val in zip(function_args, args):
            assert func_arg_name not in kwargs, f'Specified argument twice: {func_arg_name}'
            kwargs[func_arg_name] = arg_val

        numeric_return_values: T.List[T.Union[float, np.ndarray]] = []
        # iterate over all return values and substitute all args (This is inefficient)
        for return_val in symbolic_return_values:
            for func_arg_name in kwargs:
                symbolic_arg = function_args[func_arg_name]
                numeric_value = kwargs[func_arg_name]
                return_val = subs(
                    name=func_arg_name,
                    output=return_val,
                    symbolic=symbolic_arg,
                    numeric=numeric_value)

            numeric_return_values.append(return_val)

        if len(numeric_return_values) == 1:
            return numeric_return_values[0]
        return tuple(numeric_return_values)

    return evaluator
