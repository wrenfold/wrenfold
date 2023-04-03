"""Utility functions to support code-generation."""
import inspect
import itertools
import typing as T

from . import sym
from sym_wrapper import pycodegen as codegen

AstVariantTuple = (codegen.Add, codegen.Assignment, codegen.Call, codegen.Declaration,
                   codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                   codegen.Multiply, codegen.OutputBlock, codegen.ReturnValueBlock,
                   codegen.VariableRef)

AstTypeTuple = (codegen.ScalarType, codegen.MatrixType)

AstVariantAnnotation = T.Union[codegen.Add, codegen.Assignment, codegen.Call, codegen.Declaration,
                               codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                               codegen.Multiply, codegen.OutputBlock, codegen.ReturnValueBlock,
                               codegen.VariableRef]


class CodeGenerator:

    def __init__(self) -> None:
        self._type_to_formatter: T.Dict[type, T.Callable] = dict()

    def _get_formatter(self, arg_type: T.Type) -> T.Callable:
        if arg_type not in self._type_to_formatter:
            format_method_name = f'format_{arg_type.__name__}'
            if not hasattr(self, format_method_name):
                raise KeyError(f"Code generator is missing formatting method: {format_method_name}")
            self._type_to_formatter[arg_type] = getattr(self, format_method_name)
        return self._type_to_formatter[arg_type]

    def _format_ast_element(
            self, arg: T.Union[AstVariantAnnotation, codegen.ScalarType,
                               codegen.MatrixType]) -> str:
        formatter = self._get_formatter(type(arg))
        return formatter(arg)

    def _maybe_format_ast_type(self, arg: T.Any) -> T.Any:
        if not isinstance(arg, AstVariantTuple) and not isinstance(arg, AstTypeTuple):
            return arg
        return self._format_ast_element(arg)

    def format(self, fmt: str, *args, **kwargs) -> str:
        return fmt.format(*[self._maybe_format_ast_type(x) for x in args],
                          **{k: self._maybe_format_ast_type(v) for (k, v) in kwargs.items()})

    def generate(self, ast: T.List[AstVariantAnnotation]) -> str:
        result = str()
        for element in ast:
            result += self._format_ast_element(arg=element)
        return result


class PythonCodeGenerator(CodeGenerator):

    def __init__(self) -> None:
        super(PythonCodeGenerator, self).__init__()

    def format_ScalarType(self, x: codegen.ScalarType) -> str:
        return 'float'

    def format_MatrixType(self, x: codegen.MatrixType) -> str:
        return 'np.ndarray'

    def format_Add(self, x: codegen.Add) -> str:
        """Format one of the AST types to python."""
        return self.format('{} + {}', x.left, x.right)

    def format_Assignment(self, x: codegen.Assignment) -> str:
        return self.format('{} = {}', x.left, x.right)

    def format_Call(self, x: codegen.Call) -> str:
        if isinstance(x.function, codegen.BinaryFunctionName):
            if x.function == codegen.BinaryFunctionName.Pow:
                return self.format('np.pow({}, {})', x.args[0], x.args[1])
        elif isinstance(x.function, codegen.UnaryFunctionName):
            pass

        raise KeyError(f'Unsupported function name: {x.function}')

    def format_Declaration(self, x: codegen.Declaration) -> str:
        """Format declaration."""
        return self.format('{name}: {type} = {value}', name=x.name, type=x.type, value=x.value)

    def format_FloatConstant(self, x: codegen.FloatConstant) -> str:
        return '{:.16}'.format(x.value)

    def format_InputValue(self, x: codegen.InputValue) -> str:
        argument = x.argument
        if isinstance(argument.type, codegen.ScalarType):
            return argument.name
        elif isinstance(argument.type, codegen.MatrixType):
            return '{}[{}, {}]'.format(argument.name, *argument.type.compute_indices(x.element))
        else:
            raise TypeError(f'Unsupported type: {argument.type}')

    def format_IntegerConstant(self, x: codegen.IntegerConstant) -> str:
        return str(x.value)

    def format_Multiply(self, x: codegen.Multiply) -> str:
        return self.format('{} * {}', x.left, x.right)

    def format_OutputBlock(self, x: codegen.OutputBlock) -> str:
        import ipdb
        ipdb.set_trace()

    def format_ReturnValueBlock(self, x: codegen.ReturnValueBlock) -> str:
        import ipdb
        ipdb.set_trace()

    def format_VariableRef(self, x: codegen.VariableRef) -> str:
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
    return ast
