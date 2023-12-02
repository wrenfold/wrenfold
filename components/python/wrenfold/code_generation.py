"""Utility functions to support code-generation."""
import dataclasses
import inspect
import typing as T

from . import sym

# Import the C++ code-generation module into this file.
from pywrenfold.wf_wrapper import codegen
from pywrenfold.wf_wrapper.codegen import generate_cpp, generate_rust

AstVariantAnnotation = T.Union[codegen.Add, codegen.AssignTemporary, codegen.AssignOutputArgument,
                               codegen.Branch, codegen.Call, codegen.Cast, codegen.Compare,
                               codegen.ConstructReturnValue, codegen.Declaration,
                               codegen.FloatConstant, codegen.InputValue, codegen.IntegerConstant,
                               codegen.Multiply, codegen.OptionalOutputBranch, codegen.VariableRef,]


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


# Union of ReturnValue and OutputArg
ReturnValueOrOutputArg = T.Union[ReturnValue, OutputArg]

# Things that can be returned from symbolic python functions.
CodegenFuncInvocationResult = T.Union[sym.Expr, sym.MatrixExpr, T.Iterable[ReturnValueOrOutputArg]]


def codegen_function(
    func: T.Callable[..., CodegenFuncInvocationResult],
    name: T.Optional[str] = None
) -> T.Tuple[codegen.FunctionSignature, T.List[AstVariantAnnotation]]:
    """
    Accept a python function that manipulates symbolic mathematical expressions, and convert it
    to an abstract syntax tree (AST) representation suitable for code-generation. After AST
    construction, you can invoke a suitable code-generator:

    >>> def foo(x: RealScalar, y: RealScalar):
    >>>     return [OutputArg(x + y, "z")]
    >>>
    >>> signature, ast = codegen_function(func=foo)
    >>> cpp_code = generate_cpp(signature=signature, ast=ast)

    TODO: Add support saving and emitting the docstring.

    :param func: A python function whose arguments are type-annotated (see type_annotations.py) so
      that we may extract type signatures. The function should return a list of ReturnValue or
      OutputArg objects. These indicate how outputs are passed out of the function.

    :param name: String name of the function.
    :return: A tuple containing two items:
      (1) The `FunctionSignature` object, which describes input and output arguments and their types.
      (2) An iterable of AST types that descibe how the resulting code should be emitted.
    """
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
