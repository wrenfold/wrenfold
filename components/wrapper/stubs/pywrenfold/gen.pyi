from collections.abc import Sequence
import enum
from typing import overload

import pywrenfold.ast
import pywrenfold.sym
import pywrenfold.type_info


class ArgumentDirection(enum.Enum):
    Input = 0
    """Argument is an input."""

    Output = 1
    """Argument is an output."""

    OptionalOutput = 2
    """Argument is an optional output."""

class Argument:
    """Describe an argument to a function."""

    @property
    def name(self) -> str:
        """String name of the argument."""

    @property
    def type(self) -> pywrenfold.type_info.ScalarType | pywrenfold.type_info.MatrixType | pywrenfold.type_info.CustomType:
        """Type of the argument."""

    @property
    def direction(self) -> ArgumentDirection:
        """How the argument is used by the function."""

    @property
    def is_optional(self) -> bool:
        """True if the argument is optional."""

    @property
    def is_input(self) -> bool:
        """True if the function is an input argument."""

    def create_symbolic_input(self) -> pywrenfold.sym.Expr | pywrenfold.sym.MatrixExpr | pywrenfold.sym.CompoundExpr:
        """Create corresponding symbolic input expressions for this argument."""

    def __repr__(self) -> str: ...

class PyExternalFunction:
    """OMIT_FROM_SPHINX"""

    @overload
    def __init__(self, name: str, arguments: Sequence[tuple[str, object]], return_type: object) -> None:
        """Construct with name, arguments, and return type."""

    @overload
    def __init__(self, arg: PyExternalFunction) -> None:
        """Copy constructor."""

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: PyExternalFunction) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: PyExternalFunction) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def name(self) -> str:
        """Name of the function."""

    @property
    def arguments(self) -> list[Argument]:
        """List of arguments."""

    @property
    def num_arguments(self) -> int:
        """Number of arguments the function expects to receive."""

    def arg_position(self, arg: str) -> int | None:
        """Find the position of the argument with the specified name."""

    @property
    def return_type(self) -> pywrenfold.type_info.ScalarType | pywrenfold.type_info.MatrixType | pywrenfold.type_info.CustomType:
        """
        Return type of the function. This will determine the type of variable we must declare in code-generated functions.
        """

    def call(self, args: list) -> pywrenfold.sym.Expr | pywrenfold.sym.MatrixExpr | pywrenfold.sym.CompoundExpr | pywrenfold.sym.BooleanExpr:
        """Call external function and create return expression. OMIT_FROM_SPHINX"""

    def __repr__(self) -> str: ...

class ExpressionUsage(enum.Enum):
    OptionalOutputArgument = 0
    """Value is an optional output argument."""

    OutputArgument = 1
    """Value is a required output argument."""

    ReturnValue = 2
    """Value is the return value."""

class OutputKey:
    def __init__(self, usage: ExpressionUsage, name: str) -> None: ...

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: OutputKey) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: OutputKey) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def usage(self) -> ExpressionUsage:
        """Describe how the output is returned by the function."""

    @property
    def name(self) -> str:
        """Name of the output value."""

    def __repr__(self) -> str: ...

class FunctionDescription:
    """
    Stores information required to emit the function signature, including:
      * The types of input and output values.
      * All the symbolic expressions required to compute the outputs.

    ``FunctionDescription`` may be passed to :func:`wrenfold.code_generation.transpile` in order to
    create a syntax tree representation, which may then be converted into usable code.
    """

    def __init__(self, name: str) -> None:
        """Construct with function name."""

    @property
    def name(self) -> str:
        """Name of the function."""

    @property
    def arguments(self) -> list[Argument]:
        """Arguments to the function."""

    def __repr__(self) -> str: ...

    @overload
    def add_input_argument(self, name: str, type: pywrenfold.type_info.ScalarType) -> pywrenfold.sym.Expr:
        """
        Add a scalar input argument. Returns placeholder value to pass to the python function.
        """

    @overload
    def add_input_argument(self, name: str, type: pywrenfold.type_info.MatrixType) -> pywrenfold.sym.MatrixExpr:
        """
        Add a matrix input argument. Returns placeholder value to pass to the python function.
        """

    @overload
    def add_input_argument(self, name: str, type: pywrenfold.type_info.CustomType) -> pywrenfold.sym.CompoundExpr:
        """Add an input argument with a custom user-specified type."""

    @overload
    def add_output_argument(self, name: str, is_optional: bool, value: pywrenfold.sym.Expr) -> None: ...

    @overload
    def add_output_argument(self, name: str, is_optional: bool, value: pywrenfold.sym.MatrixExpr) -> None:
        """Record an output argument of matrix type."""

    @overload
    def add_output_argument(self, name: str, is_optional: bool, custom_type: pywrenfold.type_info.CustomType, expressions: Sequence[pywrenfold.sym.Expr]) -> None:
        """Record an output argument of custom type."""

    @overload
    def set_return_value(self, value: pywrenfold.sym.Expr) -> None: ...

    @overload
    def set_return_value(self, value: pywrenfold.sym.MatrixExpr) -> None: ...

    @overload
    def set_return_value(self, custom_type: pywrenfold.type_info.CustomType, expressions: Sequence[pywrenfold.sym.Expr]) -> None: ...

    def output_expressions(self) -> dict[OutputKey, pywrenfold.sym.Expr | pywrenfold.sym.MatrixExpr | pywrenfold.sym.CompoundExpr | pywrenfold.sym.BooleanExpr]:
        """Retrieve a dict of output expressions computed by this function."""

class OptimizationParams:
    def __init__(self) -> None:
        """Construct with defaults."""

    @property
    def factorization_passes(self) -> int:
        """
        When non-zero, sum-of-product expressions are factorized. This parameter determines the number of passes through the expression graph.
        """

    @factorization_passes.setter
    def factorization_passes(self, arg: int, /) -> None: ...

    @property
    def binarize_operations(self) -> bool:
        """
        When true, convert n-ary additions and multiplications into binary operations.
        """

    @binarize_operations.setter
    def binarize_operations(self, arg: bool, /) -> None: ...

def transpile(desc: FunctionDescription, optimization_params: OptimizationParams | None = None, convert_ternaries: bool = True) -> pywrenfold.ast.FunctionDefinition:
    """
    Given a :class:`wrenfold.code_generation.FunctionDescription` object, convert it to an abstract
    syntax tree suitable for code generation. This operation incorporates three steps:

      #. The symbolic expression tree is converted to a flat intermediate representation. Common
         subexpression elimination is performed to minimize duplicated operations.
      #. A control flow graph (CFG) is generated.
      #. The CFG is then converted to an abstract syntax tree (AST) that can be emitted as usable code.
         See the :doc:`ast` module for a list of types used in the syntax tree.

    The syntax tree can then be passed to a generator (for example,
    :class:`wrenfold.code_generation.CppGenerator`) to emit compilable code that you can incorporate in
    your project.

    Args:
      desc: :class:`wrenfold.code_generation.FunctionDescription` object.
      optimization_params: Optional parameters to control subexpression elimination.
      convert_ternaries: If true, ternary :func:`wrenfold.sym.where` statements become if-else control
        flow.

    Returns:
      Instance of :class:`wrenfold.ast.FunctionDefinition`.
    """

def cse_function_description(desc: FunctionDescription, params: OptimizationParams | None = None) -> tuple[dict[OutputKey, pywrenfold.sym.Expr | pywrenfold.sym.MatrixExpr | pywrenfold.sym.CompoundExpr | pywrenfold.sym.BooleanExpr], list[tuple[pywrenfold.sym.Expr, pywrenfold.sym.Expr]]]:
    """
    Given a :class:`wrenfold.code_generation.FunctionDescription` object, run the code-generation CSE
    and then convert the simplified result *back* to a list of symbolic expressions. This function will
    apply all the simplifications and subexpression elimination steps normally applied during
    code-generation.

    The first returned object is a dict mapping from ``OutputKey`` to output expressions. The outputs
    will be expressed as a function of variables ``[v0, v1, ... v{N}]``. The second returned object is a
    list of tuples of the form ``[(v0, <v0 expr>), (v1, <v1 expr>), ...]`` - these are the eliminated
    subexpressions required to evaluate the function.

    Args:
      desc: :class:`wrenfold.code_generation.FunctionDescription` object.
      optimization_params: Optional parameters to control subexpression elimination.

    Returns:
      A dict mapping from ``OutputKey`` to output expressions, and a list of intermediate subexpressions
      required to compute the function outputs.

    Example:
      >>> import wrenfold as wf
      >>> from wrenfold import sym
      >>>
      >>> def func(x: wf.FloatScalar, y: wf.FloatScalar):
      >>>   return sym.abs(x * y) * sym.cos(x * y)
      >>> desc = wf.create_function_description(func)
      >>> outputs, intermediate_values = wf.cse_function_description(desc)
      >>> outputs
      {OutputKey(return_value): [v5]}
      >>> intermediate_values
      [(v0, $arg(0, 0)),
       (v1, $arg(1, 0)),
       (v2, v0*v1),
       (v3, abs(v2)),
       (v4, cos(v2)),
       (v5, v3*v4)]
    """

class CppMatrixTypeBehavior(enum.Enum):
    GenericSpan = 0
    """
    Generate functions that accept generic/templated arguments for which a specialization of ``wf::convert_to_span`` is defined.
    """

    Eigen = 1
    """Generate functions that accept and return Eigen::Matrix."""

class CppGenerator:
    """Generates C++ code."""

    def __init__(self, matrix_args: CppMatrixTypeBehavior = CppMatrixTypeBehavior.GenericSpan) -> None: ...

    @overload
    def generate(self, definition: pywrenfold.ast.FunctionDefinition) -> str:
        """Generate code for the provided definition."""

    @overload
    def generate(self, definition: Sequence[pywrenfold.ast.FunctionDefinition]) -> str:
        """Generate code for multiple definitions."""

    @overload
    def format(self, element: pywrenfold.ast.Add) -> str:
        """Format type :class:`wrenfold.ast.Add`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """Format type :class:`wrenfold.ast.AssignTemporary`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputScalar`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputStruct`."""

    @overload
    def format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """Format type :class:`wrenfold.ast.BooleanLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Branch) -> str:
        """Format type :class:`wrenfold.ast.Branch`."""

    @overload
    def format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """Format type :class:`wrenfold.ast.CallExternalFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """Format type :class:`wrenfold.ast.CallStdFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.Cast) -> str:
        """Format type :class:`wrenfold.ast.Cast`."""

    @overload
    def format(self, element: pywrenfold.ast.Comment) -> str:
        """Format type :class:`wrenfold.ast.Comment`."""

    @overload
    def format(self, element: pywrenfold.ast.Compare) -> str:
        """Format type :class:`wrenfold.ast.Compare`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """Format type :class:`wrenfold.ast.ConstructCustomType`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """Format type :class:`wrenfold.ast.ConstructMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.Declaration) -> str:
        """Format type :class:`wrenfold.ast.Declaration`."""

    @overload
    def format(self, element: pywrenfold.ast.Divide) -> str:
        """Format type :class:`wrenfold.ast.Divide`."""

    @overload
    def format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """Format type :class:`wrenfold.ast.FloatLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.GetArgument) -> str:
        """Format type :class:`wrenfold.ast.GetArgument`."""

    @overload
    def format(self, element: pywrenfold.ast.GetField) -> str:
        """Format type :class:`wrenfold.ast.GetField`."""

    @overload
    def format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """Format type :class:`wrenfold.ast.GetMatrixElement`."""

    @overload
    def format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """Format type :class:`wrenfold.ast.IntegerLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Multiply) -> str:
        """Format type :class:`wrenfold.ast.Multiply`."""

    @overload
    def format(self, element: pywrenfold.ast.Negate) -> str:
        """Format type :class:`wrenfold.ast.Negate`."""

    @overload
    def format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """Format type :class:`wrenfold.ast.OptionalOutputBranch`."""

    @overload
    def format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """Format type :class:`wrenfold.ast.Parenthetical`."""

    @overload
    def format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """Format type :class:`wrenfold.ast.SpecialConstant`."""

    @overload
    def format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """Format type :class:`wrenfold.ast.ReturnObject`."""

    @overload
    def format(self, element: pywrenfold.ast.Ternary) -> str:
        """Format type :class:`wrenfold.ast.Ternary`."""

    @overload
    def format(self, element: pywrenfold.ast.VariableRef) -> str:
        """Format type :class:`wrenfold.ast.VariableRef`."""

    @overload
    def format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """Format type :class:`wrenfold.ast.FunctionSignature`."""

    @overload
    def format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """Format type :class:`wrenfold.type_info.ScalarType`."""

    @overload
    def format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """Format type :class:`wrenfold.type_info.MatrixType`."""

    @overload
    def format(self, element: pywrenfold.type_info.CustomType) -> str:
        """Format type :class:`wrenfold.type_info.CustomType`."""

    @overload
    def super_format(self, element: pywrenfold.ast.Add) -> str:
        """
        Format type :class:`wrenfold.ast.Add`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """
        Format type :class:`wrenfold.ast.AssignTemporary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputScalar`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputStruct`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.BooleanLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Branch) -> str:
        """
        Format type :class:`wrenfold.ast.Branch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallExternalFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallStdFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Cast) -> str:
        """
        Format type :class:`wrenfold.ast.Cast`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Comment) -> str:
        """
        Format type :class:`wrenfold.ast.Comment`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Compare) -> str:
        """
        Format type :class:`wrenfold.ast.Compare`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructCustomType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Declaration) -> str:
        """
        Format type :class:`wrenfold.ast.Declaration`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Divide) -> str:
        """
        Format type :class:`wrenfold.ast.Divide`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.FloatLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetArgument) -> str:
        """
        Format type :class:`wrenfold.ast.GetArgument`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetField) -> str:
        """
        Format type :class:`wrenfold.ast.GetField`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """
        Format type :class:`wrenfold.ast.GetMatrixElement`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.IntegerLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Multiply) -> str:
        """
        Format type :class:`wrenfold.ast.Multiply`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Negate) -> str:
        """
        Format type :class:`wrenfold.ast.Negate`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """
        Format type :class:`wrenfold.ast.OptionalOutputBranch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """
        Format type :class:`wrenfold.ast.Parenthetical`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """
        Format type :class:`wrenfold.ast.SpecialConstant`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """
        Format type :class:`wrenfold.ast.ReturnObject`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Ternary) -> str:
        """
        Format type :class:`wrenfold.ast.Ternary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.VariableRef) -> str:
        """
        Format type :class:`wrenfold.ast.VariableRef`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """
        Format type :class:`wrenfold.ast.FunctionSignature`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """
        Format type :class:`wrenfold.type_info.ScalarType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """
        Format type :class:`wrenfold.type_info.MatrixType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.CustomType) -> str:
        """
        Format type :class:`wrenfold.type_info.CustomType`. Invokes the wrapped base class implementation.
        """

    def apply_preamble(self, code: str, namespace: str, imports: str = '') -> str:
        """Apply a preamble that incorporates necessary runtime includes."""

    @property
    def matrix_type_behavior(self) -> CppMatrixTypeBehavior: ...

class RustGenerator:
    """Generates Rust code."""

    def __init__(self) -> None: ...

    @overload
    def generate(self, definition: pywrenfold.ast.FunctionDefinition) -> str:
        """Generate code for the provided definition."""

    @overload
    def generate(self, definition: Sequence[pywrenfold.ast.FunctionDefinition]) -> str:
        """Generate code for multiple definitions."""

    @overload
    def format(self, element: pywrenfold.ast.Add) -> str:
        """Format type :class:`wrenfold.ast.Add`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """Format type :class:`wrenfold.ast.AssignTemporary`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputScalar`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputStruct`."""

    @overload
    def format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """Format type :class:`wrenfold.ast.BooleanLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Branch) -> str:
        """Format type :class:`wrenfold.ast.Branch`."""

    @overload
    def format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """Format type :class:`wrenfold.ast.CallExternalFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """Format type :class:`wrenfold.ast.CallStdFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.Cast) -> str:
        """Format type :class:`wrenfold.ast.Cast`."""

    @overload
    def format(self, element: pywrenfold.ast.Comment) -> str:
        """Format type :class:`wrenfold.ast.Comment`."""

    @overload
    def format(self, element: pywrenfold.ast.Compare) -> str:
        """Format type :class:`wrenfold.ast.Compare`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """Format type :class:`wrenfold.ast.ConstructCustomType`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """Format type :class:`wrenfold.ast.ConstructMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.Declaration) -> str:
        """Format type :class:`wrenfold.ast.Declaration`."""

    @overload
    def format(self, element: pywrenfold.ast.Divide) -> str:
        """Format type :class:`wrenfold.ast.Divide`."""

    @overload
    def format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """Format type :class:`wrenfold.ast.FloatLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.GetArgument) -> str:
        """Format type :class:`wrenfold.ast.GetArgument`."""

    @overload
    def format(self, element: pywrenfold.ast.GetField) -> str:
        """Format type :class:`wrenfold.ast.GetField`."""

    @overload
    def format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """Format type :class:`wrenfold.ast.GetMatrixElement`."""

    @overload
    def format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """Format type :class:`wrenfold.ast.IntegerLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Multiply) -> str:
        """Format type :class:`wrenfold.ast.Multiply`."""

    @overload
    def format(self, element: pywrenfold.ast.Negate) -> str:
        """Format type :class:`wrenfold.ast.Negate`."""

    @overload
    def format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """Format type :class:`wrenfold.ast.OptionalOutputBranch`."""

    @overload
    def format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """Format type :class:`wrenfold.ast.Parenthetical`."""

    @overload
    def format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """Format type :class:`wrenfold.ast.SpecialConstant`."""

    @overload
    def format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """Format type :class:`wrenfold.ast.ReturnObject`."""

    @overload
    def format(self, element: pywrenfold.ast.Ternary) -> str:
        """Format type :class:`wrenfold.ast.Ternary`."""

    @overload
    def format(self, element: pywrenfold.ast.VariableRef) -> str:
        """Format type :class:`wrenfold.ast.VariableRef`."""

    @overload
    def format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """Format type :class:`wrenfold.ast.FunctionSignature`."""

    @overload
    def format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """Format type :class:`wrenfold.type_info.ScalarType`."""

    @overload
    def format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """Format type :class:`wrenfold.type_info.MatrixType`."""

    @overload
    def format(self, element: pywrenfold.type_info.CustomType) -> str:
        """Format type :class:`wrenfold.type_info.CustomType`."""

    @overload
    def super_format(self, element: pywrenfold.ast.Add) -> str:
        """
        Format type :class:`wrenfold.ast.Add`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """
        Format type :class:`wrenfold.ast.AssignTemporary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputScalar`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputStruct`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.BooleanLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Branch) -> str:
        """
        Format type :class:`wrenfold.ast.Branch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallExternalFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallStdFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Cast) -> str:
        """
        Format type :class:`wrenfold.ast.Cast`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Comment) -> str:
        """
        Format type :class:`wrenfold.ast.Comment`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Compare) -> str:
        """
        Format type :class:`wrenfold.ast.Compare`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructCustomType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Declaration) -> str:
        """
        Format type :class:`wrenfold.ast.Declaration`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Divide) -> str:
        """
        Format type :class:`wrenfold.ast.Divide`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.FloatLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetArgument) -> str:
        """
        Format type :class:`wrenfold.ast.GetArgument`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetField) -> str:
        """
        Format type :class:`wrenfold.ast.GetField`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """
        Format type :class:`wrenfold.ast.GetMatrixElement`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.IntegerLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Multiply) -> str:
        """
        Format type :class:`wrenfold.ast.Multiply`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Negate) -> str:
        """
        Format type :class:`wrenfold.ast.Negate`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """
        Format type :class:`wrenfold.ast.OptionalOutputBranch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """
        Format type :class:`wrenfold.ast.Parenthetical`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """
        Format type :class:`wrenfold.ast.SpecialConstant`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """
        Format type :class:`wrenfold.ast.ReturnObject`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Ternary) -> str:
        """
        Format type :class:`wrenfold.ast.Ternary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.VariableRef) -> str:
        """
        Format type :class:`wrenfold.ast.VariableRef`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """
        Format type :class:`wrenfold.ast.FunctionSignature`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """
        Format type :class:`wrenfold.type_info.ScalarType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """
        Format type :class:`wrenfold.type_info.MatrixType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.CustomType) -> str:
        """
        Format type :class:`wrenfold.type_info.CustomType`. Invokes the wrapped base class implementation.
        """

    @staticmethod
    def apply_preamble(code: str) -> str:
        """Apply a preamble to generated code."""

class BaseGenerator:
    """
    Abstract base class for generators. The user may inherit from this in python when writing a new generator from scratch.
    """

    def __init__(self) -> None: ...

    @overload
    def generate(self, definition: pywrenfold.ast.FunctionDefinition) -> str:
        """Generate code for the provided definition."""

    @overload
    def generate(self, definition: Sequence[pywrenfold.ast.FunctionDefinition]) -> str:
        """Generate code for multiple definitions."""

    @overload
    def format(self, element: pywrenfold.ast.Add) -> str:
        """Format type :class:`wrenfold.ast.Add`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """Format type :class:`wrenfold.ast.AssignTemporary`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputScalar`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputStruct`."""

    @overload
    def format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """Format type :class:`wrenfold.ast.BooleanLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Branch) -> str:
        """Format type :class:`wrenfold.ast.Branch`."""

    @overload
    def format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """Format type :class:`wrenfold.ast.CallExternalFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """Format type :class:`wrenfold.ast.CallStdFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.Cast) -> str:
        """Format type :class:`wrenfold.ast.Cast`."""

    @overload
    def format(self, element: pywrenfold.ast.Comment) -> str:
        """Format type :class:`wrenfold.ast.Comment`."""

    @overload
    def format(self, element: pywrenfold.ast.Compare) -> str:
        """Format type :class:`wrenfold.ast.Compare`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """Format type :class:`wrenfold.ast.ConstructCustomType`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """Format type :class:`wrenfold.ast.ConstructMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.Declaration) -> str:
        """Format type :class:`wrenfold.ast.Declaration`."""

    @overload
    def format(self, element: pywrenfold.ast.Divide) -> str:
        """Format type :class:`wrenfold.ast.Divide`."""

    @overload
    def format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """Format type :class:`wrenfold.ast.FloatLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.GetArgument) -> str:
        """Format type :class:`wrenfold.ast.GetArgument`."""

    @overload
    def format(self, element: pywrenfold.ast.GetField) -> str:
        """Format type :class:`wrenfold.ast.GetField`."""

    @overload
    def format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """Format type :class:`wrenfold.ast.GetMatrixElement`."""

    @overload
    def format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """Format type :class:`wrenfold.ast.IntegerLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Multiply) -> str:
        """Format type :class:`wrenfold.ast.Multiply`."""

    @overload
    def format(self, element: pywrenfold.ast.Negate) -> str:
        """Format type :class:`wrenfold.ast.Negate`."""

    @overload
    def format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """Format type :class:`wrenfold.ast.OptionalOutputBranch`."""

    @overload
    def format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """Format type :class:`wrenfold.ast.Parenthetical`."""

    @overload
    def format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """Format type :class:`wrenfold.ast.SpecialConstant`."""

    @overload
    def format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """Format type :class:`wrenfold.ast.ReturnObject`."""

    @overload
    def format(self, element: pywrenfold.ast.Ternary) -> str:
        """Format type :class:`wrenfold.ast.Ternary`."""

    @overload
    def format(self, element: pywrenfold.ast.VariableRef) -> str:
        """Format type :class:`wrenfold.ast.VariableRef`."""

    @overload
    def format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """Format type :class:`wrenfold.ast.FunctionSignature`."""

    @overload
    def format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """Format type :class:`wrenfold.type_info.ScalarType`."""

    @overload
    def format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """Format type :class:`wrenfold.type_info.MatrixType`."""

    @overload
    def format(self, element: pywrenfold.type_info.CustomType) -> str:
        """Format type :class:`wrenfold.type_info.CustomType`."""

    @overload
    def super_format(self, element: pywrenfold.ast.Add) -> str:
        """
        Format type :class:`wrenfold.ast.Add`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """
        Format type :class:`wrenfold.ast.AssignTemporary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputScalar`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputStruct`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.BooleanLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Branch) -> str:
        """
        Format type :class:`wrenfold.ast.Branch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallExternalFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallStdFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Cast) -> str:
        """
        Format type :class:`wrenfold.ast.Cast`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Comment) -> str:
        """
        Format type :class:`wrenfold.ast.Comment`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Compare) -> str:
        """
        Format type :class:`wrenfold.ast.Compare`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructCustomType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Declaration) -> str:
        """
        Format type :class:`wrenfold.ast.Declaration`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Divide) -> str:
        """
        Format type :class:`wrenfold.ast.Divide`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.FloatLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetArgument) -> str:
        """
        Format type :class:`wrenfold.ast.GetArgument`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetField) -> str:
        """
        Format type :class:`wrenfold.ast.GetField`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """
        Format type :class:`wrenfold.ast.GetMatrixElement`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.IntegerLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Multiply) -> str:
        """
        Format type :class:`wrenfold.ast.Multiply`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Negate) -> str:
        """
        Format type :class:`wrenfold.ast.Negate`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """
        Format type :class:`wrenfold.ast.OptionalOutputBranch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """
        Format type :class:`wrenfold.ast.Parenthetical`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """
        Format type :class:`wrenfold.ast.SpecialConstant`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """
        Format type :class:`wrenfold.ast.ReturnObject`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Ternary) -> str:
        """
        Format type :class:`wrenfold.ast.Ternary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.VariableRef) -> str:
        """
        Format type :class:`wrenfold.ast.VariableRef`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """
        Format type :class:`wrenfold.ast.FunctionSignature`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """
        Format type :class:`wrenfold.type_info.ScalarType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """
        Format type :class:`wrenfold.type_info.MatrixType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.CustomType) -> str:
        """
        Format type :class:`wrenfold.type_info.CustomType`. Invokes the wrapped base class implementation.
        """

class PythonGeneratorTarget(enum.Enum):
    NumPy = 0
    """Target the NumPy API."""

    PyTorch = 1
    """Target the PyTorch API."""

    JAX = 2
    """Target the JAX API."""

class PythonGeneratorFloatWidth(enum.Enum):
    Float32 = 0
    """Float arrays/tensors will be interpreted as float32."""

    Float64 = 1
    """Float arrays/tensors will be interpreted as float64."""

class PythonGenerator:
    """Generates Python code. Can target NumPy, PyTorch, or JAX."""

    def __init__(self, target: PythonGeneratorTarget = PythonGeneratorTarget.NumPy, float_width: PythonGeneratorFloatWidth = PythonGeneratorFloatWidth.Float64, indentation: int = 4, use_output_arguments: bool = False) -> None:
        """
        Construct a python code generator.

        Args:
          target: Which python API to utilize.
          float_width: Precision of floating point to enforce throughout generated code.
          indentation: Number of spaces of indentation to apply. Typically 2 or 4.
          use_output_arguments: By default, output arguments are emitted as return values when generating
            Python. When ``use_output_arguments=True``, matrix-type output arguments will become actual
            output arguments in the generated code. Optional outputs will have type ``np.ndarray | None``.
            This mode is only supported with ``target=NumPy``, and is untested in other configurations.
        """

    @overload
    def generate(self, definition: pywrenfold.ast.FunctionDefinition) -> str:
        """Generate code for the provided definition."""

    @overload
    def generate(self, definition: Sequence[pywrenfold.ast.FunctionDefinition]) -> str:
        """Generate code for multiple definitions."""

    @overload
    def format(self, element: pywrenfold.ast.Add) -> str:
        """Format type :class:`wrenfold.ast.Add`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """Format type :class:`wrenfold.ast.AssignTemporary`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputScalar`."""

    @overload
    def format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """Format type :class:`wrenfold.ast.AssignOutputStruct`."""

    @overload
    def format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """Format type :class:`wrenfold.ast.BooleanLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Branch) -> str:
        """Format type :class:`wrenfold.ast.Branch`."""

    @overload
    def format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """Format type :class:`wrenfold.ast.CallExternalFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """Format type :class:`wrenfold.ast.CallStdFunction`."""

    @overload
    def format(self, element: pywrenfold.ast.Cast) -> str:
        """Format type :class:`wrenfold.ast.Cast`."""

    @overload
    def format(self, element: pywrenfold.ast.Comment) -> str:
        """Format type :class:`wrenfold.ast.Comment`."""

    @overload
    def format(self, element: pywrenfold.ast.Compare) -> str:
        """Format type :class:`wrenfold.ast.Compare`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """Format type :class:`wrenfold.ast.ConstructCustomType`."""

    @overload
    def format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """Format type :class:`wrenfold.ast.ConstructMatrix`."""

    @overload
    def format(self, element: pywrenfold.ast.Declaration) -> str:
        """Format type :class:`wrenfold.ast.Declaration`."""

    @overload
    def format(self, element: pywrenfold.ast.Divide) -> str:
        """Format type :class:`wrenfold.ast.Divide`."""

    @overload
    def format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """Format type :class:`wrenfold.ast.FloatLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.GetArgument) -> str:
        """Format type :class:`wrenfold.ast.GetArgument`."""

    @overload
    def format(self, element: pywrenfold.ast.GetField) -> str:
        """Format type :class:`wrenfold.ast.GetField`."""

    @overload
    def format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """Format type :class:`wrenfold.ast.GetMatrixElement`."""

    @overload
    def format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """Format type :class:`wrenfold.ast.IntegerLiteral`."""

    @overload
    def format(self, element: pywrenfold.ast.Multiply) -> str:
        """Format type :class:`wrenfold.ast.Multiply`."""

    @overload
    def format(self, element: pywrenfold.ast.Negate) -> str:
        """Format type :class:`wrenfold.ast.Negate`."""

    @overload
    def format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """Format type :class:`wrenfold.ast.OptionalOutputBranch`."""

    @overload
    def format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """Format type :class:`wrenfold.ast.Parenthetical`."""

    @overload
    def format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """Format type :class:`wrenfold.ast.SpecialConstant`."""

    @overload
    def format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """Format type :class:`wrenfold.ast.ReturnObject`."""

    @overload
    def format(self, element: pywrenfold.ast.Ternary) -> str:
        """Format type :class:`wrenfold.ast.Ternary`."""

    @overload
    def format(self, element: pywrenfold.ast.VariableRef) -> str:
        """Format type :class:`wrenfold.ast.VariableRef`."""

    @overload
    def format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """Format type :class:`wrenfold.ast.FunctionSignature`."""

    @overload
    def format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """Format type :class:`wrenfold.type_info.ScalarType`."""

    @overload
    def format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """Format type :class:`wrenfold.type_info.MatrixType`."""

    @overload
    def format(self, element: pywrenfold.type_info.CustomType) -> str:
        """Format type :class:`wrenfold.type_info.CustomType`."""

    @overload
    def super_format(self, element: pywrenfold.ast.Add) -> str:
        """
        Format type :class:`wrenfold.ast.Add`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignTemporary) -> str:
        """
        Format type :class:`wrenfold.ast.AssignTemporary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputScalar) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputScalar`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.AssignOutputStruct) -> str:
        """
        Format type :class:`wrenfold.ast.AssignOutputStruct`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.BooleanLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.BooleanLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Branch) -> str:
        """
        Format type :class:`wrenfold.ast.Branch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallExternalFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallExternalFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.CallStdFunction) -> str:
        """
        Format type :class:`wrenfold.ast.CallStdFunction`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Cast) -> str:
        """
        Format type :class:`wrenfold.ast.Cast`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Comment) -> str:
        """
        Format type :class:`wrenfold.ast.Comment`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Compare) -> str:
        """
        Format type :class:`wrenfold.ast.Compare`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructCustomType) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructCustomType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ConstructMatrix) -> str:
        """
        Format type :class:`wrenfold.ast.ConstructMatrix`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Declaration) -> str:
        """
        Format type :class:`wrenfold.ast.Declaration`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Divide) -> str:
        """
        Format type :class:`wrenfold.ast.Divide`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FloatLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.FloatLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetArgument) -> str:
        """
        Format type :class:`wrenfold.ast.GetArgument`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetField) -> str:
        """
        Format type :class:`wrenfold.ast.GetField`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.GetMatrixElement) -> str:
        """
        Format type :class:`wrenfold.ast.GetMatrixElement`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.IntegerLiteral) -> str:
        """
        Format type :class:`wrenfold.ast.IntegerLiteral`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Multiply) -> str:
        """
        Format type :class:`wrenfold.ast.Multiply`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Negate) -> str:
        """
        Format type :class:`wrenfold.ast.Negate`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.OptionalOutputBranch) -> str:
        """
        Format type :class:`wrenfold.ast.OptionalOutputBranch`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Parenthetical) -> str:
        """
        Format type :class:`wrenfold.ast.Parenthetical`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.SpecialConstant) -> str:
        """
        Format type :class:`wrenfold.ast.SpecialConstant`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.ReturnObject) -> str:
        """
        Format type :class:`wrenfold.ast.ReturnObject`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.Ternary) -> str:
        """
        Format type :class:`wrenfold.ast.Ternary`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.VariableRef) -> str:
        """
        Format type :class:`wrenfold.ast.VariableRef`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.ast.FunctionSignature) -> str:
        """
        Format type :class:`wrenfold.ast.FunctionSignature`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.ScalarType) -> str:
        """
        Format type :class:`wrenfold.type_info.ScalarType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.MatrixType) -> str:
        """
        Format type :class:`wrenfold.type_info.MatrixType`. Invokes the wrapped base class implementation.
        """

    @overload
    def super_format(self, element: pywrenfold.type_info.CustomType) -> str:
        """
        Format type :class:`wrenfold.type_info.CustomType`. Invokes the wrapped base class implementation.
        """

    @property
    def target(self) -> PythonGeneratorTarget:
        """The API that the python generator targets."""

    @property
    def float_width(self) -> PythonGeneratorFloatWidth:
        """Float precision applied to all NumPy arrays and tensors."""

    @property
    def indentation(self) -> int:
        """Amount of spaces used to indent nested scopes."""

    def apply_preamble(self, code: str) -> str:
        """Apply a preamble to generated code."""
