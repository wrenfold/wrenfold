from collections.abc import Iterator

import pywrenfold.enumerations
import pywrenfold.gen
import pywrenfold.type_info


class AstSpan:
    """Stores a sequence of AST elements."""

    def __repr__(self) -> str: ...

    def __len__(self) -> int: ...

    def __iter__(self) -> Iterator[Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef]: ...

    def __getitem__(self, index: int) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Array access operator."""

class Add:
    """Addition operation: ``args[0] + args[1] + ...``"""

    def __repr__(self) -> str: ...

    @property
    def args(self) -> AstSpan:
        """Operands to the addition. There will always be more than one element."""

class AssignTemporary:
    """Assignment to a temporary variable: ``left = right``"""

    def __repr__(self) -> str: ...

    @property
    def left(self) -> str:
        """Name of the variable to which the assignment applies."""

    @property
    def right(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """The value being assigned."""

class AssignOutputMatrix:
    """Assign a matrix to an output argument."""

    def __repr__(self) -> str: ...

    @property
    def arg(self) -> pywrenfold.gen.Argument:
        """Destination argument."""

    @property
    def value(self) -> ConstructMatrix:
        """``ConstructMatrix`` specifying values to assign."""

class AssignOutputScalar:
    """Assign a scalar to an output argument."""

    def __repr__(self) -> str: ...

    @property
    def arg(self) -> pywrenfold.gen.Argument:
        """Destination argument."""

    @property
    def value(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Scalar value to assign."""

class AssignOutputStruct:
    """Assign a struct to an output argument."""

    def __repr__(self) -> str: ...

    @property
    def arg(self) -> pywrenfold.gen.Argument:
        """Destination argument."""

    @property
    def value(self) -> ConstructCustomType:
        """``ConstructCustomType`` specifying values to assign."""

class BooleanLiteral:
    """Emit a boolean literal constant."""

    def __repr__(self) -> str: ...

    @property
    def value(self) -> bool:
        """Value of the constant (True or False)."""

class Branch:
    """Emit an if-else statement: ``if (condition) { ... } else { ... }``"""

    def __repr__(self) -> str: ...

    @property
    def condition(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Condition governing which branch to take."""

    @property
    def if_branch(self) -> AstSpan:
        """Statements that evaluate when the condition is true."""

    @property
    def else_branch(self) -> AstSpan:
        """Statements that evaluate when the condition is false."""

class CallExternalFunction:
    """Invoke a user-provided external function."""

    def __repr__(self) -> str: ...

    @property
    def function(self) -> pywrenfold.gen.PyExternalFunction: ...

    @property
    def args(self) -> AstSpan: ...

class CallStdFunction:
    """Invoke a standard library math function."""

    def __repr__(self) -> str: ...

    @property
    def function(self) -> pywrenfold.enumerations.StdMathFunction:
        """The function being invoked."""

    @property
    def args(self) -> AstSpan:
        """Arguments to the function."""

class Cast:
    """Cast a numerical value."""

    def __repr__(self) -> str: ...

    @property
    def destination_type(self) -> pywrenfold.type_info.NumericType:
        """The destination numerical type."""

    @property
    def arg(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Source value being casted."""

class Comment:
    """Emit a comment block."""

    def __repr__(self) -> str: ...

    @property
    def content(self) -> str:
        """Comment as a single string."""

    def split_lines(self) -> list[str]:
        """Split comment by newlines and return a list of strings, one per line."""

class Compare:
    """Compare two operands."""

    def __repr__(self) -> str: ...

    @property
    def left(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """The left operand."""

    @property
    def right(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """The right operand."""

    @property
    def operation(self) -> pywrenfold.enumerations.RelationalOperation:
        """Relational operation."""

class ConstructCustomType:
    """Construct an instance of a user-provided type."""

    def __repr__(self) -> str: ...

    @property
    def type(self) -> pywrenfold.type_info.CustomType:
        """
        Instance of :class:`wrenfold.codegen.CustomType` specifying which type to instantiate.
        """

    def get_field_value(self, name: str) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef | None:
        """
        Given the name of a field, return the statement being assigned to it (or None if the field does not exist).
        """

class ConstructMatrix:
    """Construct a matrix from a list of statements."""

    def __repr__(self) -> str: ...

    @property
    def type(self) -> pywrenfold.type_info.MatrixType:
        """Describe dimensions of the matrix."""

    @property
    def args(self) -> AstSpan:
        """Contents of the matrix, in row-major order."""

class Declaration:
    """
    Declare a variable, and optionally assign it a value: ``name: type = value``
    """

    def __repr__(self) -> str: ...

    @property
    def name(self) -> str:
        """Name of the variable."""

    @property
    def type(self) -> pywrenfold.type_info.ScalarType | pywrenfold.type_info.MatrixType | pywrenfold.type_info.CustomType:
        """Type of the variable."""

    @property
    def value(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef | None:
        """Optional value with which the variable should be initialized."""

class Divide:
    """Division operation: ``left / right``"""

    def __repr__(self) -> str: ...

    @property
    def left(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Left operand (numerator)."""

    @property
    def right(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Right operand (denominator)."""

class FloatLiteral:
    """Emit a floating-point literal constant."""

    def __repr__(self) -> str: ...

    @property
    def value(self) -> float:
        """Value of the constant."""

class GetArgument:
    """Reference an argument to the generated function."""

    def __repr__(self) -> str: ...

    @property
    def argument(self) -> pywrenfold.gen.Argument:
        """Argument being accessed."""

class GetField:
    """Reference a field on a struct: ``arg.field_name``"""

    def __repr__(self) -> str: ...

    @property
    def arg(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Operand from which we wish to retrieve the specified field."""

    @property
    def struct_type(self) -> pywrenfold.type_info.CustomType:
        """Type of the struct."""

    @property
    def field_name(self) -> str:
        """Name of the field being accessed."""

class GetMatrixElement:
    """Retrieve a value from a matrix: ``arg[row, col]``"""

    def __repr__(self) -> str: ...

    @property
    def arg(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Operand matrix."""

    @property
    def row(self) -> int:
        """Row to access."""

    @property
    def col(self) -> int:
        """Column to access."""

class IntegerLiteral:
    """Emit an integer literal constant."""

    def __repr__(self) -> str: ...

    @property
    def value(self) -> int:
        """Value of the constant."""

class Multiply:
    """Multiplication operation: ``args[0] * args[1] * ...``"""

    def __repr__(self) -> str: ...

    @property
    def args(self) -> AstSpan:
        """Operands to the multiplication. There will always be more than one."""

class Negate:
    """Negation operation: ``-arg``"""

    def __repr__(self) -> str: ...

    @property
    def arg(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Operand being negated."""

class OptionalOutputBranch:
    """
    Conditionally assign values to an optional output argument: ``if (<argument exists>) { ... }``
    """

    def __repr__(self) -> str: ...

    @property
    def argument(self) -> pywrenfold.gen.Argument:
        """An optional output argument."""

    @property
    def statements(self) -> AstSpan:
        """Statements that are relevant when the optional argument is present."""

class Parenthetical:
    """Wrap an expression in parentheses."""

    def __repr__(self) -> str: ...

    @property
    def contents(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Value that should be wrapped in parentheses."""

class SpecialConstant:
    """Emit a mathematical constant"""

    def __repr__(self) -> str: ...

    @property
    def value(self) -> pywrenfold.enumerations.SymbolicConstant:
        """Enum indicating the value of the constant."""

class ReturnObject:
    """Return a value from the function."""

    def __repr__(self) -> str: ...

    @property
    def value(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Value or object being returned."""

class Ternary:
    """A ternary expression: ``condition ? left : right``"""

    def __repr__(self) -> str: ...

    @property
    def condition(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Value that should be wrapped in parentheses."""

    @property
    def left(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Value when the condition is true."""

    @property
    def right(self) -> Add | AssignTemporary | AssignOutputMatrix | AssignOutputScalar | AssignOutputStruct | BooleanLiteral | Branch | CallExternalFunction | CallStdFunction | Cast | Comment | Compare | ConstructCustomType | ConstructMatrix | Declaration | Divide | FloatLiteral | GetArgument | GetField | GetMatrixElement | IntegerLiteral | Multiply | Negate | OptionalOutputBranch | Parenthetical | SpecialConstant | ReturnObject | Ternary | VariableRef:
        """Value when the condition is false."""

class VariableRef:
    """Reference a local variable."""

    def __repr__(self) -> str: ...

    @property
    def name(self) -> str:
        """Name of the variable."""

class FunctionSignature:
    """
    Emit the signature of a generated function: ``name(... arguments ...) -> return_annotation``
    """

    def __repr__(self) -> str: ...

    @property
    def return_type(self) -> pywrenfold.type_info.ScalarType | pywrenfold.type_info.MatrixType | pywrenfold.type_info.CustomType | None:
        """Return type of the function."""

    @property
    def name(self) -> str:
        """Name of the function."""

    @property
    def arguments(self) -> list[pywrenfold.gen.Argument]:
        """List of arguments."""

class FunctionDefinition:
    """
    Define a generated function. This is the top level object of the emitted syntax tree.
    """

    def __repr__(self) -> str: ...

    @property
    def signature(self) -> FunctionSignature:
        """Function signature."""

    @property
    def body(self) -> AstSpan:
        """Statements that make up the body of the function."""
