import typing as T

from wrenfold import ast, code_generation, type_info
from wrenfold.enumerations import (
    RelationalOperation,
    StdMathFunction,
    SymbolicConstant,
)


# noinspection PyMethodMayBeStatic
class CCodeGenerator(code_generation.BaseGenerator):
    """
    A custom code-generator that emits C code. I have endeavored to make this compatible
    with C99. Not being a C programmer myself, it might have small mistakes.

    By inheriting `BaseGenerator` and implementing the methods below, you can adapt wrenfold
    to whatever language or API you like.
    """

    def __init__(self, indent: int = 2):
        super().__init__()
        assert indent > 0, f"indent = {indent}"
        self._indent: str = " " * indent

    def _indent_and_join(self, lines: T.Iterable[str]) -> str:
        lines_split = []
        for line in lines:
            lines_split.extend(line.splitlines())
        return self._indent + f"\n{self._indent}".join(lines_split)

    @staticmethod
    def _format_numeric_type(t: type_info.NumericType) -> str:
        if t == type_info.NumericType.Bool:
            return "bool"
        elif t == type_info.NumericType.Integer:
            return "int64_t"
        elif t == type_info.NumericType.Float:
            return "double"
        else:
            raise NotImplementedError(f"Unsupported type: {t}")

    def format_scalar_type(self, t: type_info.ScalarType) -> str:
        return self._format_numeric_type(t.numeric_type)

    def format_custom_type(self, custom: type_info.CustomType) -> str:
        return custom.name

    def format_add(self, add: ast.Add) -> str:
        return " + ".join(self.format(x) for x in add.args)

    def format_assign_output_matrix(self, mat: ast.AssignOutputMatrix) -> str:
        """Call `set_output_value` on every element of the matrix."""
        mat_type = mat.arg.type
        assert isinstance(mat_type, type_info.MatrixType)
        result = []
        # Optional args are pointers, non-optional are const spans.
        addr_of = "&" if not mat.arg.is_optional else ""
        for index, val in enumerate(mat.value.args):
            row, col = mat_type.compute_indices(idx=index)
            result.append(
                f"set_output_value({addr_of}{mat.arg.name}, {row}, {col}, {self.format(val)});"
            )
        return "\n".join(result)

    def format_assign_output_scalar(self, scalar: ast.AssignOutputScalar) -> str:
        """Scalar arguments are passed as non-const pointers we assign to."""
        return f"*{scalar.arg.name} = {self.format(scalar.value)};"

    def format_assign_output_struct(self, struct: ast.AssignOutputStruct) -> str:
        """Assign to a struct."""
        return f"*{struct.arg.name} = {self.format(struct.value)};"

    def format_assign_temporary(self, temp: ast.AssignTemporary) -> str:
        return f"{temp.left} = {self.format(temp.right)};"

    def format_boolean_literal(self, b: ast.BooleanLiteral) -> str:
        """We assume the user has included stdbool.h so we can use true/false."""
        return "true" if b.value else "false"

    def format_branch(self, branch: ast.Branch) -> str:
        result = f"if ({self.format(branch.condition)}) {{\n"
        result += self._indent_and_join(self.format(x) for x in branch.if_branch)
        if len(branch.else_branch) > 0:
            result += "\n} else {\n"
            result += self._indent_and_join(self.format(x) for x in branch.else_branch)
        result += "\n}"
        return result

    def format_call_external_function(self, call: ast.CallExternalFunction) -> str:
        """You can override this method to customize the generated name for external functions."""
        return f"{call.function.name}(" + ", ".join(self.format(x) for x in call.args) + ")"

    def format_call_std_function(self, call: ast.CallStdFunction) -> str:
        if call.function == StdMathFunction.Signum:
            # There is no standard sign function in C.
            sign_arg = self.format(call.args[0])
            return f"(double)(int(0.0 < {sign_arg}) - int({sign_arg} < 0.0))"
        functions = {
            StdMathFunction.Cos: "cos",
            StdMathFunction.Sin: "sin",
            StdMathFunction.Tan: "tan",
            StdMathFunction.Acos: "acos",
            StdMathFunction.Asin: "asin",
            StdMathFunction.Atan: "atan",
            StdMathFunction.Sqrt: "sqrt",
            StdMathFunction.Cosh: "cosh",
            StdMathFunction.Sinh: "sinh",
            StdMathFunction.Tanh: "tanh",
            StdMathFunction.Acosh: "acosh",
            StdMathFunction.Asinh: "asinh",
            StdMathFunction.Atanh: "atanh",
            StdMathFunction.Abs: "fabs",
            StdMathFunction.Log: "log",
            StdMathFunction.Signum: "sign",
            StdMathFunction.Floor: "floor",
            StdMathFunction.Atan2: "atan2",
            StdMathFunction.Powi: "pow",
            StdMathFunction.Powf: "pow",
        }
        return f"{functions[call.function]}(" + ", ".join(self.format(x) for x in call.args) + ")"

    def format_cast(self, cast: ast.Cast) -> str:
        return f"({self._format_numeric_type(cast.destination_type)})({self.format(cast.arg)})"

    def format_comment(self, comment: ast.Comment) -> str:
        return "\n".join(f"// {x}" for x in comment.split_lines())

    def format_compare(self, compare: ast.Compare) -> str:
        if compare.operation == RelationalOperation.LessThan:
            op = "<"
        elif compare.operation == RelationalOperation.LessThanOrEqual:
            op = "<="
        elif compare.operation == RelationalOperation.Equal:
            op = "=="
        else:
            raise NotImplementedError(f"Unknown operation: {compare.operation}")
        return f"{self.format(compare.left)} {op} {self.format(compare.right)}"

    def format_construct_matrix(self, matrix: ast.ConstructMatrix) -> str:
        """
        If we want to return output matrices, we need to generate code that can allocate and fill
        them here.
        """
        raise NotImplementedError("Not implemented in this example.")

    def format_construct_custom_type(self, custom: ast.ConstructCustomType) -> str:
        """Use C99 designated initializer syntax to create structs."""
        fields = ", ".join(
            f".{f.name} = {self.format(custom.get_field_value(f.name))}" for f in custom.type.fields
        )
        return f"{{ {fields} }}"

    def format_declaration(self, decl: ast.Declaration) -> str:
        if isinstance(decl.type, (type_info.ScalarType, type_info.CustomType)):
            if decl.value is not None:
                return f"const {self.format(decl.type)} {decl.name} = {self.format(decl.value)};"
            else:
                return f"{self.format(decl.type)} {decl.name};"
        else:
            # TODO: Add allocation and initialization of matrix here.
            raise TypeError("Declaration of matrices not implemented for this generator.")

    def format_divide(self, div: ast.Divide) -> str:
        return f"{self.format(div.left)} / {self.format(div.right)}"

    def format_float_literal(self, flt: ast.FloatLiteral) -> str:
        return f"(double)({flt.value})"

    def format_get_argument(self, get: ast.GetArgument) -> str:
        return get.argument.name

    def format_get_field(self, get: ast.GetField) -> str:
        return f"{self.format(get.arg)}.{get.field_name}"

    def format_get_matrix_element(self, get: ast.GetMatrixElement) -> str:
        return f"get_input_value({self.format(get.arg)}, {get.row}, {get.col})"

    def format_integer_literal(self, i: ast.IntegerLiteral) -> str:
        return str(i.value)

    def format_multiply(self, mul: ast.Multiply) -> str:
        return " * ".join(self.format(x) for x in mul.args)

    def format_negate(self, neg: ast.Negate) -> str:
        return f"-{self.format(neg.arg)}"

    def format_optional_output_branch(self, branch: ast.OptionalOutputBranch) -> str:
        result = f"if ({branch.argument.name}) {{\n"
        result += self._indent_and_join(self.format(x) for x in branch.statements)
        result += "\n}"
        return result

    def format_parenthetical(self, parenthetical: ast.Parenthetical) -> str:
        return f"({self.format(parenthetical.contents)})"

    def format_return_object(self, ret: ast.ReturnObject) -> str:
        if isinstance(ret.value, ast.ConstructCustomType):
            # We cannot return a struct directly in C, we need to declare it first.
            result = f"{self.format(ret.value.type)} __ret = {self.format(ret.value)};"
            result += "\nreturn __ret;"
            return result
        return f"return {self.format(ret.value)};"

    def format_special_constant(self, constant: ast.SpecialConstant) -> str:
        if constant.value == SymbolicConstant.Euler:
            return "M_E"
        elif constant.value == SymbolicConstant.Pi:
            return "M_PI"
        else:
            raise NotImplementedError(f"Unsupported constant: {constant.value}")

    def format_ternary(self, ternary: ast.Ternary) -> str:
        left = self.format(ternary.left)
        right = self.format(ternary.right)
        return f"{self.format(ternary.condition)} ? {left} : {right}"

    def format_variable_ref(self, var: ast.VariableRef) -> str:
        return var.name

    def format_function_definition(self, definition: ast.FunctionDefinition) -> str:
        """
        The top-level formatting function. The `FunctionDefinition` object is the root of the
        syntax tree.

        Args:
            definition: ast.FunctionDefinition
        """
        return (
            self.format(definition.signature)
            + " {\n"
            + self._indent_and_join([self.format(x) for x in definition.body])
            + "\n}"
        )

    def format_function_signature(self, sig: ast.FunctionSignature) -> str:
        """
        Assemble an argument list
        """
        args = []
        for arg in sig.arguments:
            if isinstance(arg.type, type_info.MatrixType):
                if arg.direction == code_generation.ArgumentDirection.Input:
                    args.append(f"const input_span2d_t {arg.name}")
                elif not arg.is_optional:
                    args.append(f"const output_span2d_t {arg.name}")
                elif arg.is_optional:
                    # Optional output spans are pass by pointer.
                    # A null span indicates an output we don't want to compute.
                    args.append(f"const output_span2d_t* {arg.name}")
            elif isinstance(arg.type, type_info.CustomType):
                if arg.is_input:
                    args.append(f"const {self.format(arg.type)}*")
                else:
                    args.append(f"{self.format(arg.type)}*")
            else:
                assert isinstance(arg.type, type_info.ScalarType)
                if arg.is_input:
                    args.append(f"const {self.format(arg.type)}")
                else:
                    args.append(f"{self.format(arg.type)}*")

        return_type = "void"
        if sig.return_type is not None:
            if isinstance(sig.return_type, (type_info.ScalarType, type_info.CustomType)):
                return_type = self.format(sig.return_type)
            else:
                raise TypeError("Returning matrices is not supported in this generator yet.")

        return f"inline {return_type} {sig.name}({', '.join(args)})"


C_PREAMBLE = """// Machine generated code.
#ifndef {header_name}_H
#define {header_name}_H

#ifdef __cplusplus
extern "C" {{
#endif  // __cplusplus

#include <math.h>
#include <stdbool.h>    // bool
#include <stdint.h>     // int64_t

#include "c_span_types.h"

{code}

#ifdef __cplusplus
}}
#endif  // __cplusplus

#endif // {header_name}_H
"""
