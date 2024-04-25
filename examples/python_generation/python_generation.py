"""
An example of a custom code-generator implemented entirely in python.
"""
import dataclasses
import numpy as np
import typing as T
import unittest

from wrenfold import ast
from wrenfold import code_generation
from wrenfold import geometry
from wrenfold import sym
from wrenfold import type_annotations
from wrenfold import type_info
from wrenfold.enumerations import StdMathFunction, RelationalOperation, SymbolicConstant


@dataclasses.dataclass
class SimParamsSymbolic:
    """
    A sample struct with some parameters of a simulation. This is the symbolic version.
    We use this to test code-generation of custom types.
    """
    mass: sym.Expr
    drag_coefficient: sym.Expr
    gravity: sym.Expr


@dataclasses.dataclass
class SimParams:
    """A sample struct with some parameters of a simulation."""
    mass: float
    drag_coefficient: float
    gravity: float


# noinspection PyMethodMayBeStatic
class PythonCodeGenerator(code_generation.BaseGenerator):
    """
    A custom code-generator that emits python code. This could be used quickly try out a generated
    function in an ipython notebook, for example. The code emitted by this generator is also
    suitable for use with numba.

    Caveat: Because python doesn't have an exact equivalent of "output arguments" from C++, we
    convert output arguments into a tuple of return values. For optional outputs, we instead
    generate a boolean argument that specifies whether to compute the optional value.
    """

    def __init__(self, indent: int = 2):
        super().__init__()
        assert indent > 0, f'indent = {indent}'
        self._indent: str = ' ' * indent

    def _indent_and_join(self, lines: T.Iterable[str]) -> str:
        lines_split = []
        for line in lines:
            lines_split.extend(line.splitlines())
        return self._indent + f'\n{self._indent}'.join(lines_split)

    @staticmethod
    def _format_numeric_type(t: type_info.NumericType) -> str:
        if t == type_info.NumericType.Bool:
            return 'bool'
        elif t == type_info.NumericType.Integer:
            return 'int'
        elif t == type_info.NumericType.Float:
            return 'float'
        else:
            raise NotImplementedError(f"Unsupported type: {t}")

    def format_scalar_type(self, t: type_info.ScalarType) -> str:
        return self._format_numeric_type(t.numeric_type)

    def format_matrix_type(self, _: type_info.MatrixType) -> str:
        return 'np.ndarray'

    def format_custom_type(self, custom: type_info.CustomType) -> str:
        if custom.python_type == SimParamsSymbolic:
            return "SimParams"
        return custom.name

    def format_add(self, add: ast.Add) -> str:
        return f'{self.format(add.left)} + {self.format(add.right)}'

    def format_assign_output_matrix(self, mat: ast.AssignOutputMatrix) -> str:
        mat_type = mat.arg.type
        assert isinstance(mat_type, type_info.MatrixType)

        args = ",\n  ".join([self.format(arg) for arg in mat.value.args])
        return f'{mat.arg.name} = np.array([\n  {args}\n], dtype=np.float64).reshape({mat_type.rows}, {mat_type.cols})'

    def format_assign_output_scalar(self, scalar: ast.AssignOutputScalar) -> str:
        return f'{scalar.arg.name} = {self.format(scalar.value)}'

    def format_assign_output_struct(self, struct: ast.AssignOutputStruct) -> str:
        return f'{struct.arg.name} = {self.format(struct.value)}'

    def format_assign_temporary(self, temp: ast.AssignTemporary) -> str:
        return f'{temp.left} = {self.format(temp.right)}'

    def format_boolean_literal(self, b: ast.BooleanLiteral) -> str:
        return str(b.value)

    def format_branch(self, branch: ast.Branch) -> str:
        result = f'if {self.format(branch.condition)}:\n'
        result += self._indent_and_join(self.format(x) for x in branch.if_branch)
        if len(branch.else_branch) > 0:
            result += f'\nelse:\n'
            result += self._indent_and_join(self.format(x) for x in branch.else_branch)
        return result

    def format_call_external_function(self, call: ast.CallExternalFunction) -> str:
        return f'{call.function.name}(' + ', '.join(self.format(x) for x in call.args) + ')'

    def format_call_std_function(self, call: ast.CallStdFunction) -> str:
        functions = {
            StdMathFunction.Cos: 'cos',
            StdMathFunction.Sin: 'sin',
            StdMathFunction.Tan: 'tan',
            StdMathFunction.Acos: 'arccos',
            StdMathFunction.Asin: 'arcsin',
            StdMathFunction.Atan: 'arctan',
            StdMathFunction.Sqrt: 'sqrt',
            StdMathFunction.Cosh: 'cosh',
            StdMathFunction.Sinh: 'sinh',
            StdMathFunction.Tanh: 'tanh',
            StdMathFunction.Acosh: 'arccosh',
            StdMathFunction.Asinh: 'arcsinh',
            StdMathFunction.Atanh: 'arctanh',
            StdMathFunction.Abs: 'abs',
            StdMathFunction.Log: 'log',
            StdMathFunction.Signum: 'sign',
            StdMathFunction.Floor: 'floor',
            StdMathFunction.Atan2: 'arctan2',
            StdMathFunction.Powi: 'power',
            StdMathFunction.Powf: 'power',
        }
        return f'np.{functions[call.function]}(' + ', '.join(
            self.format(x) for x in call.args) + ')'

    def format_cast(self, cast: ast.Cast) -> str:
        return f'{self._format_numeric_type(cast.destination_type)}({self.format(cast.arg)})'

    def format_comment(self, comment: ast.Comment) -> str:
        return '\n'.join(f'# {x}' for x in comment.split_lines())

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

    def format_construct_matrix(self, matrix: ast.ConstructMatrix) -> str:
        args = ', '.join(self.format(x) for x in matrix.args)
        return f'np.array(({args})).reshape({matrix.type.rows}, {matrix.type.cols})'

    def format_construct_custom_type(self, custom: ast.ConstructCustomType) -> str:
        fields = self._indent_and_join(
            f'{f.name}={self.format(custom.get_field_value(f.name))}' for f in custom.type.fields)
        return f'{self.format(custom.type)}(\n  {fields}\n)'

    def format_declaration(self, decl: ast.Declaration) -> str:
        return f'{decl.name} = {self.format(decl.value) if decl.value is not None else None}'

    def format_divide(self, div: ast.Divide) -> str:
        return f'{self.format(div.left)} / {self.format(div.right)}'

    def format_float_literal(self, flt: ast.FloatLiteral) -> str:
        return f'float({flt.value})'

    def format_get_argument(self, get: ast.GetArgument) -> str:
        return get.argument.name

    def format_get_field(self, get: ast.GetField) -> str:
        return f'{self.format(get.arg)}.{get.field_name}'

    def format_get_matrix_element(self, get: ast.GetMatrixElement) -> str:
        return f'{self.format(get.arg)}[{get.row}, {get.col}]'

    def format_integer_literal(self, i: ast.IntegerLiteral) -> str:
        return str(i.value)

    def format_multiply(self, mul: ast.Multiply) -> str:
        return f'{self.format(mul.left)} * {self.format(mul.right)}'

    def format_negate(self, neg: ast.Negate) -> str:
        return f'-{self.format(neg.arg)}'

    def format_optional_output_branch(self, branch: ast.OptionalOutputBranch) -> str:
        result = f'if compute_{branch.argument.name}:\n'
        result += self._indent_and_join(self.format(x) for x in branch.statements)
        result += f'\nelse:\n{self._indent}{branch.argument.name} = None'
        return result

    def format_return_object(self, ret: ast.ReturnObject) -> str:
        raise NotImplementedError("Handled in function body")

    def format_special_constant(self, constant: ast.SpecialConstant) -> str:
        if constant.value == SymbolicConstant.Euler:
            return 'np.e'
        elif constant.value == SymbolicConstant.Pi:
            return 'np.pi'
        else:
            raise NotImplementedError(f"Unsupported constant: {constant.value}")

    def format_variable_ref(self, var: ast.VariableRef) -> str:
        return var.name

    def format_function_definition(self, definition: ast.FunctionDefinition) -> str:
        """
        The top-level formatting function. The `FunctionDefinition` object is the root of the
        syntax tree.

        Args:
            definition: ast.FunctionDefinition
        """
        # Insert some reshape calls to make sure matrix inputs are accessible with 2D slicing.
        lines: T.List[str] = []
        for arg in definition.signature.arguments:
            if arg.direction == code_generation.ArgumentDirection.Input and \
                    isinstance(arg.type, type_info.MatrixType):
                lines.append(f'{arg.name} = {arg.name}.reshape(({arg.type.rows}, {arg.type.cols}))')

        # Generate every statement in the body, but omit the return statement.
        # We customize the return below to include output args.
        lines.extend(
            [self.format(x) for x in definition.body if not isinstance(x, ast.ReturnObject)])

        # Figure out what we need to include in the return statement. Since we don't have
        # output args, convert them to a tuple of return values.
        returned: T.List[str] = []
        if isinstance(definition.body[-1], ast.ReturnObject):
            return_value = T.cast(ast.ReturnObject, definition.body[-1]).value
            returned.append(self.format(return_value))

        for arg in definition.signature.arguments:
            if arg.direction != code_generation.ArgumentDirection.Input:
                returned.append(arg.name)

        lines.append('return ' + ', '.join(returned))
        return self.format(definition.signature) + '\n' + self._indent_and_join(lines)

    def format_function_signature(self, sig: ast.FunctionSignature) -> str:
        args = []
        for arg in sig.arguments:
            if arg.direction == code_generation.ArgumentDirection.Input:
                args.append(f'{arg.name}: {self.format(arg.type)}')
            elif arg.is_optional:
                # For output args, create a bool to indicate if it should be computed
                args.append(f'compute_{arg.name}: bool')

        returned_types: T.List[str] = []
        if sig.return_type is not None:
            returned_types.append(self.format(sig.return_type))

        for arg in sig.arguments:
            if arg.direction == code_generation.ArgumentDirection.Output:
                returned_types.append(self.format(arg.type))
            elif arg.direction == code_generation.ArgumentDirection.OptionalOutput:
                returned_types.append(f'T.Optional[{self.format(arg.type)}]')

        if returned_types:
            return_annotation = f'T.Tuple[{", ".join(returned_types)}]'
        else:
            return_annotation = 'None'

        return f'def {sig.name}({", ".join(args)}) -> {return_annotation}:'


def sample_function_1(w: type_annotations.Vector3, v: type_annotations.Vector3):
    """
    Rotate vector `v` by the rotation matrix determined by Rodrigues vector `w`.

    Args:
        w: 3x1 rotation vector.
        v: 3x1 vector to be rotated.
    """
    v_rot = geometry.Quaternion.from_rotation_vector(w, epsilon=1.0e-6).to_rotation_matrix() * v
    v_rot_D_w = v_rot.jacobian(vars=w)
    return (code_generation.ReturnValue(v_rot),
            code_generation.OutputArg(v_rot_D_w, name='v_rot_D_w', is_optional=True))


def sample_function_2(position: type_annotations.Vector2, velocity: type_annotations.Vector2,
                      dt: type_annotations.RealScalar, params: SimParamsSymbolic):
    """
    An example function used to test code generation. We integrate some simple 2D dynamics
    for a projectile subject to gravity and drag.

    Args:
        position: Position of 2D object.
        velocity: Velocity of 2D object.
        dt: Step length.
        params: Some made up simulation params.
    """
    # Compute force:
    f = sym.vector(0, -params.gravity * params.mass) - \
        velocity * velocity.norm() * (params.drag_coefficient / 2)
    accel = f * (1 / params.mass)

    # Euler integration:
    velocity = velocity + accel * dt
    position = position + velocity * dt + accel * (dt ** 2) / 2

    return (
        code_generation.ReturnValue(position),
        code_generation.OutputArg(expression=velocity, name="velocity_out", is_optional=False),
    )


def generate_and_import(func: T.Callable) -> T.Callable:
    """
    Code-generate a symbolic function as python code, then `exec` the code and return a python
    function that operations on numpy types.
    """
    code = code_generation.generate_function(func, generator=PythonCodeGenerator())

    # Apply preamble
    code = '\n'.join([
        'import numpy as np',
        'import typing as T',
    ]) + '\n\n' + code

    # Execute the generated code and return the specified function. This is a fair bit easier
    # than writing it out and using importlib, which fails anyway on windows when we write
    # the file to /tmp: https://stackoverflow.com/questions/66884520/
    exec(code)
    return locals()[func.__name__]


# noinspection PyMethodMayBeStatic
class PythonGenerationTest(unittest.TestCase):

    def test_sample_function_1(self):
        sample_function_1_gen = generate_and_import(sample_function_1)

        # Generate some random vectors to work with:
        np.random.seed(13)
        sample_w = np.random.uniform(size=(50, 3), low=-np.pi, high=np.pi)
        sample_v = np.random.uniform(size=(50, 3))

        # Put half of the rotation samples into the small-angle approximation territory:
        half = len(sample_w) // 2
        sample_w[half:, ...] *= 1.0e-9

        for (w, v) in zip(sample_w, sample_v):
            v_rot, v_rot_D_w = sample_function_1_gen(w=w, v=v, compute_v_rot_D_w=True)

            # Compute with the symbolic version.
            # We need to create variables for `w` so the derivative can be taken:
            w_sym = sym.symbols('x, y, z')
            v_rot_sym, v_rot_D_w_sym = sample_function_1(
                w=type_annotations.Vector3(w_sym), v=type_annotations.Vector3(v))

            v_rot_sym, v_rot_D_w_sym = T.cast(sym.MatrixExpr, v_rot_sym.expression), T.cast(
                sym.MatrixExpr, v_rot_D_w_sym.expression)

            for s, val in zip(w_sym, w):
                v_rot_sym = v_rot_sym.subs(s, val)
                v_rot_D_w_sym = v_rot_D_w_sym.subs(s, val)

            v_rot_sym = v_rot_sym.eval()
            v_rot_D_w_sym = v_rot_D_w_sym.eval()

            np.testing.assert_allclose(v_rot_sym, v_rot, atol=1.0e-14)
            np.testing.assert_allclose(v_rot_D_w_sym, v_rot_D_w, atol=1.0e-14)

    def test_sample_function_2(self):
        sample_function_2_gen = generate_and_import(sample_function_2)

        p_in = np.array([10.2, -5.0])
        v_in = np.array([9.1, 8.3])

        p_out_sym, v_out_sym = sample_function_2(
            position=type_annotations.Vector2(p_in),
            velocity=type_annotations.Vector2(v_in),
            dt=0.7,
            params=SimParamsSymbolic(mass=5.7, drag_coefficient=2.4, gravity=9.81))
        p_out_sym = p_out_sym.expression.eval()
        v_out_sym = v_out_sym.expression.eval()

        p_out, v_out = sample_function_2_gen(
            position=p_in,
            velocity=v_in,
            dt=0.7,
            params=SimParams(mass=5.7, drag_coefficient=2.4, gravity=9.81))

        np.testing.assert_allclose(p_out_sym, p_out, atol=1.0e-14)
        np.testing.assert_allclose(v_out_sym, v_out, atol=1.0e-14)


if __name__ == '__main__':
    unittest.main()
