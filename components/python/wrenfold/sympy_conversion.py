"""
Logic to support conversion from sympy --> wrenfold.
Conversion in the opposite direction is implemented in C++ in `sympy_conversion.cc`.
"""
import importlib
import typing as T

from pywrenfold.sympy_conversion import function_argument_variable, to_sympy

from . import sym, type_info


class Conversions:
    """
    Object used to recursively convert sympy expressions into wrenfold objects.

    OMIT_FROM_SPHINX
    """

    def __init__(self, sp: T.Any) -> None:
        """Initialize with the sympy module."""
        self.sp = sp
        self.value_map = {
            sp.E:
                sym.E,
            sp.pi:
                sym.pi,
            sp.zoo:
                sym.zoo,
            sp.I:
                sym.I,
            sp.nan:
                sym.nan,
            sp.true:
                sym.true,
            sp.false:
                sym.false,
            # Zero, one, negative one, and 1/2 are all specific types in sympy.
            sp.Integer(0):
                sym.zero,
            sp.Integer(1):
                sym.one,
            sp.Integer(-1):
                sym.integer(-1),
            sp.Rational(1, 2):
                sym.rational(1, 2),
        }
        self.type_map = {
            sp.Pow: sym.pow,
            sp.cos: sym.cos,
            sp.sin: sym.sin,
            sp.tan: sym.tan,
            sp.acos: sym.acos,
            sp.asin: sym.asin,
            sp.atan: sym.atan,
            sp.cosh: sym.cosh,
            sp.sinh: sym.sinh,
            sp.tanh: sym.tanh,
            sp.acosh: sym.acosh,
            sp.asinh: sym.asinh,
            sp.atanh: sym.atanh,
            sp.log: sym.log,
            sp.Abs: sym.abs,
            sp.sign: sym.sign,
            sp.floor: sym.floor,
            sp.atan2: sym.atan2,
            sp.StrictLessThan: sym.lt,
            sp.LessThan: sym.le,
            sp.StrictGreaterThan: sym.gt,
            sp.GreaterThan: sym.ge,
            sp.Eq: sym.eq,
            sp.UnevaluatedExpr: sym.unevaluated,
        }
        self.custom_converters = {
            sp.Add: self.convert_add,
            sp.Mul: self.convert_mul,
            sp.Symbol: self.convert_symbol,
            sp.Piecewise: self.convert_piecewise,
            sp.Min: self.convert_min,
            sp.Max: self.convert_max,
            sp.Heaviside: self.convert_heaviside,
            sp.Derivative: self.convert_derivative,
            sp.Subs: self.convert_subs,
            sp.Integer: lambda x: sym.integer(int(x)),
            sp.Float: lambda x: sym.float(float(x)),
            sp.Rational: lambda x: sym.rational(n=x.numerator, d=x.denominator),
        }

        # Cache of already converted expressions. Since expressions often include repeated terms,
        # avoid converting them more than once.
        self.cache: T.Dict[T.Any, sym.AnyExpression] = {}

    def convert_add(self, expr) -> sym.Expr:
        return sym.addition([self(x) for x in expr.args])

    def convert_mul(self, expr) -> sym.Expr:
        return sym.multiplication([self(x) for x in expr.args])

    @staticmethod
    def convert_symbol(expr) -> sym.Expr:
        """
        Convert a symbolic variable.

        :param expr: sympy `Symbol` object.
        """
        kwargs = dict()
        if expr.is_positive:
            kwargs.update(positive=True)
        elif expr.is_nonnegative:
            kwargs.update(nonnegative=True)
        elif expr.is_real:
            kwargs.update(real=True)
        elif expr.is_complex:
            kwargs.update(complex=True)

        if expr.name.startswith('$arg_'):
            if expr.is_integer:
                numeric_type = type_info.NumericType.Integer
            else:
                assert expr.is_real, f"Function argument variables must be floats: {expr}"
                numeric_type = type_info.NumericType.Float
            arg_index, element_index = [int(x) for x in expr.name.lstrip('$arg_').split('_')]
            return function_argument_variable(arg_index, element_index, type=numeric_type)

        return sym.symbols(expr.name, **kwargs)

    def convert_piecewise(self, expr) -> sym.Expr:
        """
        Convert a sympy `Piecewise` expression. Because the `Conditional` type only supports two
        branches, piecewise functions with more than two intervals are converted into a nested
        series of conditionals.

        :param expr: Instance of sympy `Piecewise`.
        """
        if len(expr.args) == 2:
            # If the output values are one and zero, this is equivalent to the iverson bracket.
            (true_val, cond), (false_val, _) = expr.args
            if true_val == 1 and false_val == 0:
                return sym.iverson(self(cond))

        output = self(expr.args[-1][0])
        for (val, cond) in reversed(expr.args[:-1]):
            output = sym.where(self(cond), self(val), output)
        return output

    def convert_min(self, expr) -> sym.Expr:
        if len(expr.args) != 2:
            # TODO: Implement min/max with a variable number of arguments.
            raise NotImplementedError("More than 2 args to min(...) is not supported yet.")
        return sym.min(self(expr.args[0]), self(expr.args[1]))

    def convert_max(self, expr) -> sym.Expr:
        if len(expr.args) != 2:
            raise NotImplementedError("More than 2 args to max(...) is not supported yet.")
        return sym.max(self(expr.args[0]), self(expr.args[1]))

    def convert_heaviside(self, expr) -> sym.Expr:
        """
        Convert `Heaviside` expression. SymPy heaviside specifies a value at x = 0, so
        we need to use two conditionals here to match it.
        """
        x, zero_val = self(expr.args[0]), self(expr.args[1])
        return sym.where(x > 0, 1, sym.where(x < 0, -1, zero_val))

    def convert_derivative(self, expr) -> sym.Expr:
        """
        Convert `Derivative` expression. We need to convert derivatives of multiple variables
        into a stack of nested derivative objects.
        """
        differentiand = expr.args[0]
        argument, count = expr.args[1]
        result = self(differentiand)
        for argument, count in expr.args[1:]:
            result = sym.derivative(function=result, arg=self(argument), order=count)
        return result

    def convert_subs(self, expr) -> sym.Expr:
        """
        Convert incomplete substitution (`Subs`) expression.
        """
        # Subs can have multiple replacements - for now we have to chain these together.
        result = self(expr.expr)
        for variable, value in zip(expr.bound_symbols, expr.point):
            result = sym.substitution(result, self(variable), self(value))

        return result

    def convert_applied_undef(self, expr) -> sym.Expr:
        """
        Convert AppliedUndef to symbolic function invocation.
        """
        sp_func = expr.func
        if sp_func.default_assumptions:
            raise NotImplementedError(f"Cannot convert SymPy function with assumptions: {sp_func}")
        args = [self(x) for x in expr.args]
        return sym.Function(sp_func.name)(*args)

    def __call__(self, expr: T.Any) -> T.Union[sym.Expr, sym.MatrixExpr, sym.BooleanExpr]:
        """
        Convert sympy expression `expr`. We check the different maps stored on self for
        matching values or types, and use the corresponding method to convert.

        :param expr: A sympy expression.
        """
        # Not all types are hashable (sympy matrix, for example) so cache only hashable things.
        if isinstance(expr, T.Hashable):
            cached_result = self.cache.get(expr)
            if cached_result is not None:
                return cached_result
            result = self._convert_expr(expr)
            self.cache[expr] = result
            return result
        else:
            return self._convert_expr(expr)

    def _convert_expr(self, expr: T.Any) -> T.Union[sym.Expr, sym.MatrixExpr, sym.BooleanExpr]:
        func = self.type_map.get(type(expr), None)
        if func is not None:
            args = [self(x) for x in expr.args]
            return func(*args)

        func = self.custom_converters.get(type(expr), None)
        if func is not None:
            return func(expr)

        if isinstance(expr, self.sp.MatrixBase):
            # All matrix expressions are converted into `matrix`.
            rows, cols = expr.shape
            data: T.List[sym.Expr] = []
            for i in range(0, rows):
                data.append([self(expr[i, j]) for j in range(0, cols)])
            return sym.matrix(data)

        if isinstance(expr, T.Hashable):
            value = self.value_map.get(expr, None)
            if value is not None:
                return value

        if isinstance(expr, self.sp.core.function.AppliedUndef):
            return self.convert_applied_undef(expr)

        raise TypeError(f"sympy expression of type `{type(expr)}` cannot be converted.")


def from_sympy(expr: T.Any, sp: T.Any = None) -> T.Union[sym.Expr, sym.MatrixExpr, sym.BooleanExpr]:
    """
    Convert sympy expressions to wrenfold expressions. This method will recursively traverse
    the sympy expression tree, converting each encountered object to the equivalent wrenfold
    expression.

    Args:
      expr: A sympy expression.
      sp: The sympy module. If None, the package ``sympy`` will be imported.

    Returns:
      The closest equivalent expression.

    Raises:
      TypeError: When a sympy object has no equivalent in wrenfold.
    """
    if sp is None:
        sp = importlib.import_module(name="sympy")
    return Conversions(sp=sp)(expr=expr)
