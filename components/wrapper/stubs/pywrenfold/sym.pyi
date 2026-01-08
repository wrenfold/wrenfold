from collections.abc import Callable, Iterable, Iterator, Sequence
from typing import Annotated, Any, overload

import numpy
from numpy.typing import NDArray

import pywrenfold.enumerations
import pywrenfold.type_info


class BooleanExpr:
    """A boolean-valued symbolic expression."""

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: BooleanExpr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: BooleanExpr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __repr__(self) -> str: ...

    def expression_tree_str(self) -> str:
        """See :func:`wrenfold.sym.Expr.expression_tree_str`."""

    @property
    def type_name(self) -> str:
        """
        Retrieve the name of the underlying C++ expression type. See :func:`wrenfold.sym.Expr.type_name`.
        """

    @property
    def args(self) -> tuple:
        """Arguments of ``self`` as a tuple."""

    @overload
    def subs(self, target: Expr, substitute: Expr) -> BooleanExpr:
        """See :func:`wrenfold.sym.subs`"""

    @overload
    def subs(self, target: BooleanExpr, substitute: BooleanExpr) -> BooleanExpr: ...

    @overload
    def subs(self, pairs: Sequence[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> BooleanExpr: ...

    def __bool__(self) -> bool:
        """Coerce expression to boolean."""

true: BooleanExpr = ...

false: BooleanExpr = ...

class Expr:
    """A scalar-valued symbolic expression."""

    @overload
    def __init__(self, arg: int, /) -> None: ...

    @overload
    def __init__(self, arg: float, /) -> None: ...

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: Expr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: Expr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __repr__(self) -> str: ...

    def expression_tree_str(self) -> str:
        """
        Recursively traverses the expression tree and creates a pretty-printed string
        describing the expression. This can be a useful representation for diffing math
        expression trees.

        Returns:
         String representation of the expression tree.

        Example:
          >>> x, y = sym.make_symbols('x', 'y')
          >>> print((x*y + 5).expression_tree_str())
          Addition:
          ├─ Integer (5)
          └─ Multiplication:
            ├─ Variable (x, unknown)
            └─ Variable (y, unknown)
        """

    @property
    def type_name(self) -> str:
        """
        Retrieve the name of the underlying C++ expression type.

        Returns:
          String name of the underlying expression type.

        Example:
          >>> x, y = sym.make_symbols('x', 'y')
          >>> print(x.type_name)
          Variable
          >>> print((x + y).type_name)
          Addition
        """

    @property
    def args(self) -> tuple:
        """Arguments of ``self`` as a tuple."""

    def diff(self, var: Expr, order: int = 1, use_abstract: bool = False) -> Expr:
        """
        Differentiate the expression with respect to the specified variable.

        Args:
          var: Scalar variable with respect to which the derivative is taken. Must be of type ``variable``
            or ``compound_expression_element``. Differentiation with respect to arbitrary expressions is not
            permitted.

          order: Order of the derivative, where 1 is the first derivative.

          use_abstract: Governs behavior when a non-differentiable expression is encountered. If
            false (the default), the value zero is substituted. This is not mathematically accurate, but is
            often more computationally useful than introducing an expression that would produce ``nan`` or
            ``inf`` when evaluated. If ``use_abstract=True``, an abstract ``derivative`` expression is
            inserted instead.

        Returns:
          Derivative of ``self`` taken ``order`` times with respect to ``var``.

        Examples:
          Taking the derivative with respect to a variable:

          >>> x, y = sym.make_symbols('x', 'y')
          >>> f = x * sym.sin(x * y) + 3
          >>> f.diff(x)
          x*y*cos(x*y) + sin(x*y)
          >>> f.diff(x, order=2)
          -x*y**2*sin(x*y) + 2*y*cos(x*y)
          >>> f.diff(y)
          x**2*cos(x*y)
        """

    def distribute(self) -> Expr:
        """See :func:`wrenfold.sym.distribute`."""

    @overload
    def subs(self, pairs: Sequence[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> Expr:
        """See :func:`wrenfold.sym.subs`."""

    @overload
    def subs(self, target: Expr, substitute: Expr) -> Expr:
        """
        Overload of ``subs`` that performs a single scalar-valued substitution.
        """

    @overload
    def subs(self, target: BooleanExpr, substitute: BooleanExpr) -> Expr:
        """
        Overload of ``subs`` that performs a single boolean-valued substitution.
        """

    def eval(self) -> int | float | complex:
        """
        Evaluate the mathematical expression into a numeric value. Traverses the expression tree, converting
        every numeric literal and constant into a double precision float. Operations are recursively
        evaluated until the result is obtained.

        Important:
          Unlike ``sympy.evalf``, no special care is taken to preserve fixed precision. This operation will
          allow floating point error to propagate normally. Built-in math functions like ``sin`` or ``log``
          will call their C++ STL equivalent.

        Returns:
          The evaluated result. Typically the result is ``int``, ``float``, or ``complex``.

        Raises:
          :class:`wrenfold.exceptions.TypeError`: If the expression contains symbolic expressions that
            cannot be converted to a numerical representation.

        Examples:
          >>> x = sym.symbol('x')
          >>> f = sym.cos(sym.pi / 3 + x)
          >>> f.subs(x, sym.pi).eval()
          -0.4999999999999998
          >>> sym.sin(sym.pi/4 + sym.pi*sym.I/6).eval()
          >>> (0.8062702490019065+0.3873909064828401j)
        """

    @overload
    def collect(self, term: Expr) -> Expr:
        """Overload of ``collect`` that accepts a single variable."""

    @overload
    def collect(self, terms: Sequence[Expr]) -> Expr:
        r"""
        Combine coefficients of the specified expression(s) (and powers thereof) into additive sums.
        Given a target expression :math:`x`, ``collect`` traverse the expression tree and identifies terms
        that appear in products with the target. Terms multiplied by matching powers of :math:`x` are summed
        into a new combined coefficient. For example:

        A single power of :math:`x` exists in the input:

        .. math::
          x\cdot\pi + x \cdot y + x \rightarrow x\cdot\left(\pi + y + 1\right)

        Both :math:`x` and :math:`x^2` exist in the input:

        .. math::
          x^2\cdot\sin{y} - x\cdot 5 + x^2 \cdot 4 + x\cdot\pi \rightarrow x\cdot\left(-5 + \pi\right) +
          x^2\left(4 + \sin{y}\right)

        When multiple input variables are specified, ``collect`` will group recursively starting with the
        first variable, then proceeding to the second. For example, if we take the expression:

        .. math::
          x^2 \cdot y \cdot \cos{w} + 3 \cdot y \cdot x^2 + x \cdot y^2 + x \cdot y^2 \cdot \pi -
          5 \cdot x^2 \cdot y^2 + x \cdot w

        And collect it with respect to ``[x, y]``, we obtain:

        .. math::
          x^2 \cdot \left(y \cdot \left(3 + \cos{w}\right) - 5 \cdot y^2\right) +
          x \cdot \left(w + y^2 \cdot \left(1 + \pi\right)\right)

        Args:
          terms: Sequence of expressions whose coefficients we will collect.

        Returns:
          The collected expression.

        Examples:
          >>> x, y, w = sym.make_symbols('x', 'y', 'w')
          >>> f = x**2*y*sym.cos(w) + 3*y*x**2 + x*y**2 + x*y**2*sym.pi - 5*x**2*y**2 + x*w
          >>> f.collect(x)
          x*(w + pi*y**2 + y**2) + x**2*(3*y + y*cos(w) - 5*y**2)
          >>> f.collect([x, y])
          x*(w + y**2*(1 + pi)) + x**2*(y*(3 + cos(w)) - 5*y**2)
          >>> f.collect([y, x])
          w*x + x**2*y*(3 + cos(w)) + y**2*(x*(1 + pi) - 5*x**2)
        """

    @overload
    def __add__(self, arg: Expr, /) -> Expr: ...

    @overload
    def __add__(self, arg: int, /) -> Expr: ...

    @overload
    def __add__(self, arg: float, /) -> Expr: ...

    @overload
    def __sub__(self, arg: Expr, /) -> Expr: ...

    @overload
    def __sub__(self, arg: int, /) -> Expr: ...

    @overload
    def __sub__(self, arg: float, /) -> Expr: ...

    @overload
    def __mul__(self, arg: Expr, /) -> Expr: ...

    @overload
    def __mul__(self, arg: int, /) -> Expr: ...

    @overload
    def __mul__(self, arg: float, /) -> Expr: ...

    @overload
    def __truediv__(self, arg: Expr, /) -> Expr: ...

    @overload
    def __truediv__(self, arg: int, /) -> Expr: ...

    @overload
    def __truediv__(self, arg: float, /) -> Expr: ...

    def __neg__(self) -> Expr: ...

    @overload
    def __pow__(self, other: Expr) -> Expr: ...

    @overload
    def __pow__(self, other: int) -> Expr: ...

    @overload
    def __pow__(self, other: float) -> Expr: ...

    @overload
    def __rpow__(self, other: Expr) -> Expr: ...

    @overload
    def __rpow__(self, other: int) -> Expr: ...

    @overload
    def __rpow__(self, other: float) -> Expr: ...

    def __abs__(self) -> Expr: ...

    @overload
    def __gt__(self, arg: Expr, /) -> BooleanExpr: ...

    @overload
    def __gt__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __gt__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __gt__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __gt__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __ge__(self, arg: Expr, /) -> BooleanExpr: ...

    @overload
    def __ge__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __ge__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __ge__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __ge__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __lt__(self, arg: Expr, /) -> BooleanExpr: ...

    @overload
    def __lt__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __lt__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __lt__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __lt__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __le__(self, arg: Expr, /) -> BooleanExpr: ...

    @overload
    def __le__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __le__(self, arg: int, /) -> BooleanExpr: ...

    @overload
    def __le__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __le__(self, arg: float, /) -> BooleanExpr: ...

    @overload
    def __radd__(self, arg: int, /) -> Expr: ...

    @overload
    def __radd__(self, arg: float, /) -> Expr: ...

    @overload
    def __rsub__(self, arg: int, /) -> Expr: ...

    @overload
    def __rsub__(self, arg: float, /) -> Expr: ...

    @overload
    def __rmul__(self, arg: int, /) -> Expr: ...

    @overload
    def __rmul__(self, arg: float, /) -> Expr: ...

    @overload
    def __rtruediv__(self, arg: int, /) -> Expr: ...

    @overload
    def __rtruediv__(self, arg: float, /) -> Expr: ...

    def __bool__(self) -> None:
        """Coerce expression to bool."""

def symbol(name: str, set: pywrenfold.enumerations.NumberSet = pywrenfold.enumerations.NumberSet.Unknown) -> Expr:
    """
    Create a ``variable`` expressions with the provided name and numeric set.

    Args:
      name: String name for the variable. Variables with the same name and numeric set are considered
        identical.
      set: Classification of the symbol, an instance of :class:`wrenfold.enumerations.NumberSet`.

    Returns:
      A ``variable`` expression.

    Examples:
      >>> x0 = sym.symbol('x')
      >>> print(x0)
      x
      >>> x0.type_name
      'Variable'
      >>> x1 = sym.symbol('x', set=NumberSet.Complex)
      >>> x0.is_identical_to(x1)
      False

    Raises:
      :class:`wrenfold.exceptions.InvalidArgumentError`: If the variable name is an empty string.
    """

@overload
def make_symbols(names: Sequence[str], set: pywrenfold.enumerations.NumberSet = pywrenfold.enumerations.NumberSet.Unknown) -> list[Expr]:
    """
    Create multiple ``variable`` expressions with the provided names and numeric set.

    Args:
      names: List of string names for the variables.
      set: Classification of the symbols, an instance of :class:`wrenfold.enumerations.NumberSet`.

    Returns:
      A list of ``variable`` expressions.

    Examples:
      >>> x0, = sym.make_symbols(['x'])
      >>> print(x0)
      x
      >>> x0.type_name
      'Variable'
      >>> x, y = sym.make_symbols(['x', 'y'], set=NumberSet.Complex)
      >>> x.is_identical_to(y)
      False
      >>> x, y = sym.make_symbols('x', 'y', set=NumberSet.Complex) # Alternative invocation using *args.
      >>> print(x + y)
      x + y

    Raises:
      :class:`wrenfold.exceptions.InvalidArgumentError`: If any variable name is an empty string.
    """

@overload
def make_symbols(*args, set: pywrenfold.enumerations.NumberSet = pywrenfold.enumerations.NumberSet.Unknown) -> list[Expr]:
    """
    Overload of :func:`wrenfold.sym.make_symbols` that accepts a variadic argument list.
    """

def symbols(names: str | Iterable[str] | Iterable[Iterable[str]], real: bool = False, positive: bool = False, nonnegative: bool = False, complex: bool = False) -> Any:
    """
    Create instances of ``variable`` expressions with the provided names. The argument ``names`` may be
    either:

    #. A single string containing multiple names separated by whitespace or commas. The string will be
       split into parts - each of which becomes a variable. If the string contains a single element, a
       single ``sym.Expr`` will be returned.
    #. An iterable of strings. In this case, each string will be processed per the previous rule and a
       list of ``sym.Expr`` will be returned. This rule is applied recursively, such that
       ``Iterable[Iterable[str]]`` will produce return type ``List[List[sym.Expr]]``.

    Args:
      names: String or nested iterable of strings indicating the variable names.
      real: Indicate the symbols are real-valued.
      positive: Indicate the symbols are positive and real-valued.
      nonnegative: Indicate the symbols are non-negative and real-valued.
      complex: Indicate the symbols are complex.

    Returns:
      A ``variable`` expression, or a nested iterable of ``variable`` expressions.

    Examples:
      >>> sym.symbols('x') # Creating a single symbol.
      x
      >>> sym.symbols('x, y') # Multiple symbols from a single string.
      [x, y]
      >>> sym.symbols([('w', 'x'), ('y', 'z')]) # Nested iterable of symbols.
      [[w, x], [y, z]]

    Caution:
      Prefer using :func:`wrenfold.sym.symbol` or :func:`wrenfold.sym.make_symbols`, which have more
      meaningful return type annotations. Because of the highly permissive treatment of the input arg
      ``names``, this function can only be annotated with a return type of `typing.Any` - which reduces
      the effectiveness of type checking.
    """

def integer(value: int) -> Expr:
    """
    Create a constant expression from an integer.

    Tip:
      Typically you do not need to explicitly call this method, since python integers involved in
      mathematical operations will automatically be promoted to ``sym.Expr``::

        y = x + 3 # Equivalent to: y = x + sym.integer(3)

    Args:
      value: A python integer.

    Returns:
      An ``integer_constant`` expression representing the argument.

    Raises:
      TypeError: If the input argument exceeds the range of a signed 64-bit integer.
    """

def float_constant(value: float) -> Expr:
    """
    Create a constant expression from a float.

    Tip:
      Typically you do not need to explicitly call this method, since python floats involved in
      mathematical operations will automatically be promoted to ``sym.Expr``::

        y = x + 2.87 # Equivalent to: y = x + sym.float_constant(2.87)

    Args:
      value: A python float.

    Returns:
      * If the argument is infinite, a ``complex_infinity`` expression.
      * If the argument is ``nan``, an ``undefined`` expression.
      * Otherwise, a ``float_constant`` expression.
    """

def rational(n: int, d: int) -> Expr:
    """
    Create a constant expression from the quotient of two integers. The rational will be simplified by
    dividing out the greatest common divisor.

    Args:
      n: The numerator.
      d: The denominator.

    Returns:
      * If the quotient simplifies to an integer, an `integer_constant` expression.
      * Otherwise, a ``rational_constant`` expression.

    Raises:
      :class:`wrenfold.exceptions.ArithmeticError`: If ``d`` is zero.

    Examples:
      >>> sym.rational(5, 10) # Equivalent to: sym.integer(5) / 10
      1/2
      >>> sym.rational(-2, 7)
      -2/7
    """

def unique_symbols(count: int, real: bool = False, positive: bool = False, nonnegative: bool = False, complex: bool = False) -> Expr | list[Expr]:
    """
    Create instances of ``variable`` expressions with unique identities that can never be accidentally
    re-used. This is useful if you need temporary variables that will later be replaced by substitution,
    and want to be certain that their names do not conflict with other symbols.

    Args:
      count: Number of symbols to create. If one, a single expression is returned. If greater than one,
        a list of expressions is returned.

    Examples:
      >>> sym.unique_symbols(count=2)
      [$u_1, $u_2]
      >>> sym.unique_symbols(count=1)
      $u_3
    """

def compare(a: Expr, b: Expr) -> int:
    """
    Determine relative ordering of two scalar-valued expressions. Note that this is *not* a
    mathematical ordering. Expressions are first ordered by their expression type, and then by the
    contents of the underlying concrete expressions. For non-leaf expressions (for instance
    addition or multiplication), a lexicographical ordering of the children is used to determine
    relative order.

    Args:
      a: The first expression.
      b: The second expression.

    Returns:
      * ``-1`` if ``a`` belongs before ``b``.
      * ``0`` if ``a`` is identical to ``b``.
      * ``+1`` if ``a`` belongs after ``b``.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.compare(x, y)
      -1
      >>> sym.compare(y, x)
      1
      >>> sym.compare(5, 8)
      -1
      >>> sym.compare(sym.sin(x), sym.cos(x))
      1
    """

def log(arg: Expr) -> Expr:
    r"""
    The natural logarithm :math:`\ln{x}`.

    Args:
      arg: Argument to the logarithm.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.log(x + 3)
      log(x + 3)
      >>> sym.log(sym.E)
      1
      >>> sym.log(1)
      0
    """

def exp(arg: Expr) -> Expr:
    r"""
    The exponential function :math:`e^{x}` or :math:`\exp{x}`.

    Args:
      arg: Argument to the exponential function.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.exp(x + 3)
      exp(x + 3)
      >>> sym.exp(0)
      1
      >>> sym.exp(1)
      sym.E
    """

def pow(base: Expr, exp: Expr) -> Expr:
    r"""
    Construct a power expression: :math:`\text{pow}\left(x, y\right) \rightarrow x^y`.

    ``pow`` will attempt to apply simplifications where possible. Some common cases include:

    * The inputs are numerical values and can be evaluated immediately.
    * Various undefined forms: :math:`b^{\tilde{\infty}}`, :math:`{\tilde{\infty}}^0`, :math:`0^0`.
    * Distribution of integer powers: :math:`\left(x \cdot y\right)^n \rightarrow x^n \cdot y^n`
    * Collapsing of powers, when appropriate: :math:`\left(\sqrt{x}\right)^2 \rightarrow x`
    * Interactions with complex infinity: :math:`\tilde{\infty}^n \rightarrow \tilde{\infty}`,
      :math:`\tilde{\infty}^{-n} \rightarrow 0`, :math:`0^{-n} \rightarrow \tilde{\infty}`.

    Args:
      base: Base of the power.
      exp: Exponent of the power.

    Returns:
      A symbolic expresssion corresponding to ``base ** exp``. The expression need not have underlying
      type ``power``, as a simplification may have occurred.

    Examples:
      >>> sym.pow(3, 2)
      9
      >>> sym.pow(24, sym.rational(1, 2))
      2*6**(1/2)
      >>> sym.pow(0, -1)
      nan
      >>> sym.pow(sym.zoo, -1)
      0
      >>> x, w = sym.make_symbols('x', 'w')
      >>> sym.pow(x * w, 2)
      x**2*w**2
      >>> sym.pow(sym.sqrt(x), 2)
      x
      >>> sym.sqrt(sym.pow(x, 2))
      (x**2)**(1/2)
    """

def cos(arg: Expr) -> Expr:
    r"""
    The cosine function :math:`\cos{x}`.

    Args:
      arg: Argument in radians.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.cos(x)
      cos(x)
      >>> sym.cos(sym.pi)
      -1
    """

def sin(arg: Expr) -> Expr:
    r"""
    The sine function :math:`\sin{x}`.

    Args:
      arg: Argument in radians.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.sin(x)
      sin(x)
      >>> sym.sin(0)
      0
    """

def tan(arg: Expr) -> Expr:
    r"""
    The tangent function :math:`\tan{x}`.

    Args:
      arg: Argument in radians.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.tan(x)
      tan(x)
      >>> sym.tan(sym.pi/2)
      zoo
    """

def acos(arg: Expr) -> Expr:
    r"""
    The inverse cosine function :math:`\cos^{-1}{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.acos(x)
      acos(x)
      >>> sym.acos(0)
      pi/2
    """

def asin(arg: Expr) -> Expr:
    r"""
    The inverse sine function :math:`\sin^{-1}{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.asin(x)
      asin(x)
      >>> sym.asin(-1)
      -pi/2
    """

def atan(arg: Expr) -> Expr:
    r"""
    The inverse tangent function :math:`\tan^{-1}{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.atan(x)
      atan(x)
      >>> sym.atan(-x)
      -atan(x)
    """

def cosh(arg: Expr) -> Expr:
    r"""
    The hyperbolic cosine function :math:`\cosh{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.cosh(x)
      cosh(x)
      >>> sym.cosh(x * sp.I)
      cos(x)
    """

def sinh(arg: Expr) -> Expr:
    r"""
    The hyperbolic sine function :math:`\sinh{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.sinh(x)
      sinh(x)
      >>> sym.sinh(x * sp.I)
      I*sin(x)
    """

def tanh(arg: Expr) -> Expr:
    r"""
    The hyperbolic tangent function :math:`\tanh{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.tanh(x)
      tanh(x)
      >>> sym.tanh(x * sp.I)
      I*tan(x)
    """

def acosh(arg: Expr) -> Expr:
    r"""
    The inverse hyperbolic cosine function :math:`\cosh^{-1}{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.acosh(x)
      acosh(x)
    """

def asinh(arg: Expr) -> Expr:
    r"""
    The inverse hyperbolic sine function :math:`\sinh^{-1}{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.asinh(x)
      asinh(x)
    """

def atanh(arg: Expr) -> Expr:
    r"""
    The inverse hyperbolic tangent function :math:`\tanh^{-1}{x}`.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.atanh(x)
      atanh(x)
    """

def sqrt(arg: Expr) -> Expr:
    r"""
    The square root function :math:`\sqrt{x}`. This is an alias for ``sym.pow(x, sym.rational(1, 2))``.
    """

def abs(arg: Expr) -> Expr:
    r"""
    The absolute value function :math:`\lvert x \rvert`.

    Examples:
      >>> sym.abs(3)
      3
      >>> x = sym.symbol('x')
      >>> sym.abs(x)
      abs(x)
      >>> sym.abs(x).diff(x)
      x/abs(x)
    """

def sign(arg: Expr) -> Expr:
    r"""
    The sign/signum function :math:`\text{sign}\left(x\right)`, defined as:

    .. math::
      \text{sign}\left(x\right) = \begin{cases}
      -1 & x \lt 0   \\
       0 & x  = 0    \\
       1 & x \gt 0
      \end{cases}

    Tip:
      Like other functions for which no finite derivative exists, by default wrenfold will assume
      :math:`\frac{\partial}{\partial x}\text{sign}\left(x\right) = 0`. See
      :func:`wrenfold.sym.Expr.diff` for alternative behavior.

    Caution:
      Some floating-point implementations of ``sign`` have the behavior ``sign(+0.0) = 1`` and
      ``sign(-0.0) = -1``. The  symbolic ``sign`` function in wrenfold produces zero for all numerical
      zero-valued inputs.

    Returns:
      * If the input can be immediately evaluated, one of: ``[-1, 0, 1]``.
      * Otherwise, a ``function`` expression.

    Examples:
      >>> x = sym.symbol('x')
      >>> sym.sign(x)
      sign(x)
      >>> sym.sign(x).diff(x)
      0
      >>> sym.sign(-0.78231)
      -1
    """

def floor(arg: Expr) -> Expr:
    r"""
    The floor function, sometimes written as :math:`\lfloor x \rfloor` or
    :math:`\text{floor}\left(x\right)`. This function returns the largest integer such that
    :math:`\lfloor x \rfloor \le x`.

    Returns:
      * If the input can be immediately evaluated, an integer constant.
      * Otherwise, a ``function`` expression.

    Examples:
       >>> sym.floor(6.7)
       6
       >>> sym.floor(-3.1)
       -4
       >>> sym.floor(sym.rational(-5, 40))
       -2
       >>> x = sym.symbol('x')
       >>> sym.floor(x)
       floor(x)
    """

def atan2(y: Expr, x: Expr) -> Expr:
    r"""
    Two-argument inverse tangent function :math:`\text{atan2}\left(y, x\right)`. Returns the angle
    :math:`\theta \in [-\pi, \pi]` such that:

    .. math::
      \begin{align}
       x &= \sqrt{x^2 + y^2}\cdot\cos\theta \\
       y &= \sqrt{x^2 + y^2}\cdot\sin\theta
      \end{align}

    Examples:
      >>> sym.atan2(1, 0)
      pi/2
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.atan2(y, x).diff(x)
      -y/(x**2 + y**2)
    """

def max(a: Expr, b: Expr) -> Expr:
    r"""
    The maximum of two scalar values :math:`\text{max}\left(a, b\right)`, defined as:

    .. math::
      \text{max}\left(a, b\right) = \begin{cases}
      b & a \lt b \\
      a & a \ge b
      \end{cases}

    Returns:
      * If :math:`a \lt b` can be immediately evaluated, the larger of ``a`` and ``b`` will be returned.
      * Otherwise, a ``sym.where`` expression.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.max(x, y)
      where(x < y, y, x)
      >>> sym.max(2, 3)
      3
    """

def min(a: Expr, b: Expr) -> Expr:
    r"""
    The minimum of two scalar values :math:`\text{min}\left(a, b\right)`, defined as:

    .. math::
      \text{min}\left(a, b\right) = \begin{cases}
      b & b \lt a \\
      a & a \ge b
      \end{cases}

    Returns:
      * If :math:`a \lt b` can be immediately evaluated, the smaller of ``a`` and ``b`` will be returned.
      * Otherwise, a ``sym.where`` expression.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.min(x, y)
      where(y < x, y, x)
      >>> sym.min(sym.rational(2, 3), sym.rational(3, 4))
      2/3
    """

@overload
def where(c: BooleanExpr, a: Expr, b: Expr) -> Expr:
    r"""
    ``where(c, a, b)`` will select between either ``a`` or ``b``, depending on the boolean-valued
    expression ``c``. When ``c`` is true, ``a`` is returned, otherwise ``b`` is returned:

    .. math::
      \text{where}\left(c, a, b\right) = \begin{cases}
      a & c = \text{true} \\
      b & c = \text{false}
      \end{cases}

    In generated code, conditional expressions are converted to if-else statements.

    Tip:
      When differentiated, the discontinuity introduced by the conditional is ignored. The derivatives
      of ``a`` and ``b`` are computed, and a new conditional statement is formed.

    Args:
      c: The boolean-valued condition used to switch between values.
      a: Returned if ``c`` is true.
      b: Returned if ``c`` is false.

    Returns:
      * If the truthiness of ``c`` can be immediately evaluated, ``a`` or ``b`` is returned directly.
      * Otherwise, a ``conditional`` expression.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.where(x > y, sym.cos(x), sym.sin(y))
      where(y < x, cos(x), sin(y))
      >>> sym.where(x > y, sym.pi * x, 0).subs(x, 3).subs(y, 2)
      3*pi
      >>> sym.where(x > y, 4*x**3, 2*y).diff(x)
      where(y < x, 12*x**2, 0)
    """

@overload
def where(c: BooleanExpr, a: MatrixExpr, b: MatrixExpr) -> MatrixExpr:
    """
    Overload of :func:`wrenfold.sym.where` for matrix arguments. Accepts two identically-shaped matrices
    ``a, b`` and a boolean-valued expression ``c``.

    Args:
      c: Boolean-valued expression.
      a: Matrix whose elements are selected when ``c`` is True.
      b: Matrix whose elements are selected when ``c`` is False.

    Returns:
      A new matrix where element ``[i, j]`` evaluates to ``sym.where(c, a[i, j], b[i, j])``.

    Raises:
      wrenfold.sym.DimensionError: If the dimensions of ``a`` and ``b`` do not match.
    """

def lt(a: Expr, b: Expr) -> BooleanExpr:
    r"""
    Boolean-valued relational expression :math:`a \lt b`, or ``<`` operator.

    Examples:
      >>> sym.lt(2, 3)
      True
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.lt(x, y)
      x < y
    """

def le(a: Expr, b: Expr) -> BooleanExpr:
    r"""
    Boolean-valued relational expression :math:`a \le b`, or ``<=`` operator.

    Examples:
      >>> sym.le(1, sym.rational(3, 2))
      True
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.le(x, y)
      x <= y
    """

def gt(a: Expr, b: Expr) -> BooleanExpr:
    r"""
    Boolean-valued relational expression :math:`a \gt b`, or ``>`` operator. ``a > b`` will be
    automatically canonicalized to ``b < a``.

    Examples:
      >>> sym.gt(2.12, 2)
      True
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.gt(x, y)
      y < x
    """

def ge(a: Expr, b: Expr) -> BooleanExpr:
    r"""
    Boolean-valued relational expression :math:`a \ge b`, or ``>=`` operator. ``a >= b`` will be
    automatically canonicalized to ``b <= a``.

    Examples:
      >>> sym.ge(2, 2)
      True
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.ge(x, y)
      y <= x
    """

def eq(a: Expr, b: Expr) -> BooleanExpr:
    """
    Boolean-valued relational expression :math:`a = b`, or ``==`` operator.

    Examples:
      >>> sym.eq(2, 5)
      False
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.eq(3*x, y/2)
      3*x == y/2
    """

def iverson(arg: BooleanExpr) -> Expr:
    r"""
    Cast a boolean expression to an integer scalar expression. The iverson bracket of boolean-valued
    argument :math:`P` is defined as:

    .. math::
      \text{iverson}\left(P\right) = \begin{cases}
      1 & P = \text{true} \\
      0 & P = \text{false}
      \end{cases}

    The iverson bracket can be used to convert ``sym.BooleanExpr`` into ``sym.Expr``.

    Returns:
      * Boolean constants ``sym.true`` and ``sym.false`` are converted to one and zero directly.
      * Otherwise, an ``iverson_bracket`` expression is returned.

    Examples:
      >>> sym.iverson(sym.integer(1) < 3)
      1
    """

def unevaluated(arg: Expr) -> Expr:
    """
    Wrap a scalar-valued expression to prevent automatic simplifications/combinations in downstream
    operations. This is similar in intent to SymPy's ``UnevaluatedExpr``.

    Args:
      arg: Scalar-valued expression to wrap.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> f = sym.unevaluated(x * y) * y
      >>> f # Products are not combined automatically.
      y*(x*y)
      >>> f.diff(y) # Derivatives retain the parentheses.
      y*(x) + (x*y)
    """

def stop_derivative(arg: Expr) -> Expr:
    """
    Wrap a scalar-valued expression in order to block propagation of derivatives. ``stop_derivative``
    acts like a function whose derivative is always zero.

    Args:
      arg: Scalar-valued expression to wrap.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> f = sym.stop_derivative(x * y) * y
      >>> f.diff(x)
      0
      >>> f.diff(y)
      StopDerivative(x * y)
    """

@overload
def eliminate_subexpressions(expr: Expr, make_variable: Callable[[int], Expr] | None, min_occurrences: int = 2) -> tuple[Expr, list[tuple[Expr, Expr]]]:
    """
    Extract common subexpressions from a scalar-valued expression. The expression tree is traversed and
    unique expressions are counted. Those that appear ``min_occurrences`` or more times are replaced
    with a variable.

    This is *not* the same algorithm used during code-generation. Rather, it is a simplified version
    that pulls out atomic expressions. Additions and multiplications will not be broken down into
    smaller pieces in order to reduce the operation count. The intended use case is to allow a human to
    more easily inspect a complex nested expression by breaking it into pieces.

    Args:
      expr: The expression to operate on.
      make_variable: A callable that accepts an integer indicating the index of the next variable. It
        should return an appropriately named variable expression. By default, the names
        ``v0, v1, ..., v{N}`` will be used.
      min_occurrences: The number of times an expression must appear as part of a unique parent in order
        to be extracted.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> f = sym.cos(x * y) - sym.sin(x * y) + 5 * sym.abs(sym.cos(x * y))
      >>> g, replacements = sym.eliminate_subexpressions(f)
      >>> replacements
      [(v0, x*y), (v1, cos(v0))]
      >>> g
      v1 + 5*abs(v1) - sin(v0)
    """

@overload
def eliminate_subexpressions(expr: MatrixExpr, make_variable: Callable[[int], Expr] | None = None, min_occurences: int = 2) -> tuple[MatrixExpr, list[tuple[Expr, Expr]]]:
    """Matrix-valued overload."""

E: Expr = ...

pi: Expr = ...

zoo: Expr = ...

one: Expr = ...

zero: Expr = ...

imaginary_unit: Expr = ...

I: Expr = ...

nan: Expr = ...

def addition(args: Sequence[Expr]) -> Expr:
    """Construct addition expression from provided operands."""

def multiplication(args: Sequence[Expr]) -> Expr:
    """Construct multiplication expression from provided operands."""

class Function:
    """
    A scalar-valued symbolic function. Used to construct expressions of undefined functions.
    """

    def __init__(self, name: str) -> None:
        """
        Declare a new symbolic function with the provided string name. Two functions with the same name are
        considered equivalent.

        Args:
          name: Function name.

        Examples:
          >>> x, y = sym.make_symbols('x', 'y')
          >>> f = sym.Function('f')
          >>> f(x)
          f(x)
          >>> f(x, y ** 2).diff(x)
          Derivative(f(x, y**2), x)

        Raises:
          :class:`wrenfold.exceptions.InvalidArgumentError`: If the input string is empty.
        """

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: Function) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: Function) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def name(self) -> str:
        """Name of the function."""

    def __repr__(self) -> str: ...

    def __call__(self, *args) -> Expr:
        """
        Invoke the symbolic function with the provided scalar expressions, and return a new scalar expression.
        """

def substitution(input: Expr, target: Expr, replacement: Expr) -> Expr:
    """
    Create a deferred substitution expression.

    Args:
      input: The expression that would be modified by the substitution.
      target: The target expression being replaced.
      replacement: The substituted expression.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> f = sym.substitution(x**2 + y, y, sym.cos(x))
      >>> print(f)
      Subs(y + x**2, y, cos(x))
      >>> f.args
      (y + x**2, y, cos(x))

    Raises:
      :class:`wrenfold.exceptions.TypeError`: If ``target`` is a numeric constant.
    """

def derivative(function: Expr, arg: Expr, order: int = 1) -> Expr:
    """
    Create a deferred derivative expression. This expression type is used to represent the derivatives
    of abstract symbolic functions.

    Args:
      function: Function to be differentiated.
      arg: Argument with respect to which the derivative is taken.
      order: The order of the derivative.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.derivative(x * x, x)
      Derivative(x**2, x)
      >>> f = sym.Function('f')
      >>> f(x, y).diff(x)
      Derivative(f(x, y), x)

    Raises:
      :class:`wrenfold.exceptions.TypeError`: If ``arg`` is not a variable expression.
      :class:`wrenfold.exceptions.InvalidArgumentError`: If ``order <= 0``.
    """

def get_variables(expr: Expr) -> list[Expr]:
    """
    Retrieve all variable expressions from a symbolic expression tree, and return them in a list.

    Args:
      expr: A scalar-valued expression.

    Returns:
      A list of scalar expressions of underlying type ``Variable`, ``UniqueVariable``, and
      ``FuntionArgumentVariable``.

    Examples:
      >>> x, y, z = sym.make_symbols('x', 'y', 'z')
      >>> sym.get_variables(x**2 + y * 3 - sym.cos(z))
      [x, y, z]
      >>> sym.get_variables(x * 3)
      [x]
      >>> vs = sym.get_variables(sym.cos(x * y))
      >>> type(vs[0])
      pywrenfold.sym.Expr
      >>> print(vs[0].type_name)
      Variable
    """

class MatrixExpr:
    """A matrix-valued symbolic expression."""

    def __init__(self, rows: Iterable) -> None:
        """Construct from an iterable of values. See :func:`wrenfold.sym.matrix`."""

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: MatrixExpr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: MatrixExpr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __repr__(self) -> str: ...

    def expression_tree_str(self) -> str:
        """See :func:`wrenfold.sym.Expr.expression_tree_str`."""

    @property
    def type_name(self) -> str:
        """
        Retrieve the name of the underlying C++ expression type. See :func:`wrenfold.sym.Expr.type_name`.
        """

    def diff(self, var: Expr, order: int = 1, use_abstract: bool = False) -> MatrixExpr:
        """
        Differentiate every element of the matrix with respect to the specified variable.

        See :func:`wrenfold.sym.Expr.diff` for meaning of the arguments.

        Returns:
          A new matrix (with the same shape as ``self``) where each element is the derivative of the
          corresponding input element, taken ``order`` times with respect to ``var``.

        Examples:
          >>> x = sym.symbol('x')
          >>> m = sym.matrix([[x, sym.sin(x)], [22, -x**2]])
          >>> m.diff(x)
          [[1, cos(x)], [0, -2*x]]
        """

    def jacobian(self, vars: Sequence[Expr] | MatrixExpr, use_abstract: bool = False) -> MatrixExpr:
        """
        See :func:`wrenfold.sym.jacobian`. Equivalent to ``sym.jacobian(self, vars)``.
        """

    def distribute(self) -> MatrixExpr:
        """Invoke :func:`wrenfold.sym.distribute` on every element of the matrix."""

    @overload
    def subs(self, target: Expr, substitute: Expr) -> MatrixExpr:
        """
        Overload of ``subs`` that performs a single scalar-valued substitution.
        """

    @overload
    def subs(self, target: BooleanExpr, substitute: BooleanExpr) -> MatrixExpr:
        """
        Overload of ``subs`` that performs a single boolean-valued substitution.
        """

    @overload
    def subs(self, pairs: Sequence[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> MatrixExpr:
        """Invoke :func:`wrenfold.sym.subs` on every element of the matrix."""

    def eval(self) -> Annotated[NDArray[numpy.int64], dict(shape=(None, None), order='C')] | Annotated[NDArray[numpy.float64], dict(shape=(None, None), order='C')] | Annotated[NDArray[numpy.complex128], dict(shape=(None, None), order='C')]:
        """
        Invoke :func:`wrenfold.sym.Expr.eval` on every element of the matrix, and return a numpy array
        containing the resulting values.

        Important:
          The resulting matrix must have a single ``dtype``. In order to ensure a consistent type for all
          entries, the following promotion order is applied: ``int64 -> double -> std::complex<double>``.

        Returns:
          ``np.ndarray`` with C-storage order and one of the following data types: ``np.int64``,
          ``np.float64``, or ``np.complex128``.

        Raises:
          :class:`wrenfold.exceptions.TypeError`: If any child expression contains symbolic expressions that
            cannot be converted to a numerical representation.

        Examples:
          >>> sym.matrix([[1, 2], [0.4 * 2 * sym.I, 0]]).eval()
          array([[1.+0.j , 2.+0.j], [0.+0.8j, 0.+0.j]])
        """

    @overload
    def collect(self, var: Expr) -> MatrixExpr:
        """
        Invokes :func:`wrenfold.sym.Expr.collect` on every element of the matrix. This overload accepts a single variable.
        """

    @overload
    def collect(self, var: Sequence[Expr]) -> MatrixExpr:
        """
        Invokes :func:`wrenfold.sym.Expr.collect` on every element of the matrix. This overload accepts a list of variables, and collects recursively in the order they are specified.
        """

    @property
    def shape(self) -> tuple:
        """Shape of the matrix in (row, col) format."""

    @property
    def size(self) -> int:
        """Total number of elements."""

    @property
    def is_empty(self) -> bool:
        """
        True if the matrix empty (either zero rows or cols). This should only occur with empty slices.
        """

    @overload
    def __getitem__(self, row: int) -> Expr | MatrixExpr:
        """Retrieve a row from the matrix."""

    @overload
    def __getitem__(self, row_col: tuple[int, int]) -> Expr:
        """Retrieve a row and column from the matrix."""

    @overload
    def __getitem__(self, slice: slice) -> MatrixExpr:
        """Slice along rows."""

    @overload
    def __getitem__(self, slices: tuple[slice, slice]) -> MatrixExpr:
        """Slice along rows and cols."""

    @overload
    def __getitem__(self, row_and_col_slice: tuple[int, slice]) -> MatrixExpr:
        """Slice a specific row."""

    @overload
    def __getitem__(self, row_slice_and_col: tuple[slice, int]) -> MatrixExpr:
        """Slice a specific column."""

    def __array__(self, **kwargs) -> Annotated[NDArray[numpy.int64], dict(shape=(None, None), order='C')] | Annotated[NDArray[numpy.float64], dict(shape=(None, None), order='C')] | Annotated[NDArray[numpy.complex128], dict(shape=(None, None), order='C')]:
        """
        Convert to numpy array. See :func:`wrenfold.sym.MatrixExpr.eval` for limitations.
        """

    def __len__(self) -> int:
        """Number of rows in the matrix."""

    def __iter__(self) -> Iterator[Expr | MatrixExpr]:
        """Iterate over rows in the matrix."""

    def unary_map(self, func: Callable[[Expr], Expr]) -> MatrixExpr:
        """
        Perform a unary map operation on ``self``. The provided callable ``func`` is invoked on every
        element of the matrix (traversing in row-major order). The returned values are used to create a new
        matrix with the same shape as the input.

        Args:
          func: A callable that accepts and returns ``sym.Expr``.

        Returns:
          The mapped matrix.

        Examples:
          >>> x, y = sym.make_symbols('x', 'y')
          >>> m = sym.matrix([[x, y], [2, sym.pi]])
          >>> m.unary_map(lambda v: v * 2)
          [[2*x, 2*y], [4, 2*pi]]
        """

    @overload
    def reshape(self, rows: int, cols: int) -> MatrixExpr:
        """
        Reshape a matrix and return a copy with the new shape. The underlying order (row-major) of elements
        in ``self`` is unchanged. The expressions are simply copied into a new matrix with the requested
        number of rows and columns. The total number of elements must remain unchanged.

        Args:
          rows: Number of rows in the output.
          cols: Number of columns in the output.

        Returns:
          A new matrix with shape ``(rows, cols)``.

        Raises:
          wrenfold.sym.DimensionError: If the specified dimensions are invalid.

        Examples:
          >>> sym.matrix([1, 2, 3, 4]).reshape(2, 2)
          [[1, 2], [3, 4]]
        """

    @overload
    def reshape(self, shape: tuple[int, int]) -> MatrixExpr:
        """Overload of ``reshape`` that accepts a (row, col) tuple."""

    def col_join(self, other: MatrixExpr) -> MatrixExpr:
        """
        Vertically stack ``self`` and ``other``.

        Args:
          other: Matrix to stack below ``self``.

        Raises:
          wrenfold.sym.DimensionError: If the number of columns does not match.
        """

    def row_join(self, other: MatrixExpr) -> MatrixExpr:
        """
        Horizontally stack ``self`` and ``other``.

        Args:
          other: Matrix to stack to the right of ``self``.

        Raises:
          wrenfold.sym.DimensionError: If the number of rows does not match.
        """

    def to_list(self) -> list:
        """Convert to a list of lists."""

    def to_flat_list(self) -> list[Expr]:
        """
        Convert to a flat list assembled in the storage order (row-major) of the matrix.
        """

    def transpose(self) -> MatrixExpr:
        """
        Transpose the matrix by swapping rows and columns. If ``self`` has dimensions ``(N, M)``, the result
        will be an ``(M, N)`` matrix where element ``[i, j]`` is ``self[j, i]``.

        Returns:
          The transposed matrix.

        Examples:
          >>> x, y = sym.make_symbols('x', 'y')
          >>> m = sym.matrix([[x, y*2], [sym.cos(x), 0]])
          >>> m
          [[x, 2*y], [cos(x), 0]]
          >>> m.transpose()
          [[x, cos(x)], [2*y, 0]]
        """

    @property
    def T(self) -> MatrixExpr:
        """Alias for :func:`wrenfold.sym.MatrixExpr.transpose`."""

    def squared_norm(self) -> Expr:
        """
        The sum of squared elements of ``self``, or the squared L2 norm.

        Returns:
          A scalar-valued expression.

        Examples:
          >>> x, y = sym.make_symbols('x', 'y')
          >>> m = sym.matrix([[x, y], [2, 0]])
          >>> m.squared_norm()
          4 + x**2 + y**2
        """

    def norm(self) -> Expr:
        """
        The L2 norm of the matrix, or square root of :func:`wrenfold.sym.MatrixExpr.squared_norm`.
        """

    def det(self) -> Expr:
        """Alias for :func:`wrenfold.sym.det`."""

    def __add__(self, arg: MatrixExpr, /) -> MatrixExpr: ...

    def __sub__(self, arg: MatrixExpr, /) -> MatrixExpr: ...

    @overload
    def __mul__(self, arg: MatrixExpr, /) -> MatrixExpr: ...

    @overload
    def __mul__(self, arg: Expr, /) -> MatrixExpr: ...

    @overload
    def __mul__(self, arg: int, /) -> MatrixExpr: ...

    @overload
    def __mul__(self, arg: float, /) -> MatrixExpr: ...

    @overload
    def __rmul__(self, arg: Expr, /) -> MatrixExpr: ...

    @overload
    def __rmul__(self, arg: int, /) -> MatrixExpr: ...

    @overload
    def __rmul__(self, arg: float, /) -> MatrixExpr: ...

    @overload
    def __truediv__(self, arg: Expr, /) -> MatrixExpr: ...

    @overload
    def __truediv__(self, arg: int, /) -> MatrixExpr: ...

    @overload
    def __truediv__(self, arg: float, /) -> MatrixExpr: ...

    def __neg__(self) -> MatrixExpr:
        """Element-wise negation of the matrix."""

    def __bool__(self) -> None: ...

def eye(rows: int, cols: int | None = None) -> MatrixExpr:
    """
    Create an identity matrix.

    Args:
      rows: Number of rows in the resulting matrix.
      cols: Number of columns. By default this will match the number of rows.

    Raises:
      wrenfold.sym.DimensionError: If the dimensions are non-positive.
    """

def zeros(rows: int, cols: int) -> MatrixExpr:
    """
    Create a matrix of zeros with the specified shape.

    Args:
      rows: Number of rows. Must be positive.
      cols: Number of cols. Must be positive.

    Returns:
      A ``(rows, cols)`` matrix of zeroes.

    Raises:
      wrenfold.sym.DimensionError: If the dimensions are non-positive.

    Examples:
      >>> sym.zeros(2, 3)
      [[0, 0, 0], [0, 0, 0]]
    """

def vector(*args) -> MatrixExpr:
    """
    Create a column-vector from the provided arguments.

    Args:
      args: The elements of the vector.

    Returns:
      A ``(N, 1)`` column vector, where ``N`` is the number of args.

    Raises:
      wrenfold.sym.DimensionError: If no arguments are provided.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.vector(x * 2, 0, y + 3)
      [[2*x], [0], [3 + y]]
    """

def row_vector(*args) -> MatrixExpr:
    """
    Create a row-vector from the provided arguments.

    Args:
      args: The elements of the vector.

    Returns:
      A ``(1, M)`` row vector, where ``M`` is the number of args.

    Raises:
      wrenfold.sym.DimensionError: If no arguments are provided.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.row_vector(x * 2, 0, y + 3)
      [[2*x, 0, 3 + y]]
    """

def matrix(rows: Iterable) -> MatrixExpr:
    """
    Construct a matrix from an iterator over rows. Inputs are vertically concatenated to create a matrix
    expression.

    Args:
      rows: Either ``Iterable[sym.Expr]`` or ``Iterable[Iterable[sym.Expr]]``.

    Tip:
      If the input is an iterable of expressions, the result will have column-vector shape.

    Returns:
      * A matrix of expressions.

    Raises:
      wrenfold.sym.DimensionError: If the input is empty, or the number of columns is inconsistent
        between rows.

    Examples:
      >>> sym.matrix([1,2,3])
      [[1], [2], [3]]
      >>> sym.matrix([(1,2,3), (x, y, z)])
      [[1, 2, 3], [x, y, z]]
      >>> def yield_rows():
      >>>   yield sym.matrix([sym.pi, x, 0]).T
      >>>   yield sym.matrix([-2, y**2, sym.cos(x)]).T
      >>> sym.matrix(yield-rows())
      [[pi, x, 0], [-2, y**2, cos(x)]]
    """

def matrix_of_symbols(prefix: str, rows: int, cols: int) -> MatrixExpr:
    """
    Construct a matrix of ``variable`` expressions with the provided prefix.

    Args:
      prefix: String prefix for variable names. Variables will be named ``{prefix}_{row}_{col}``.
      rows: Number of rows.
      cols: Number of cols.

    Returns:
      A ``(rows, cols)`` shaped matrix filled with ``variable`` expressions.

    Raises:
      wrenfold.sym.DimensionError: If the dimensions are non-positive.
    """

def hstack(values: Sequence[MatrixExpr]) -> MatrixExpr:
    """
    Horizontally (joining rows) stack the provided matrices. Number of rows must be the same for every
    input.

    Raises:
      wrenfold.sym.DimensionError: If there is a dimension mismatch, or the input is empty.
    """

def vstack(values: Sequence[MatrixExpr]) -> MatrixExpr:
    """
    Vertically (joining columns) stack the provided matrices. Number of columns must be the same for
    every input.

    Raises:
      wrenfold.sym.DimensionError: If there is a dimension mismatch, or the input is empty.
    """

@overload
def diag(values: Sequence[MatrixExpr]) -> MatrixExpr:
    """
    Diagonally stack the provided matrices. Off-diagonal blocks are filled with zeroes.

    Raises:
      wrenfold.sym.DimensionError: If the input is empty.

    Examples:
      >>> x, y, z = sym.make_symbols('x', 'y', 'z')
      >>> m = sym.matrix([[x, 0], [y*z, 5]])
      >>> sym.diagonal([m, sym.eye(2)])
      [[x, 0, 0, 0], [y*z, 5, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
    """

@overload
def diag(values: Sequence[Expr]) -> MatrixExpr:
    """Overload of :func:`wrenfold.sym.diag` that accepts a list of scalars."""

def vec(m: MatrixExpr) -> MatrixExpr:
    """
    Vectorize matrix in column-major order.

    Args:
      m: Matrix to flatten.

    Returns:
      New column-vector matrix formed by vertically stacking the columns of ``m``.
    """

def det(m: MatrixExpr) -> Expr:
    """
    Compute the determinant of the matrix. For 2x2 and 3x3 matrices, a hardcoded formula is employed.
    For sizes 4x4 and above, the matrix is first decomposed via full-pivoting LU decomposition.

    Caution:
      When computing the LU decomposition, the pivot selection step cannot know apriori which symbolic
      expressions *might* evaluate to zero at runtime. As a result, the decomposition ordering could
      produce ``NaN`` values at runtime when numerical values are substituted into this expression.

    Returns:
      The matrix determinant as a scalar-valued expression.

    Raises:
      wrenfold.sym.DimensionError: If the matrix is not square.

    Examples:
      >>> x = sym.symbol('x')
      >>> m = sym.matrix([[sym.cos(x), -sym.sin(x)], [sym.sin(x), sym.cos(x)]])
      >>> m.det()
      cos(x)**2 + sin(x)**2

    References:
      * `Matrix determinant <https://en.wikipedia.org/wiki/Determinant>`_.
    """

def full_piv_lu(m: MatrixExpr) -> tuple[MatrixExpr, MatrixExpr, MatrixExpr, MatrixExpr]:
    """Factorize a matrix using complete pivoting LU decomposition."""

def jacobian(functions: Sequence[Expr] | MatrixExpr, vars: Sequence[Expr] | MatrixExpr, use_abstract: bool = False) -> MatrixExpr:
    """
    Compute the Jacobian of a vector-valued function with respect to multiple variables.

    Given ``functions`` of length ``N`` and ``vars`` of length ``M``, this method produces an ``(N, M)``
    shaped matrix where element ``[i, j]`` is the partial derivative of ``functions[i]`` taken with
    respect to ``vars[j]``.

    Args:
      functions: An iterable of scalar-valued expressions to differentiate.
      vars: An iterable of ``variable`` or ``compound_expression_element`` expressions.
      use_abstract: See :func:`wrenfold.sym.Expr.diff`.

    Returns:
      The Jacobian matrix of shape ``(N, M)``.

    Raises:
      sym.DimensionError: If the dimensions of ``functions`` or ``vars`` are invalid.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> J = sym.jacobian([x + 1, sym.sin(y*x), x*y**2], [x, y])
      >>> J.shape
      (3, 2)
      >>> J
      [[1, 0], [y*cos(x*y), x*cos(x*y)], [y**2, 2*x*y]]

    References:
      * `Jacobian matrix <https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant>`_
    """

class CompoundExpr:
    """
    A compound expression is an instance of an aggregate type with members that have been initialized
    from symbolic expressions. It is used to represent a user-provided struct in the symbolic expression
    tree. This enables a couple of functionalities:

      * User types can be passed as input and output arguments from generated functions.
      * User types can be passed as inputs and outputs from external (non-generated) functions that are
        invoked from within a generated function.
    """

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: CompoundExpr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: CompoundExpr) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def type_name(self) -> str:
        """
        Retrieve the name of the underlying C++ expression type. See :func:`wrenfold.sym.Expr.type_name`.
        """

    def __repr__(self) -> str: ...

    def expression_tree_str(self) -> str:
        """See :func:`wrenfold.sym.Expr.expression_tree_str`."""

    def __bool__(self) -> None: ...

def create_compound_expression_elements(provenance: CompoundExpr, num: int) -> list[Expr]:
    """
    Create scalar expressions that represent the members of the provided compound expression. OMIT_FROM_SPHINX
    """

def create_custom_type_construction(type: pywrenfold.type_info.CustomType, expressions: Sequence[Expr]) -> CompoundExpr:
    """
    Create compound expression of type `CustomTypeConstruction`. OMIT_FROM_SPHINX
    """

def distribute(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr:
    r"""
    Expand the mathematical expression. ``distribute`` will recursively traverse the expression tree and
    multiply out any product of additions and subtractions. For example:

    .. math::
      \left(x + y\right)\cdot(4 - y) \rightarrow 4 \cdot x + 4  \cdot y - x  \cdot y - y^{2}

    Powers of the form :math:`f\left(x\right)^{\frac{n}{2}}` where :math:`n` is an integer are also
    expanded:

    .. math::
      \left(x + 2\right)^{\frac{3}{2}} \rightarrow x \cdot \left(x + 2\right)^{\frac{1}{2}} +
      2 \cdot \left(x + 2\right)^{\frac{1}{2}}

    Returns:
      The input expression after expansion.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> f = (x - 2) * (y + x) * (y - 3)
      >>> f.distribute()
      6*x + 6*y - 5*x*y + x**2*y + x*y**2 - 3*x**2 - 2*y**2
    """

@overload
def subs(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr, pairs: Sequence[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr:
    """
    Traverse the expression tree and replace target expressions with corresponding substitutions. The
    list of replacements is executed *in order*, such that substitutions that appear later in the list
    of ``(target, replacement)`` pairs may leverage the result of earlier ones.

    Args:
      pairs: A list of tuples. Each tuple contains a ``(target, replacement)`` pair, where the *target*
        is the expression to find and the *replacement* is the expression to substitute in its place.
        The pairs may be scalar-valued or boolean-valued expressions.

    Returns:
      The input expression after performing replacements.

    Examples:
      >>> x, y = sym.make_symbols('x', 'y')
      >>> sym.cos(x).subs([(x, y*2), (y, sym.pi)])
      1
      >>> (sym.pow(x, 2) * y).subs(x * y, 3)
      3*x
      >>> (x + 2*y - 5).subs(x + 2*y, y)
      -5 + y
    """

@overload
def subs(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr, target: Expr, replacement: Expr) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr:
    """
    Overload of ``subs`` that performs a single scalar-valued substitution.
    """

@overload
def subs(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr, target: BooleanExpr, replacement: BooleanExpr) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr:
    """
    Overload of ``subs`` that performs a single boolean-valued substitution.
    """
