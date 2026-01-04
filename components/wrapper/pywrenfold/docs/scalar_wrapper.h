// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <string_view>

namespace wf::docstrings {

inline constexpr std::string_view scalar_expr_expression_tree_str = R"doc(
Recursively traverses the expression tree and creates a pretty-printed string
describing the expression. This can be a useful representation for diffing math
expression trees.

Returns:
 String representation of the expression tree.

Example:
  >>> x, y = sym.symbols('x, y')
  >>> print((x*y + 5).expression_tree_str())
  Addition:
  ├─ Integer (5)
  └─ Multiplication:
    ├─ Variable (x, unknown)
    └─ Variable (y, unknown)
)doc";

inline constexpr std::string_view scalar_expr_type_name = R"doc(
Retrieve the name of the underlying C++ expression type.

Returns:
  String name of the underlying expression type.

Example:
  >>> x, y = sym.symbols('x, y')
  >>> print(x.type_name)
  Variable
  >>> print((x + y).type_name)
  Addition
)doc";

inline constexpr std::string_view scalar_expr_diff = R"doc(
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

  >>> x, y = sym.symbols('x, y')
  >>> f = x * sym.sin(x * y) + 3
  >>> f.diff(x)
  x*y*cos(x*y) + sin(x*y)
  >>> f.diff(x, order=2)
  -x*y**2*sin(x*y) + 2*y*cos(x*y)
  >>> f.diff(y)
  x**2*cos(x*y)
)doc";

inline constexpr std::string_view scalar_expr_eval = R"doc(
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
  >>> x = sym.symbols('x')
  >>> f = sym.cos(sym.pi / 3 + x)
  >>> f.subs(x, sym.pi).eval()
  -0.4999999999999998
  >>> sym.sin(sym.pi/4 + sym.pi*sym.I/6).eval()
  >>> (0.8062702490019065+0.3873909064828401j)
)doc";

inline constexpr std::string_view scalar_expr_collect = R"doc(
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
  >>> x, y, w = sym.symbols('x, y, w')
  >>> f = x**2*y*sym.cos(w) + 3*y*x**2 + x*y**2 + x*y**2*sym.pi - 5*x**2*y**2 + x*w
  >>> f.collect(x)
  x*(w + pi*y**2 + y**2) + x**2*(3*y + y*cos(w) - 5*y**2)
  >>> f.collect([x, y])
  x*(w + y**2*(1 + pi)) + x**2*(y*(3 + cos(w)) - 5*y**2)
  >>> f.collect([y, x])
  w*x + x**2*y*(3 + cos(w)) + y**2*(x*(1 + pi) - 5*x**2)
)doc";

inline constexpr std::string_view symbols = R"doc(
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
  wrenfold does not yet support the range-syntax implemented in sympy, so evaluating
  ``sym.symbols('x:5')`` will not produce the expected result.
)doc";

inline constexpr std::string_view unique_symbols = R"doc(
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
)doc";

inline constexpr std::string_view compare = R"doc(
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
  >>> x, y = sym.symbols('x, y')
  >>> sym.compare(x, y)
  -1
  >>> sym.compare(y, x)
  1
  >>> sym.compare(5, 8)
  -1
  >>> sym.compare(sym.sin(x), sym.cos(x))
  1
)doc";

inline constexpr std::string_view integer = R"doc(
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
)doc";

inline constexpr std::string_view float_constant = R"doc(
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
)doc";

inline constexpr std::string_view rational = R"doc(
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
)doc";

inline constexpr std::string_view log = R"doc(
The natural logarithm :math:`\ln{x}`.

Args:
  arg: Argument to the logarithm.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.log(x + 3)
  log(x + 3)
  >>> sym.log(sym.E)
  1
  >>> sym.log(1)
  0
)doc";

inline constexpr std::string_view exp = R"doc(
The exponential function :math:`e^{x}` or :math:`\exp{x}`.

Args:
  arg: Argument to the exponential function.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.exp(x + 3)
  exp(x + 3)
  >>> sym.exp(0)
  1
  >>> sym.exp(1)
  sym.E
)doc";

inline constexpr std::string_view pow = R"doc(
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
  >>> x, w = sym.symbols('x, w')
  >>> sym.pow(x * w, 2)
  x**2*w**2
  >>> sym.pow(sym.sqrt(x), 2)
  x
  >>> sym.sqrt(sym.pow(x, 2))
  (x**2)**(1/2)
)doc";

inline constexpr std::string_view cos = R"doc(
The cosine function :math:`\cos{x}`.

Args:
  arg: Argument in radians.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.cos(x)
  cos(x)
  >>> sym.cos(sym.pi)
  -1
)doc";

inline constexpr std::string_view sin = R"doc(
The sine function :math:`\sin{x}`.

Args:
  arg: Argument in radians.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.sin(x)
  sin(x)
  >>> sym.sin(0)
  0
)doc";

inline constexpr std::string_view tan = R"doc(
The tangent function :math:`\tan{x}`.

Args:
  arg: Argument in radians.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.tan(x)
  tan(x)
  >>> sym.tan(sym.pi/2)
  zoo
)doc";

inline constexpr std::string_view acos = R"doc(
The inverse cosine function :math:`\cos^{-1}{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.acos(x)
  acos(x)
  >>> sym.acos(0)
  pi/2
)doc";

inline constexpr std::string_view asin = R"doc(
The inverse sine function :math:`\sin^{-1}{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.asin(x)
  asin(x)
  >>> sym.asin(-1)
  -pi/2
)doc";

inline constexpr std::string_view atan = R"doc(
The inverse tangent function :math:`\tan^{-1}{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.atan(x)
  atan(x)
  >>> sym.atan(-x)
  -atan(x)
)doc";

inline constexpr std::string_view cosh = R"doc(
The hyperbolic cosine function :math:`\cosh{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.cosh(x)
  cosh(x)
  >>> sym.cosh(x * sp.I)
  cos(x)
)doc";

inline constexpr std::string_view sinh = R"doc(
The hyperbolic sine function :math:`\sinh{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.sinh(x)
  sinh(x)
  >>> sym.sinh(x * sp.I)
  I*sin(x)
)doc";

inline constexpr std::string_view tanh = R"doc(
The hyperbolic tangent function :math:`\tanh{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.tanh(x)
  tanh(x)
  >>> sym.tanh(x * sp.I)
  I*tan(x)
)doc";

inline constexpr std::string_view acosh = R"doc(
The inverse hyperbolic cosine function :math:`\cosh^{-1}{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.acosh(x)
  acosh(x)
)doc";

inline constexpr std::string_view asinh = R"doc(
The inverse hyperbolic sine function :math:`\sinh^{-1}{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.asinh(x)
  asinh(x)
)doc";

inline constexpr std::string_view atanh = R"doc(
The inverse hyperbolic tangent function :math:`\tanh^{-1}{x}`.

Examples:
  >>> x = sym.symbols('x')
  >>> sym.atanh(x)
  atanh(x)
)doc";

inline constexpr std::string_view sqrt = R"doc(
The square root function :math:`\sqrt{x}`. This is an alias for ``sym.pow(x, sym.rational(1, 2))``.
)doc";

inline constexpr std::string_view abs = R"doc(
The absolute value function :math:`\lvert x \rvert`.

Examples:
  >>> sym.abs(3)
  3
  >>> x = sym.symbols('x')
  >>> sym.abs(x)
  abs(x)
  >>> sym.abs(x).diff(x)
  x/abs(x)
)doc";

inline constexpr std::string_view sign = R"doc(
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
  >>> x = sym.symbols('x')
  >>> sym.sign(x)
  sign(x)
  >>> sym.sign(x).diff(x)
  0
  >>> sym.sign(-0.78231)
  -1
)doc";

inline constexpr std::string_view floor = R"doc(
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
   >>> x = sym.symbols('x')
   >>> sym.floor(x)
   floor(x)
)doc";

inline constexpr std::string_view atan2 = R"doc(
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
  >>> x, y = sym.symbols('x, y')
  >>> sym.atan2(y, x).diff(x)
  -y/(x**2 + y**2)
)doc";

inline constexpr std::string_view max = R"doc(
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
  >>> x, y = sym.symbols('x, y')
  >>> sym.max(x, y)
  where(x < y, y, x)
  >>> sym.max(2, 3)
  3
)doc";

inline constexpr std::string_view min = R"doc(
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
  >>> x, y = sym.symbols('x, y')
  >>> sym.min(x, y)
  where(y < x, y, x)
  >>> sym.min(sym.rational(2, 3), sym.rational(3, 4))
  2/3
)doc";

inline constexpr std::string_view where = R"doc(
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
  >>> x, y = sym.symbols('x, y')
  >>> sym.where(x > y, sym.cos(x), sym.sin(y))
  where(y < x, cos(x), sin(y))
  >>> sym.where(x > y, sym.pi * x, 0).subs(x, 3).subs(y, 2)
  3*pi
  >>> sym.where(x > y, 4*x**3, 2*y).diff(x)
  where(y < x, 12*x**2, 0)
)doc";

inline constexpr std::string_view less_than = R"doc(
Boolean-valued relational expression :math:`a \lt b`, or ``<`` operator.

Examples:
  >>> sym.lt(2, 3)
  True
  >>> x, y = sym.symbols('x, y')
  >>> sym.lt(x, y)
  x < y
)doc";

inline constexpr std::string_view less_than_or_equal = R"doc(
Boolean-valued relational expression :math:`a \le b`, or ``<=`` operator.

Examples:
  >>> sym.le(1, sym.rational(3, 2))
  True
  >>> x, y = sym.symbols('x, y')
  >>> sym.le(x, y)
  x <= y
)doc";

inline constexpr std::string_view greater_than = R"doc(
Boolean-valued relational expression :math:`a \gt b`, or ``>`` operator. ``a > b`` will be
automatically canonicalized to ``b < a``.

Examples:
  >>> sym.gt(2.12, 2)
  True
  >>> x, y = sym.symbols('x, y')
  >>> sym.gt(x, y)
  y < x
)doc";

inline constexpr std::string_view greater_than_or_equal = R"doc(
Boolean-valued relational expression :math:`a \ge b`, or ``>=`` operator. ``a >= b`` will be
automatically canonicalized to ``b <= a``.

Examples:
  >>> sym.ge(2, 2)
  True
  >>> x, y = sym.symbols('x, y')
  >>> sym.ge(x, y)
  y <= x
)doc";

inline constexpr std::string_view equal = R"doc(
Boolean-valued relational expression :math:`a = b`, or ``==`` operator.

Examples:
  >>> sym.eq(2, 5)
  False
  >>> x, y = sym.symbols('x, y')
  >>> sym.eq(3*x, y/2)
  3*x == y/2
)doc";

inline constexpr std::string_view iverson = R"doc(
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
)doc";

inline constexpr std::string_view unevaluated = R"doc(
Wrap a scalar-valued expression to prevent automatic simplifications/combinations in downstream
operations. This is similar in intent to SymPy's ``UnevaluatedExpr``.

Args:
  arg: Scalar-valued expression to wrap.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> f = sym.unevaluated(x * y) * y
  >>> f # Products are not combined automatically.
  y*(x*y)
  >>> f.diff(y) # Derivatives retain the parentheses.
  y*(x) + (x*y)
)doc";

inline constexpr std::string_view stop_derivative = R"doc(
Wrap a scalar-valued expression in order to block propagation of derivatives. ``stop_derivative``
acts like a function whose derivative is always zero.

Args:
  arg: Scalar-valued expression to wrap.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> f = sym.stop_derivative(x * y) * y
  >>> f.diff(x)
  0
  >>> f.diff(y)
  StopDerivative(x * y)
)doc";

inline constexpr std::string_view eliminate_subexpressions = R"doc(
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
  >>> x, y = sym.symbols('x, y')
  >>> f = sym.cos(x * y) - sym.sin(x * y) + 5 * sym.abs(sym.cos(x * y))
  >>> g, replacements = sym.eliminate_subexpressions(f)
  >>> replacements
  [(v0, x*y), (v1, cos(v0))]
  >>> g
  v1 + 5*abs(v1) - sin(v0)
)doc";

inline constexpr std::string_view symbolic_function_constructor = R"doc(
Declare a new symbolic function with the provided string name. Two functions with the same name are
considered equivalent.

Args:
  name: Function name.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> f = sym.Function('f')
  >>> f(x)
  f(x)
  >>> f(x, y ** 2).diff(x)
  Derivative(f(x, y**2), x)

Raises:
  :class:`wrenfold.exceptions.InvalidArgumentError`: If the input string is empty.
)doc";

inline constexpr std::string_view substitution = R"doc(
Create a deferred substitution expression.

Args:
  input: The expression that would be modified by the substitution.
  target: The target expression being replaced.
  replacement: The substituted expression.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> f = sym.substitution(x**2 + y, y, sym.cos(x))
  >>> print(f)
  Subs(y + x**2, y, cos(x))
  >>> f.args
  (y + x**2, y, cos(x))

Raises:
  :class:`wrenfold.exceptions.TypeError`: If ``target`` is a numeric constant.
)doc";

inline constexpr std::string_view derivative = R"doc(
Create a deferred derivative expression. This expression type is used to represent the derivatives
of abstract symbolic functions.

Args:
  function: Function to be differentiated.
  arg: Argument with respect to which the derivative is taken.
  order: The order of the derivative.

Examples:
  >>> x, y = sym.symbols('x, y')
  >>> sym.derivative(x * x, x)
  Derivative(x**2, x)
  >>> f = sym.Function('f')
  >>> f(x, y).diff(x)
  Derivative(f(x, y), x)

Raises:
  :class:`wrenfold.exceptions.TypeError`: If ``arg`` is not a variable expression.
  :class:`wrenfold.exceptions.InvalidArgumentError`: If ``order <= 0``.
)doc";

inline constexpr std::string_view get_variables = R"doc(
Retrieve all variable expressions from a symbolic expression tree, and return them in a list.

Args:
  expr: A scalar-valued expression.

Returns:
  A list of scalar expressions of underlying type ``Variable`, ``UniqueVariable``, and
  ``FuntionArgumentVariable``.

Examples:
  >>> x, y, z = sym.symbols('x, y, z')
  >>> sym.get_variables(x**2 + y * 3 - sym.cos(z))
  [x, y, z]
  >>> sym.get_variables(x * 3)
  [x]
  >>> vs = sym.get_variables(sym.cos(x * y))
  >>> type(vs[0])
  pywrenfold.sym.Expr
  >>> print(vs[0].type_name)
  Variable
)doc";

}  // namespace wf::docstrings
