import enum


class StdMathFunction(enum.Enum):
    """Standard-library math functions utilized in generated code."""

    Cos = 0
    """Cosine"""

    Sin = 1
    """Sine"""

    Tan = 2
    """Tangent"""

    Acos = 3
    """Arccosine"""

    Asin = 4
    """Arcsine"""

    Atan = 5
    """Arctangent"""

    Cosh = 6
    """Hyperbolic cosine"""

    Sinh = 7
    """Hyperbolic sine"""

    Tanh = 8
    """Hyperbolic tangent"""

    Acosh = 9
    """Inverse hyperbolic cosine"""

    Asinh = 10
    """Inverse hyperbolic sine"""

    Atanh = 11
    """Inverse hyperbolic tangent"""

    Log = 12
    """Natural logarithm"""

    Exp = 13
    """Exponential (e**x)"""

    Sqrt = 14
    """Square-root"""

    Abs = 15
    """Absolute value"""

    Signum = 16
    """Sign function"""

    Floor = 17
    """Floor function"""

    Atan2 = 18
    """Atan2"""

    Powi = 19
    """Raised to integer power"""

    Powf = 20
    """Rased to floating-point power"""

    def to_string(self) -> str:
        """Convert to string."""

class RelationalOperation(enum.Enum):
    """
    Comparison operations are canonicalized in terms of these three comparison operators.
    """

    LessThan = 0
    """< operator"""

    LessThanOrEqual = 1
    """<= operator"""

    Equal = 2
    """== operator"""

class SymbolicConstant(enum.Enum):
    """Significant mathematical constants."""

    Euler = 0
    """Euler's constant: e"""

    Pi = 1
    """Pi: π"""

class NumberSet(enum.Enum):
    """The classification or mathematical type of a symbol."""

    RealPositive = 0
    """Real and positive (> 0)."""

    RealNonNegative = 1
    """Real and non-negative (>= 0)."""

    Real = 2
    """The set of real numbers."""

    Complex = 3
    """The set of complex numbers."""

    Unknown = 4
    """No assumption is made about variables with unknown numeric set."""
