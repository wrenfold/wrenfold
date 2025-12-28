import enum


class StdMathFunction(enum.Enum):
    Cos = 0

    Sin = 1

    Tan = 2

    Acos = 3

    Asin = 4

    Atan = 5

    Cosh = 6

    Sinh = 7

    Tanh = 8

    Acosh = 9

    Asinh = 10

    Atanh = 11

    Log = 12

    Sqrt = 13

    Abs = 14

    Signum = 15

    Floor = 16

    Atan2 = 17

    Powi = 18

    Powf = 19

    def to_string(self) -> str:
        """Convert to string."""

class RelationalOperation(enum.Enum):
    LessThan = 0

    LessThanOrEqual = 1

    Equal = 2

class SymbolicConstant(enum.Enum):
    Euler = 0

    Pi = 1

class NumberSet(enum.Enum):
    RealPositive = 0

    RealNonNegative = 1

    Real = 2

    Complex = 3

    Unknown = 4
