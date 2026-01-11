

class ArithmeticError(Exception):
    """Thrown when invalid arithmetic is attempted."""

class AssertionError(Exception):
    """
    Thrown for internal errors. Report these on `GitHub <https://github.com/wrenfold/wrenfold/issues>`_.
    """

class DimensionError(Exception):
    """Thrown when matrix operations encounter invalid dimensions."""

class DomainError(Exception):
    """Thrown when an argument exceeds acceptable numeric bounds."""

class InvalidArgumentError(Exception):
    """Thrown for invalid argument values."""

class TypeError(Exception):
    """Thrown when an unsupported type is encountered."""
