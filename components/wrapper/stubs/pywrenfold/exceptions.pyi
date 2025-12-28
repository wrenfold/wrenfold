

class ArithmeticError(Exception):
    """Thrown when invalid arithmetic is attempted."""

class AssertionError(Exception):
    """Thrown for internal errors."""

class DimensionError(Exception):
    """Thrown when matrix operations encounter invalid dimensions."""

class DomainError(Exception):
    pass

class InvalidArgumentError(Exception):
    """Thrown for invalid argument values."""

class TypeError(Exception):
    """Thrown when an unsupported type is encountered."""
