from collections.abc import Sequence
import enum


class NumericType(enum.Enum):
    Bool = 0
    """Boolean value."""

    Integer = 1
    """Signed integral value."""

    Float = 2
    """Floating-point value."""

class ScalarType:
    """A scalar-valued type."""

    def __init__(self, numeric_type: NumericType) -> None:
        """Construct with ``NumericType``."""

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: ScalarType) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: ScalarType) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def numeric_type(self) -> NumericType:
        """Access underlying ``NumericType`` enum."""

    def __repr__(self) -> str: ...

class MatrixType:
    """A 2D matrix-valued type."""

    def __init__(self, rows: int, cols: int) -> None:
        """Construct with number of rows and columns."""

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: MatrixType) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: MatrixType) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def rows(self) -> int:
        """First dimension of the matrix."""

    @property
    def cols(self) -> int:
        """Second dimension of the matrix."""

    @property
    def shape(self) -> tuple[int, int]:
        """Shape as a tuple of ``(rows, cols)``."""

    def compute_indices(self, idx: int) -> tuple[int, int]:
        """Given a flat index, compute the (row, column) pair it corresponds to."""

    def __repr__(self) -> str: ...

class StructField:
    """
    Describes a field on a struct. The :class:`wrenfold.type_info.CustomType` contains a list of fields.
    """

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: StructField) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: StructField) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def name(self) -> str:
        """Name of the field."""

    @property
    def type(self) -> ScalarType | MatrixType | CustomType:
        """Type of the field."""

class CustomType:
    """
    A custom type describes a user-provided struct that exposes members that wrenfold can retrieve in generated code.
    """

    def __init__(self, name: str, fields: Sequence[tuple[str, object]], python_type: type) -> None:
        """Construct custom type."""

    def __hash__(self) -> int:
        """Compute hash."""

    def is_identical_to(self, other: CustomType) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    def __eq__(self, other: CustomType) -> bool:
        """
        Check for strict equality (identical expression trees). This is not the same as mathematical equivalence.
        """

    @property
    def name(self) -> str:
        """Name of the struct."""

    @property
    def fields(self) -> list[StructField]:
        """A list of :class:`wrenfold.type_info.StructField` objects."""

    @property
    def total_size(self) -> int:
        """
        Total number of scalar expressions in the custom type **and** all of its children.
        """

    @property
    def python_type(self) -> type | None:
        """Retrieve the underlying user-declared python type. May be None."""

    def __repr__(self) -> str: ...
