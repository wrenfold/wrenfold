from typing import ClassVar

class CustomType:
    def __init__(self, name: str, fields: list[tuple[str, object]], python_type: type) -> None: ...
    def is_identical_to(self, other: CustomType) -> bool: ...
    def __eq__(self, other: CustomType) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def fields(self) -> list[StructField]: ...
    @property
    def name(self) -> str: ...
    @property
    def python_type(self) -> None | type: ...
    @property
    def total_size(self) -> int: ...

class MatrixType:
    def __init__(self, rows: int, cols: int) -> None: ...
    def compute_indices(self, idx: int) -> tuple[int, int]: ...
    def is_identical_to(self, other: MatrixType) -> bool: ...
    def __eq__(self, other: MatrixType) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def cols(self) -> int: ...
    @property
    def rows(self) -> int: ...
    @property
    def shape(self) -> tuple[int, int]: ...

class NumericType:
    __members__: ClassVar[dict] = ...  # read-only
    Bool: ClassVar[NumericType] = ...
    Float: ClassVar[NumericType] = ...
    Integer: ClassVar[NumericType] = ...
    __entries: ClassVar[dict] = ...
    def __init__(self, value: int) -> None: ...
    def __eq__(self, other: object) -> bool: ...
    def __hash__(self) -> int: ...
    def __index__(self) -> int: ...
    def __int__(self) -> int: ...
    def __ne__(self, other: object) -> bool: ...
    @property
    def name(self) -> str: ...
    @property
    def value(self) -> int: ...

class ScalarType:
    def __init__(self, numeric_type: NumericType) -> None: ...
    def is_identical_to(self, other: ScalarType) -> bool: ...
    def __eq__(self, other: ScalarType) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def numeric_type(self) -> NumericType: ...

class StructField:
    def __init__(self, *args, **kwargs) -> None: ...
    def is_identical_to(self, other: StructField) -> bool: ...
    def __eq__(self, other: StructField) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def name(self) -> str: ...
    @property
    def type(self): ...
