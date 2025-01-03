import numpy
from typing import Any, Callable, Iterable, Iterator, overload

E: Expr
I: Expr
false: BooleanExpr
imaginary_unit: Expr
nan: Expr
one: Expr
pi: Expr
true: BooleanExpr
zero: Expr
zoo: Expr

class BooleanExpr:
    def __init__(self, *args, **kwargs) -> None: ...
    def expression_tree_str(self) -> str: ...
    def is_identical_to(self, other: BooleanExpr) -> bool: ...
    @overload
    def subs(self, target, substitute) -> BooleanExpr: ...
    @overload
    def subs(self, target: BooleanExpr, substitute: BooleanExpr) -> BooleanExpr: ...
    @overload
    def subs(self, pairs) -> BooleanExpr: ...
    def __bool__(self) -> bool: ...
    def __eq__(self, other: BooleanExpr) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def args(self) -> tuple: ...
    @property
    def type_name(self) -> str: ...

class CompoundExpr:
    def __init__(self, *args, **kwargs) -> None: ...
    def is_identical_to(self, other: CompoundExpr) -> bool: ...
    def __bool__(self) -> None: ...
    def __eq__(self, other: CompoundExpr) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def type_name(self) -> str: ...

class Expr:
    @overload
    def __init__(self, arg0: int) -> None: ...
    @overload
    def __init__(self, arg0: float) -> None: ...
    @overload
    def collect(self, term: Expr) -> Expr: ...
    @overload
    def collect(self, terms: list[Expr]) -> Expr: ...
    @overload
    def collect(self, x) -> Any: ...
    @overload
    def diff(self, var: Expr, order: int = ..., use_abstract: bool = ...) -> Expr: ...
    @overload
    def diff(self, x) -> Any: ...
    @overload
    def diff(self, x, order=...) -> Any: ...
    @overload
    def diff(self, y) -> Any: ...
    def distribute(self) -> Expr: ...
    @overload
    def eval(self) -> int | float | complex | Expr: ...
    @overload
    def eval(self) -> Any: ...
    @overload
    def eval(self) -> Any: ...
    @overload
    def expression_tree_str(self) -> str: ...
    @overload
    def expression_tree_str(self) -> Any: ...
    def is_identical_to(self, other: Expr) -> bool: ...
    @overload
    def subs(self, pairs: list[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> Expr: ...
    @overload
    def subs(self, target: Expr, substitute: Expr) -> Expr: ...
    @overload
    def subs(self, target: BooleanExpr, substitute: BooleanExpr) -> Expr: ...
    def __abs__(self) -> Expr: ...
    def __add__(self, arg0: Expr) -> Expr: ...
    def __bool__(self) -> None: ...
    def __eq__(self, other: Expr) -> bool: ...
    @overload
    def __ge__(self, arg0: Expr) -> BooleanExpr: ...
    @overload
    def __ge__(self, arg0: int) -> BooleanExpr: ...
    @overload
    def __ge__(self, arg0: float) -> BooleanExpr: ...
    @overload
    def __gt__(self, arg0: Expr) -> BooleanExpr: ...
    @overload
    def __gt__(self, arg0: int) -> BooleanExpr: ...
    @overload
    def __gt__(self, arg0: float) -> BooleanExpr: ...
    def __hash__(self) -> int: ...
    @overload
    def __le__(self, arg0: Expr) -> BooleanExpr: ...
    @overload
    def __le__(self, arg0: int) -> BooleanExpr: ...
    @overload
    def __le__(self, arg0: float) -> BooleanExpr: ...
    @overload
    def __lt__(self, arg0: Expr) -> BooleanExpr: ...
    @overload
    def __lt__(self, arg0: int) -> BooleanExpr: ...
    @overload
    def __lt__(self, arg0: float) -> BooleanExpr: ...
    def __mul__(self, arg0: Expr) -> Expr: ...
    def __neg__(self) -> Expr: ...
    def __pow__(self, arg0: Expr) -> Expr: ...
    @overload
    def __radd__(self, arg0: int) -> Expr: ...
    @overload
    def __radd__(self, arg0: float) -> Expr: ...
    @overload
    def __rmul__(self, arg0: int) -> Expr: ...
    @overload
    def __rmul__(self, arg0: float) -> Expr: ...
    def __rpow__(self, arg0: Expr) -> Expr: ...
    @overload
    def __rsub__(self, arg0: int) -> Expr: ...
    @overload
    def __rsub__(self, arg0: float) -> Expr: ...
    @overload
    def __rtruediv__(self, arg0: int) -> Expr: ...
    @overload
    def __rtruediv__(self, arg0: float) -> Expr: ...
    def __sub__(self, arg0: Expr) -> Expr: ...
    def __truediv__(self, arg0: Expr) -> Expr: ...
    @property
    def args(self) -> tuple: ...
    @property
    def type_name(self) -> str: ...

class Function:
    def __init__(self, name: str) -> None: ...
    def is_identical_to(self, other: Function) -> bool: ...
    def __call__(self, *args) -> Expr: ...
    def __eq__(self, other: Function) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def name(self) -> str: ...

class MatrixExpr:
    def __init__(self, rows: Iterable) -> None: ...
    def col_join(self, other: MatrixExpr) -> MatrixExpr: ...
    @overload
    def collect(self, var: Expr) -> MatrixExpr: ...
    @overload
    def collect(self, var: list[Expr]) -> MatrixExpr: ...
    def det(self) -> Expr: ...
    @overload
    def diff(self, var: Expr, order: int = ..., use_abstract: bool = ...) -> MatrixExpr: ...
    @overload
    def diff(self, x) -> Any: ...
    def distribute(self) -> MatrixExpr: ...
    def eval(self) -> numpy.ndarray: ...
    def expression_tree_str(self) -> str: ...
    def is_identical_to(self, other: MatrixExpr) -> bool: ...
    @overload
    def jacobian(self, vars: list[Expr] | MatrixExpr, use_abstract: bool = ...) -> MatrixExpr: ...
    @overload
    def jacobian(self, vars) -> Any: ...
    def norm(self) -> Expr: ...
    @overload
    def reshape(self, rows: int, cols: int) -> MatrixExpr: ...
    @overload
    def reshape(self, shape: tuple[int, int]) -> MatrixExpr: ...
    def row_join(self, other: MatrixExpr) -> MatrixExpr: ...
    @overload
    def squared_norm(self) -> Expr: ...
    @overload
    def squared_norm(self) -> Any: ...
    @overload
    def subs(self, target: Expr, substitute: Expr) -> MatrixExpr: ...
    @overload
    def subs(self, target: BooleanExpr, substitute: BooleanExpr) -> MatrixExpr: ...
    @overload
    def subs(self, pairs: list[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> MatrixExpr: ...
    def to_flat_list(self) -> list: ...
    def to_list(self) -> list: ...
    @overload
    def transpose(self) -> MatrixExpr: ...
    @overload
    def transpose(self) -> Any: ...
    @overload
    def unary_map(self, func: Callable[[Expr], Expr]) -> MatrixExpr: ...
    @overload
    def unary_map(self, lambdav) -> Any: ...
    def __add__(self, arg0: MatrixExpr) -> MatrixExpr: ...
    def __array__(self, **kwargs) -> numpy.ndarray: ...
    def __bool__(self) -> None: ...
    def __eq__(self, other: MatrixExpr) -> bool: ...
    @overload
    def __getitem__(self, row: int) -> Expr | MatrixExpr: ...
    @overload
    def __getitem__(self, row_col: tuple[int, int]) -> Expr: ...
    @overload
    def __getitem__(self, slice: slice) -> MatrixExpr: ...
    @overload
    def __getitem__(self, slices: tuple[slice, slice]) -> MatrixExpr: ...
    @overload
    def __getitem__(self, row_and_col_slice: tuple[int, slice]) -> MatrixExpr: ...
    @overload
    def __getitem__(self, row_slice_and_col: tuple[slice, int]) -> MatrixExpr: ...
    def __hash__(self) -> int: ...
    def __iter__(self) -> Iterator[Expr | MatrixExpr]: ...
    def __len__(self) -> int: ...
    @overload
    def __mul__(self, arg0: MatrixExpr) -> MatrixExpr: ...
    @overload
    def __mul__(self, arg0: Expr) -> MatrixExpr: ...
    def __neg__(self) -> MatrixExpr: ...
    def __rmul__(self, arg0: Expr) -> MatrixExpr: ...
    def __sub__(self, arg0: MatrixExpr) -> MatrixExpr: ...
    def __truediv__(self, arg0: Expr) -> MatrixExpr: ...
    @property
    def T(self) -> MatrixExpr: ...
    @property
    def is_empty(self) -> bool: ...
    @property
    def shape(self) -> tuple: ...
    @property
    def size(self) -> int: ...
    @property
    def type_name(self) -> str: ...

@overload
def abs(arg: Expr) -> Expr: ...
@overload
def abs(x) -> Any: ...
@overload
def abs(x) -> Any: ...
@overload
def abs(x) -> Any: ...
@overload
def abs(x) -> Any: ...
@overload
def acos(arg: Expr) -> Expr: ...
@overload
def acos(x) -> Any: ...
@overload
def acos(x) -> Any: ...
@overload
def acosh(arg: Expr) -> Expr: ...
@overload
def acosh(x) -> Any: ...
@overload
def acosh(x) -> Any: ...
def addition(args: list[Expr]) -> Expr: ...
@overload
def asin(arg: Expr) -> Expr: ...
@overload
def asin(x) -> Any: ...
@overload
def asin(x) -> Any: ...
@overload
def asinh(arg: Expr) -> Expr: ...
@overload
def asinh(x) -> Any: ...
@overload
def asinh(x) -> Any: ...
@overload
def atan(arg: Expr) -> Expr: ...
@overload
def atan(x) -> Any: ...
@overload
def atan(x) -> Any: ...
@overload
def atan(x) -> Any: ...
@overload
def atan2(y: Expr, x: Expr) -> Expr: ...
@overload
def atan2(y, x) -> Any: ...
@overload
def atanh(arg: Expr) -> Expr: ...
@overload
def atanh(x) -> Any: ...
@overload
def atanh(x) -> Any: ...
@overload
def compare(a: Expr, b: Expr) -> int: ...
@overload
def compare(x, y) -> Any: ...
@overload
def compare(y, x) -> Any: ...
@overload
def cos(arg: Expr) -> Expr: ...
@overload
def cos(x) -> Any: ...
@overload
def cos(x) -> Any: ...
@overload
def cosh(arg: Expr) -> Expr: ...
@overload
def cosh(x) -> Any: ...
@overload
def cosh(x) -> Any: ...
def create_compound_expression_elements(provenance: CompoundExpr, num: int) -> list[Expr]: ...
def create_custom_type_construction(type, expressions: list[Expr]) -> CompoundExpr: ...
def derivative(function: Expr, arg: Expr, order: int = ...) -> Expr: ...
@overload
def det(m: MatrixExpr) -> Expr: ...
@overload
def det() -> Any: ...
@overload
def diag(values: list[MatrixExpr]) -> MatrixExpr: ...
@overload
def diag(values: list[Expr]) -> MatrixExpr: ...
@overload
def distribute(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr: ...
@overload
def distribute() -> Any: ...
@overload
def eliminate_subexpressions(expr: Expr, make_variable: Callable[[int], Expr] | None = ..., min_occurrences: int = ...) -> tuple[Expr, list[tuple[Expr, Expr]]]: ...
@overload
def eliminate_subexpressions(f) -> Any: ...
@overload
def eliminate_subexpressions(expr: MatrixExpr, make_variable: Callable[[int], Expr] | None = ..., min_occurences: int = ...) -> tuple[MatrixExpr, list[tuple[Expr, Expr]]]: ...
def eq(a: Expr, b: Expr) -> BooleanExpr: ...
def eye(rows: int, cols: int | None = ...) -> MatrixExpr: ...
def float(value: float) -> Expr: ...
@overload
def floor(arg: Expr) -> Expr: ...
@overload
def floor(x) -> Any: ...
@overload
def floor(x) -> Any: ...
def full_piv_lu(m: MatrixExpr) -> tuple[MatrixExpr, MatrixExpr, MatrixExpr, MatrixExpr]: ...
@overload
def ge(a: Expr, b: Expr) -> BooleanExpr: ...
@overload
def ge(x, y) -> Any: ...
def get_variables(*args, **kwargs): ...
@overload
def gt(a: Expr, b: Expr) -> BooleanExpr: ...
@overload
def gt(x, y) -> Any: ...
def hstack(values: list[MatrixExpr]) -> MatrixExpr: ...
def integer(value: int) -> Expr: ...
def iverson(arg: BooleanExpr) -> Expr: ...
def jacobian(functions: list[Expr] | MatrixExpr, vars: list[Expr] | MatrixExpr, use_abstract: bool = ...) -> MatrixExpr: ...
@overload
def le(a: Expr, b: Expr) -> BooleanExpr: ...
@overload
def le(x, y) -> Any: ...
def log(arg: Expr) -> Expr: ...
@overload
def lt(a: Expr, b: Expr) -> BooleanExpr: ...
@overload
def lt(x, y) -> Any: ...
def matrix(rows: Iterable) -> MatrixExpr: ...
def matrix_of_symbols(prefix: str, rows: int, cols: int) -> MatrixExpr: ...
@overload
def max(a: Expr, b: Expr) -> Expr: ...
@overload
def max(x, y) -> Any: ...
@overload
def min(a: Expr, b: Expr) -> Expr: ...
@overload
def min(x, y) -> Any: ...
def multiplication(args: list[Expr]) -> Expr: ...
def pow(base: Expr, exp: Expr) -> Expr: ...
def rational(n: int, d: int) -> Expr: ...
def row_vector(*args) -> MatrixExpr: ...
@overload
def sign(arg: Expr) -> Expr: ...
@overload
def sign(x) -> Any: ...
@overload
def sign(x) -> Any: ...
@overload
def sign(x) -> Any: ...
@overload
def sin(arg: Expr) -> Expr: ...
@overload
def sin(x) -> Any: ...
@overload
def sin(x) -> Any: ...
@overload
def sinh(arg: Expr) -> Expr: ...
@overload
def sinh(x) -> Any: ...
@overload
def sinh(x) -> Any: ...
def sqrt(arg: Expr) -> Expr: ...
def stop_derivative(arg: Expr) -> Expr: ...
@overload
def subs(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr, pairs: list[tuple[Expr, Expr] | tuple[BooleanExpr, BooleanExpr]]) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr: ...
@overload
def subs(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr, target: Expr, replacement: Expr) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr: ...
@overload
def subs(expr: Expr | MatrixExpr | CompoundExpr | BooleanExpr, target: BooleanExpr, replacement: BooleanExpr) -> Expr | MatrixExpr | CompoundExpr | BooleanExpr: ...
def substitution(input: Expr, target: Expr, replacement: Expr) -> Expr: ...
def symbols(names: str | Iterable, real: bool = ..., positive: bool = ..., nonnegative: bool = ..., complex: bool = ...) -> Expr | list: ...
@overload
def tan(arg: Expr) -> Expr: ...
@overload
def tan(x) -> Any: ...
@overload
def tan(x) -> Any: ...
@overload
def tanh(arg: Expr) -> Expr: ...
@overload
def tanh(x) -> Any: ...
@overload
def tanh(x) -> Any: ...
def unevaluated(arg: Expr) -> Expr: ...
@overload
def unique_symbols(count: int, real: bool = ..., positive: bool = ..., nonnegative: bool = ..., complex: bool = ...) -> Expr | list: ...
@overload
def unique_symbols(count=...) -> Any: ...
@overload
def unique_symbols(count=...) -> Any: ...
def vec(m: MatrixExpr) -> MatrixExpr: ...
def vector(*args) -> MatrixExpr: ...
def vstack(values: list[MatrixExpr]) -> MatrixExpr: ...
@overload
def where(c: BooleanExpr, a: Expr, b: Expr) -> Expr: ...
@overload
def where(c, a, b) -> Any: ...
@overload
def where(c: BooleanExpr, a: MatrixExpr, b: MatrixExpr) -> MatrixExpr: ...
def zeros(rows: int, cols: int) -> MatrixExpr: ...
