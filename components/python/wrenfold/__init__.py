"""Symbolic code-generation tools."""

# ruff: noqa: F401

from pywrenfold import __version__

__author__ = "Gareth <gcross.code@icloud.com>"
__copyright__ = "Copyright (C) 2024 Gareth Cross"
__license__ = "MIT"


from .code_generation import (
    BaseGenerator,
    CodegenFuncInvocationResult,
    CppGenerator,
    CppMatrixTypeBehavior,
    FunctionDescription,
    OptimizationParams,
    OutputArg,
    PythonGenerator,
    PythonGeneratorFloatWidth,
    PythonGeneratorTarget,
    ReturnValue,
    ReturnValueOrOutputArg,
    RustGenerator,
    create_function_description,
    cse_function_description,
    generate_function,
    generate_python,
    mkdir_and_write_file,
    transpile,
)
from .enumerations import NumberSet, RelationalOperation, StdMathFunction, SymbolicConstant
from .type_annotations import (
    FloatScalar,
    IntScalar,
    Matrix1,
    Matrix2,
    Matrix3,
    Matrix4,
    Matrix5,
    Matrix6,
    Matrix7,
    Matrix8,
    Matrix9,
    Opaque,
    Shape,
    Vector1,
    Vector2,
    Vector3,
    Vector4,
    Vector5,
    Vector6,
    Vector7,
    Vector8,
    Vector9,
)
