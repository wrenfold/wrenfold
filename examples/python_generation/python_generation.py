import argparse

from wrenfold import ast
from wrenfold.code_generation import BaseGenerator


class PythonCodeGenerator(BaseGenerator):
    """
    A custom code-generator that emits python code.
    """

    def format_add(self, add: ast.Add) -> str:
        return f'{self.format(add.left)} + {self.format(add.right)}'

    def format_assign_output_matrix(self, mat: ast.AssignOutputMatrix) -> str:
        lines = []
        for index, arg in enumerate(mat.value.args):
            i, j = mat.value.type.compute_indices(index)
            lines.append(f'{mat.arg.name}[{i}, {j}] = {self.format(arg)}')

        return '\n'.join(lines)

    def format_assign_output_scalar(self, scalar: ast.AssignOutputScalar) -> str:
        return f'{scalar.arg.name}.reshape(1, )[0] = {self.format(scalar.value)}'

    def format_assign_output_struct(self, struct: ast.AssignOutputStruct) -> str:
        return f''


def main():
    pass


if __name__ == '__main__':
    main()
