"""
An example of using custom types in generated function signatures.

We define a custom `Pose3` type as the product of SO(3) (stored as a quaternion) and a translation
vector in R(3). The code generators are customized so that this type can be passed directly to
and from generated methods. The C++ and Rust methods use Eigen and nalgebra respectively to store
their members.

This example is meant to illustrate how a modest amount of customization can allow wrenfold to
interface with types in an existing codebase.
"""
import argparse
import dataclasses
import typing as T

from wrenfold import ast
from wrenfold import code_generation
from wrenfold import sym
from wrenfold.code_generation import ReturnValue, OutputArg, CppGenerator, RustGenerator
from wrenfold.geometry import Quaternion
from wrenfold.type_annotations import RealScalar, Vector3, Vector6


@dataclasses.dataclass
class EigenQuaternion:
    """
    A rotation in three dimensions, represented by a quaternion. In C++ code we'll use the Eigen
    quaternion to store this type. Each member will map to an accessor function.

    In Rust code we will use nalgebra.
    """
    x: RealScalar
    y: RealScalar
    z: RealScalar
    w: RealScalar

    def to_quaternion(self) -> Quaternion:
        """Convert to the wrenfold quaternion type, so we can more easily manipulate symbolically."""
        return Quaternion(w=self.w, x=self.x, y=self.y, z=self.z)

    @staticmethod
    def from_quaternion(q: Quaternion) -> 'EigenQuaternion':
        """Construct from wrenfold quaternion."""
        return EigenQuaternion(x=q.x, y=q.y, z=q.z, w=q.w)

    def rotation_matrix(self) -> sym.MatrixExpr:
        """Convert to 3x3 rotation matrix."""
        return self.to_quaternion().to_rotation_matrix()


@dataclasses.dataclass
class Pose3d:
    """
    Our second custom type, a simple pose in 3D space.

    We can compose custom types: In this case `EigenQuaternion` and `Vector3`.
    """
    rotation: EigenQuaternion
    translation: Vector3

    def to_flat_list(self) -> T.List[sym.Expr]:
        """Flatten to a single list of expressions."""
        return self.rotation.to_quaternion().to_list() + self.translation.to_flat_list()

    def right_retract_derivative(self) -> sym.MatrixExpr:
        """
        The 7x6 derivative of the 7 (4 quaternion + 3 translation) pose elements with respect
        to the right-tangent space.

        We treat Pose3 as the product SO(3) x R(3) rather than SE(3).
        """
        return sym.diagonal([self.rotation.to_quaternion().right_retract_derivative(), sym.eye(3)])

    def right_local_coordinates_derivative(self) -> sym.MatrixExpr:
        """
        The 6x7 derivative of the right-tangent with respect to the 7 pose elements.
        """
        return sym.diagonal(
            [self.rotation.to_quaternion().right_local_coordinates_derivative(),
             sym.eye(3)])

    def inverse(self) -> 'Pose3d':
        """
        If `A` is the 4x4 homogeneous transform corresponding to `self`, we compute the pose that
        corresponds to the matrix `A^-1` such that A * A^-1 --> Identity.
        """
        q_inverse: Quaternion = self.rotation.to_quaternion().inverse()
        return Pose3d(
            rotation=EigenQuaternion.from_quaternion(q_inverse),
            translation=q_inverse.to_rotation_matrix() * -self.translation)

    def __mul__(self, other: 'Pose3d') -> 'Pose3d':
        """
        Compute the pose obtained by right multiplying `other` onto `self`.

        If the input poses `self` and `other` are represented as 4x4 homogeneous transformation
        matrices `A` and `B`, then the result of this method will correspond to the matrix C = A * B.
        """
        rot = self.rotation.to_quaternion() * other.rotation.to_quaternion()
        return Pose3d(
            rotation=EigenQuaternion.from_quaternion(rot),
            translation=self.translation + self.rotation.rotation_matrix() * other.translation)


@dataclasses.dataclass
class Point3d:
    """
    Another hypothetical custom type, a point in 3d space.
    """
    x: RealScalar
    y: RealScalar
    z: RealScalar

    def to_vector(self) -> sym.MatrixExpr:
        """
        Custom types can feature methods that operate on symbolic types. These are ignored by the
        code-generator, but might be useful in your math code.
        """
        return sym.vector(self.x, self.y, self.z)

    @staticmethod
    def from_vector(v: sym.MatrixExpr) -> 'Point3d':
        assert v.shape == (3, 1), f'Wrong shape: {v.shape}'
        return Point3d(x=v[0], y=v[1], z=v[2])


def transform_point(world_T_body: Pose3d, p_body: Point3d):
    """
    A toy function to illustrate using custom types. We accept a poise and point, and return point
    transformed from body to world.

    :param world_T_body: A 2D pose.
    :param p_body: A point in the right frame (body) of the input pose.
    """
    p_world = world_T_body.rotation.rotation_matrix() * p_body.to_vector() + world_T_body.translation

    # Output some jacobians as well for good measure.
    p_world_D_pose = p_world.jacobian(
        world_T_body.to_flat_list()) * world_T_body.right_retract_derivative()
    p_world_D_p_body = p_world.jacobian(p_body.to_vector())

    # Custom types can be returned as well:
    return [
        ReturnValue(Point3d.from_vector(p_world)),
        OutputArg(p_world_D_pose, name='p_world_D_pose', is_optional=True),
        OutputArg(p_world_D_p_body, name='p_world_D_p_body', is_optional=True)
    ]


def compose_poses(a_T_b: Pose3d, b_T_c: Pose3d):
    """
    Chain together two poses `a_T_c = a_T_b * b_T_c` and compute the tangent-space derivative of
    the result with respect to both inputs.

    This function serves as an example of returning an aggregate custom type, `Pose3d`.

    :param a_T_b: The first pose to multiply on the left.
    :param b_T_c: The second pose to multiply on the right.

    :return: A list of outputs we want to code-generate.
    """
    a_T_c = a_T_b * b_T_c

    # Wrenfold does not impose any particular convention for manifolds. As a user, we need to
    # appropriately chain our derivatives with the mapping from our variables to the tangent
    # space we will use for retraction (the update step) in our optimization.
    composed_D_first = a_T_c.right_local_coordinates_derivative() * \
        sym.jacobian(a_T_c.to_flat_list(), a_T_b.to_flat_list()) * \
        a_T_b.right_retract_derivative()

    composed_D_second = a_T_c.right_local_coordinates_derivative() * \
        sym.jacobian(a_T_c.to_flat_list(), b_T_c.to_flat_list()) * \
        b_T_c.right_retract_derivative()

    return [
        ReturnValue(a_T_c),
        OutputArg(composed_D_first, name="composed_D_first", is_optional=True),
        OutputArg(composed_D_second, name="composed_D_second", is_optional=True)
    ]


class CustomCppGenerator(CppGenerator):

    def format_get_field(self, element: ast.GetField) -> str:
        """
        Customize access to struct Pose3. We assume that the pose type has functions to access each
        member, rather than having public members.
        """
        if element.struct_type.python_type in [Pose3d, EigenQuaternion]:
            return f"{self.format(element.arg)}.{element.field_name}()"
        return self.super_format(element)

    def format_custom_type(self, element: code_generation.codegen.CustomType) -> str:
        """Place our custom types into the `geo` namespace."""
        if element.python_type in [Point3d, Pose3d]:
            return f'geo::{element.name}'
        elif element.python_type is EigenQuaternion:
            return f'Eigen::Quaternion<double>'
        return self.super_format(element)

    def format_construct_matrix(self, element: ast.ConstructMatrix) -> str:
        """
        By default, wrenfold passes input and output matrices as spans. If we want to return a matrix,
        we need to override this method and help the code-generator emit constructor syntax suitable
        to our chosen types.
        """
        # For the small vector that we care about, this syntax works.
        formatted_args = ', '.join(self.format(x) for x in element.args)
        return f'Eigen::Matrix<double, {element.type.num_rows}, {element.type.num_cols}>({formatted_args})'

    def format_construct_custom_type(self, element: ast.ConstructCustomType) -> str:
        if element.type.python_type is EigenQuaternion:
            # Eigen quaternion expects constructor args in order (w, x, y, z)
            arg_dict = {f.name: element.get_field_value(f.name) for f in element.type.fields}
            formatted_args = ', '.join(self.format(arg_dict[i]) for i in ("w", "x", "y", "z"))
            # We normalize numerically on construction, rather than symbolically. This avoids introducing
            # superfluous operations into the symbolic math tree, which saves on generated operations.
            return f'Eigen::Quaternion<double>{{{formatted_args}}}.normalized()'
        return self.super_format(element)


class CustomRustGenerator(RustGenerator):

    def format_get_field(self, element: ast.GetField) -> str:
        """
        Use member accessors for the Pose3 type.
        """
        if element.struct_type.python_type is Pose3d:
            return f"{self.format(element.arg)}.{element.field_name}()"
        elif element.struct_type.python_type is EigenQuaternion:
            # Customize accessors for UnitQuaternion in rust:
            if element.field_name in ("x", "y", "z"):
                return f'{self.format(element.arg)}.imag().{element.field_name}'
            else:
                assert element.field_name == 'w'
                return f'{self.format(element.arg)}.scalar()'
        return self.super_format(element)

    def format_custom_type(self, element: code_generation.codegen.CustomType) -> str:
        """Place our custom types into the `geo` namespace."""
        if element.python_type in [Point3d, Pose3d]:
            return f'crate::geo::{element.name}'
        elif element.python_type is EigenQuaternion:
            return f'nalgebra::UnitQuaternion::<f64>'
        return self.super_format(element)

    def format_construct_matrix(self, element: ast.ConstructMatrix) -> str:
        """Generate constructor syntax for nalgebra matrices."""
        formatted_args = ', '.join(self.format(x) for x in element.args)
        return f'nalgebra::SMatrix::<f64, {element.type.num_rows}, {element.type.num_cols}>::new({formatted_args})'

    def format_construct_custom_type(self, element: ast.ConstructCustomType) -> str:
        """Use a `new()` method for our Pose3 type, and the UnitQuaternion constructor for rotations."""
        if element.type.python_type is Pose3d:
            r = self.format(element.get_field_value("rotation"))
            t = self.format(element.get_field_value("translation"))
            return f'crate::geo::Pose3d::new(\n  {r},\n  {t}\n)'
        elif element.type.python_type is EigenQuaternion:
            # Order for nalgebra is [w, x, y, z]
            arg_dict = {f.name: element.get_field_value(f.name) for f in element.type.fields}
            formatted_args = ', '.join(self.format(arg_dict[i]) for i in ("w", "x", "y", "z"))
            return f'nalgebra::Unit::new_normalize(nalgebra::Quaternion::<f64>::new({formatted_args}))'
        return self.super_format(element)


def main(args: argparse.Namespace):
    descriptions = [
        code_generation.create_function_description(func=transform_point, name='transform_point'),
        code_generation.create_function_description(func=compose_poses, name='compose_poses')
    ]

    definitions = code_generation.transpile(descriptions=descriptions)
    if args.language == "cpp":
        code = CustomCppGenerator().generate(definitions=definitions)
        code = code_generation.apply_cpp_preamble(code, namespace="gen")
    elif args.language == "rust":
        code = CustomRustGenerator().generate(definitions=definitions)
        code = code_generation.apply_rust_preamble(code)
    else:
        raise RuntimeError("Invalid language selection")

    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--language", type=str, choices=["cpp", "rust"], help="Target language.")
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
