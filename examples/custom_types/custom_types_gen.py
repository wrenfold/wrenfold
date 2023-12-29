"""
An example of using custom types in generated function signatures.
"""
import argh
import dataclasses
import typing as T

from wrenfold.type_annotations import RealScalar, Vector3, Vector6
from wrenfold import code_generation
from wrenfold.code_generation import ReturnValue, OutputArg, CppGenerator, RustGenerator
from wrenfold.geometry import Quaternion
from wrenfold import sym


@dataclasses.dataclass
class EigenQuaternion:
    """
    A rotation in three dimensions, represented by a quaternion. In C++ code we'll use the Eigen
    quaternion to store this type. Each member will map to an accessor function.
    """
    x: RealScalar
    y: RealScalar
    z: RealScalar
    w: RealScalar

    def to_quaternion(self) -> Quaternion:
        """Convert to the wrenfold quaternion type so we can more easily manipulate symbolically."""
        return Quaternion(w=self.w, x=self.x, y=self.y, z=self.z)

    @staticmethod
    def from_quaternion(q: Quaternion) -> 'EigenQuaternion':
        """Construct from wrenfold quaternion."""
        return EigenQuaternion(x=q.x, y=q.y, z=q.z, w=q.w)

    def rotation_matrix(self) -> sym.MatrixExpr:
        """Convert to 3x3 rotation matrix."""
        return self.to_quaternion().to_rotation_matrix()

    def from_rotation_vector(v: Vector3) -> 'EigenQuaternion':
        """Convert a Rodrigues vector to quaternion representation."""
        q = Quaternion.from_rotation_vector(v, epsilon=1.0e-16)
        return EigenQuaternion(x=q.x, y=q.y, z=q.z, w=q.w)


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

    In practice, this function is on the simpler side and may not merit code-generation. But it
    serves as a tidy example of using custom types.

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
    """
    a_T_c = a_T_b * b_T_c

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

    def format_get_field(self, element: code_generation.codegen.GetField) -> str:
        """
        Customize access to struct Pose3d. We assume that the pose type has functions to access each
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
            return f'Eigen::Quaternion'
        return self.super_format(element)

    def format_construct_matrix(self, element: code_generation.codegen.ConstructMatrix) -> str:
        # For the small vector that we care about, this works.
        # For larger matrices of vectors, we might need to customize this further and use the streaming
        # initializer syntax.
        formatted_args = ', '.join(self.format(x) for x in element.args)
        return f'Eigen::Matrix<double, {element.type.num_rows}, {element.type.num_cols}>({formatted_args})'

    def format_construct_custom_type(self,
                                     element: code_generation.codegen.ConstructCustomType) -> str:
        if element.type.python_type is EigenQuaternion:
            # Eigen quaternion expects constructor args in order (w, x, y, z)
            arg_dict = {k: v for (k, v) in element.field_values}
            formatted_args = ', '.join(self.format(arg_dict[i]) for i in ("w", "x", "y", "z"))
            # We normalize numerically on construction, rather than symbolically.
            # In this case, we get ~30% fewer operations.
            return f'Eigen::Quaternion<double>{{{formatted_args}}}.normalized()'
        return self.super_format(element)


class CustomRustGenerator(RustGenerator):
    pass


@argh.arg("--language", default=None, choices=["cpp", "rust"], help="Target language.")
def main(output: str, *, language: str):
    descriptions = [
        code_generation.create_function_description(func=transform_point, name='transform_point'),
        code_generation.create_function_description(func=compose_poses, name='compose_poses')
    ]

    definitions = code_generation.transpile(descriptions=descriptions)
    if language == "cpp":
        code = CustomCppGenerator().generate(definitions=definitions)
        code = code_generation.apply_cpp_preamble(code, namespace="gen")
        code_generation.mkdir_and_write_file(code=code, path=output)
    elif language == "rust":
        code = RustGenerator().generate(definitions=definitions)
        code_generation.mkdir_and_write_file(code=code, path=output)


if __name__ == '__main__':
    argh.dispatch_command(main)
