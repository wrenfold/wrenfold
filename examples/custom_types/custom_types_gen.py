"""
An example of using custom types in generated function signatures.
"""
import argh
import dataclasses
import typing as T

from wrenfold.type_annotations import RealScalar, Vector2
from wrenfold import code_generation
from wrenfold.code_generation import ReturnValue, OutputArg, CppGenerator, RustGenerator
from wrenfold import sym


@dataclasses.dataclass
class Point2D:
    """
    Our first custom type, a simple point in 2D space.
    """
    x: RealScalar
    y: RealScalar

    def to_vector(self) -> sym.MatrixExpr:
        """
        Custom types can feature methods that operate on symbolic types. These are ignored by the
        code-generator, but might be useful in your math code.
        """
        return sym.vector(self.x, self.y)

    @staticmethod
    def from_vector(v: sym.MatrixExpr) -> 'Point2D':
        return Point2D(x=v[0], y=v[1])


@dataclasses.dataclass
class Pose2D:
    """
    Our second custom type, a simple pose in 2D space.

    We can compose custom types - in this case Point2D is used within Pose2D.
    """
    angle: RealScalar
    position: Point2D

    def to_vector(self) -> sym.MatrixExpr:
        """Convert to flat vector."""
        return sym.vector(self.angle, self.position.x, self.position.y)

    def rotation_matrix(self) -> sym.MatrixExpr:
        """Convert our orientation angle into a rotation matrix."""
        c, s = sym.cos(self.angle), sym.sin(self.angle)
        return sym.matrix([[c, -s], [s, c]])


def transform_point(world_T_body: Pose2D, p_body: Point2D):
    """
    A toy function to illustrate using custom types. We accept a poise and point, and return point
    transformed from body to world.

    In practice, this function is on the simpler side and may not merit code-generation. But it
    serves as a tidy example of using custom types.

    :param world_T_body: A 2D pose.
    :param p_body: A point in the right frame (body) of the input pose.
    """
    p_world = world_T_body.rotation_matrix() * p_body.to_vector() + world_T_body.position.to_vector(
    )

    # Output some jacobians as well for good measure.
    p_world_D_pose = p_world.jacobian(
        [world_T_body.angle, world_T_body.position.x, world_T_body.position.y])
    p_world_D_p_body = p_world.jacobian(p_body.to_vector())

    # Custom types can be returned as well:
    return [
        ReturnValue(Point2D.from_vector(p_world)),
        OutputArg(p_world_D_pose, name='p_world_D_pose', is_optional=True),
        OutputArg(p_world_D_p_body, name='p_world_D_p_body', is_optional=True)
    ]


def compose_poses(a_T_b: Pose2D, b_T_c: Pose2D):

    # v_w = p(w_R_i * v_i + w_t_i)
    # p_w = alpha * v_w = w_R_i * v_i + w_t_i
    #

    # Inefficient way of implementing the angle wrapping:
    a_R_c = a_T_b.rotation_matrix() * b_T_c.rotation_matrix()
    a_R_c_angle = sym.atan2(-a_R_c[0, 1], a_R_c[0, 0])

    a_t_c = a_T_b.rotation_matrix() * b_T_c.position.to_vector() + a_T_b.position.to_vector()
    a_T_c = Pose2D(angle=a_R_c_angle, position=Point2D.from_vector(a_t_c))

    output_D_first = a_T_c.to_vector().jacobian(a_T_b.to_vector())
    output_D_second = a_T_c.to_vector().jacobian(b_T_c.to_vector())

    return [
        ReturnValue(a_T_c),
        OutputArg(output_D_first, name='output_D_first', is_optional=True),
        OutputArg(output_D_second, name='output_D_second', is_optional=True)
    ]


def integrate_pose(angular_rate: RealScalar, linear_velocity: Vector2, dt: RealScalar):
    """"""
    theta = angular_rate * dt

    # Integral of R(theta * alpha) over the interval alpha = [0, 1]
    prev_R_cur_int = sym.matrix([[sym.sin(theta), sym.cos(theta) - 1],
                                 [1 - sym.cos(theta), sym.sin(theta)]]) * (1 / theta)
    # First order taylor series expansion:
    prev_R_cur_int_series = sym.matrix([[1 - (theta ** 2) / 6, -theta / 2],
                                        [theta / 2, 1 - (theta ** 2) / 6]])

    prev_R_cur_int = sym.where(sym.abs(theta) > 1.0e-6, prev_R_cur_int, prev_R_cur_int_series)

    prev_T_cur = Pose2D(
        angle=theta, position=Point2D.from_vector(prev_R_cur_int * linear_velocity * dt))
    integrated_D_inputs = prev_T_cur.to_vector().jacobian(
        [angular_rate, linear_velocity[0], linear_velocity[1]])

    return [
        OutputArg(expression=prev_T_cur, name='output_pose', is_optional=True),
        OutputArg(expression=integrated_D_inputs, name='output_D_inputs', is_optional=True)
    ]


class CustomCppGenerator(CppGenerator):

    def format_get_field(self, element: code_generation.codegen.GetField) -> str:
        """
        Customize access to struct Pose2D. We assume that the pose type has functions to access each
        member, rather than having public members.
        """
        if element.struct_type.python_type is Pose2D:
            return f"{self.format(element.arg)}.{element.field_name}()"
        return self.super_format(element)

    def format_custom_type(self, element: code_generation.codegen.CustomType) -> str:
        """Place our custom types into the `geo` namespace."""
        if element.python_type in [Point2D, Pose2D]:
            return f'geo::{element.name}'
        return self.super_format(element)


class CustomRustGenerator(RustGenerator):
    pass


@argh.arg("--language", default=None, choices=["cpp", "rust"], help="Target language.")
def main(output: str, *, language: str):
    descriptions = [
        code_generation.create_function_description(func=transform_point, name='transform_point'),
        code_generation.create_function_description(func=integrate_pose, name='integrate_pose')
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
