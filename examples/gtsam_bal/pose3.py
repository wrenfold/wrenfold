import dataclasses
import functools

import wrenfold as wf
from wrenfold import geometry, sym


@dataclasses.dataclass
class Rot3:
    """
    A rotation in three dimensions, represented in C++ by an Eigen quaternion.
    Ordered [x, y, z, w].
    """

    x: wf.FloatScalar
    y: wf.FloatScalar
    z: wf.FloatScalar
    w: wf.FloatScalar

    def to_quaternion(self) -> geometry.Quaternion:
        """
        Convert to the wrenfold quaternion type, so we can more easily manipulate symbolically.
        """
        return geometry.Quaternion(w=self.w, x=self.x, y=self.y, z=self.z)

    @staticmethod
    def from_quaternion(q: geometry.Quaternion) -> "Rot3":
        """Construct from wrenfold quaternion."""
        return Rot3(x=q.x, y=q.y, z=q.z, w=q.w)

    def rotation_matrix(self) -> sym.MatrixExpr:
        """Convert to 3x3 rotation matrix."""
        return self.to_quaternion().to_rotation_matrix()

    def to_vector(self) -> sym.MatrixExpr:
        """Create a column vector in storage order."""
        return sym.vector(self.x, self.y, self.z, self.w)

    @staticmethod
    def from_vector(vec: sym.MatrixExpr) -> "Rot3":
        assert vec.shape == (4, 1), f"{vec.shape}"
        return Rot3(*vec)  # xyzw


@dataclasses.dataclass
class Pose3:
    """
    Custom type that stores a rotation + translation, and implements tangent-space derivatives
    for SE(3). This is so we can emit code suitable for use with gtsam::Pose3 or Sophus::SE3.
    """

    rotation: Rot3
    translation: wf.Vector3

    def to_vector(self) -> sym.MatrixExpr:
        """
        Create a flat list in storage order. We assume the columns of the jacobian will also match
        this order (rotation first, then translation).
        """
        return sym.vstack([self.rotation.to_vector(), self.translation])

    @staticmethod
    def from_vector(vec: sym.MatrixExpr) -> "Pose3":
        assert vec.shape == (7, 1), f"{vec.shape}"
        return Pose3(rotation=Rot3.from_vector(vec[0:4]), translation=vec[4:])

    def retract(self, vec: wf.Vector6, epsilon: int | float = 1.0e-16) -> "Pose3":
        """
        Given a 6x1 tangent-space perturbation `vec` (ordered [rotation, translation]), compute the
        exponential map of SE(3) and right-multiply the resulting pose onto `self`:

        result = self * exp(vec)
        """
        assert vec.shape == (6, 1), f"{vec.shape}"
        dR = geometry.Quaternion.from_rotation_vector(vec[0:3], epsilon=epsilon)
        return self.compose(
            Pose3(
                rotation=Rot3.from_quaternion(dR),
                translation=geometry.left_jacobian_of_so3(vec[0:3], epsilon=epsilon) * vec[3:],
            )
        )

    def retract_first_order(self, vec: wf.Vector6) -> "Pose3":
        """
        Retract using first order approximation.
        """
        assert vec.shape == (6, 1), f"{vec.shape}"
        return Pose3.from_vector(self.to_vector() + self.right_retract_derivative() * vec)

    def local_coordinates(self, other: "Pose3", epsilon: int | float = 1.0e-16) -> wf.Vector6:
        """
        Given two members of SE(3), compute the tangent-space delta beween them according to:

        result = log(self^-1 * other)

        Where log(...) performs the inverse of the exponential map.
        """
        delta_pose = self.inverse().compose(other)
        # Compute the rotation and translation parts of the tangent vector:
        w_vec = delta_pose.rotation.to_quaternion().to_rotation_vector(
            epsilon=epsilon, use_atan2=False
        )
        u_vec = (
            geometry.inverse_left_jacobian_of_so3(w=w_vec, epsilon=epsilon) * delta_pose.translation
        )
        # Turn into one 6x1 vector ordered (rotation, translation).
        return sym.vstack([w_vec, u_vec])

    def local_coordinates_first_order(self, other: "Pose3") -> wf.Vector6:
        """
        Local coordinates using first order approximation.
        """
        return self.right_local_coordinates_derivative() * (other.to_vector() - self.to_vector())

    def compose(self, other: "Pose3") -> "Pose3":
        """
        The product of two poses. Corresponds to the matrix multiplication:

        [[a_R_b  a_t_b]  * [[b_R_c  b_t_c]   =  [[a_R_b*b_R_c  a_R_b*b_t_c + a_t_b]
         [0          1]]    [0          1]]      [0                              1]]
        """
        return Pose3(
            rotation=Rot3.from_quaternion(
                self.rotation.to_quaternion() * other.rotation.to_quaternion()
            ),
            translation=self.translation + self.rotation.rotation_matrix() * other.translation,
        )

    def right_retract_derivative(self) -> sym.MatrixExpr:
        """
        The 7x6 derivative of the 7 (4 quaternion + 3 translation) pose elements with respect
        to the right-tangent space of SE(3).
        """
        variables, J = se3_right_retract_derivative()
        substitutions = list(zip(variables, self.to_vector(), strict=True))
        return J.subs(substitutions)

    def right_local_coordinates_derivative(self) -> sym.MatrixExpr:
        """
        The 6x7 derivative of the 6 tangent-space elements with respect to the 7 pose elements.
        """
        variables, J = se3_right_local_coordinates_derivative()
        substitutions = list(zip(variables, self.to_vector(), strict=True))
        return J.subs(substitutions)

    def inverse(self) -> "Pose3":
        """
        If `A` is the 4x4 homogeneous transform corresponding to `self`, we compute the pose that
        corresponds to the matrix `A^-1` such that A * A^-1 --> Identity.
        """
        q_inv = self.rotation.to_quaternion().conjugate()
        return Pose3(
            rotation=Rot3.from_quaternion(q_inv),
            translation=q_inv.to_rotation_matrix() * -self.translation,
        )


@functools.cache
def se3_right_retract_derivative() -> tuple[list[sym.Expr], sym.MatrixExpr]:
    """
    Compute the 7x6 derivative of: d(X * exp([w; u])) / d([w; u])

    Evaluated at w = 0, u = 0.

    This quantity can be derived once and re-used. We express the result as a function of 7
    variables (4 quaternion, 3 translation). The variables are returned along with the jacobian
    expression.
    """
    # First create a symbolic pose `X`:
    X = Pose3.from_vector(sym.matrix(sym.unique_symbols(count=7, real=True)))

    # Create a tangent-space perturbation, and perturb pose `X` on the right side:
    perturbation = sym.matrix(sym.unique_symbols(count=6, real=True))
    X_perturbed = X.retract(perturbation, epsilon=0)

    # Compute the jacobian wrt the tangent space perturbation:
    J = sym.jacobian(X_perturbed.to_vector(), perturbation)
    assert J.shape == (7, 6), f"{J.shape}"

    # Evaluate about perturbation = 0
    J = J.subs([(var, 0) for var in perturbation])

    return (X.to_vector().to_flat_list(), J)


@functools.cache
def se3_right_local_coordinates_derivative() -> tuple[list[sym.Expr], sym.MatrixExpr]:
    """
    Compute the 6x7 derivative of:

    d(log(X^-1 * (X + dX))) / dX

    Evaluated at dX = 0. dX is the 7-element perturbation to the quaternion and translation vector.
    """
    X = Pose3.from_vector(sym.matrix(sym.unique_symbols(count=7, real=True)))

    perturbation = sym.vector(*sym.unique_symbols(count=7, real=True))
    X_plus_dX = Pose3.from_vector(X.to_vector() + perturbation)
    tangent_delta = X.local_coordinates(X_plus_dX, epsilon=0)

    J = sym.jacobian(tangent_delta, perturbation)
    assert J.shape == (6, 7), f"{J.shape}"

    # Evaluate about perturbation = 0
    J = J.subs([(var, 0) for var in perturbation])

    return (X.to_vector().to_flat_list(), J)
