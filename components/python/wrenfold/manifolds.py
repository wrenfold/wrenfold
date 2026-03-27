import abc
import typing

from . import geometry, sym

T = typing.TypeVar("T")


class Manifold(abc.ABC, typing.Generic[T]):
    """
    Generic base class for manifolds.

    Implementations are associated with a type ``T``: the concrete instantiation
    of the manifold present in symbolic function signatures.
    """

    @property
    @abc.abstractmethod
    def ambient_dimension(self) -> int:
        """
        Dimension of the ambient space in which the manifold resides.
        """

    @abc.abstractmethod
    def construct_from_symbols(self, expressions: typing.Sequence[sym.Expr]) -> T:
        """
        Given a sequence of symbols of length ``ambient_dimension``, construct
        an instance of type ``T``.
        """

    @abc.abstractmethod
    def retract_jacobian(self, x: T) -> sym.MatrixExpr:
        """
        Given an instance of type ``T``, produce the (M, N) Jacobian of the
        retraction operation:

        .. math::

          \\mathbf{y} = \\mathbf{x} \\oplus \\delta\\mathbf{x}

        Taken with respect to :math:`\\delta\\mathbf{x}` and evaluated about
        :math:`\\delta\\mathbf{x} = 0`.

        ``M`` is the *ambient* dimension of the manifold, and ``N`` is the dimension
        of the *tangent* space. Rows of the Jacobian should be ordered to match the
        elements of the vector returned by ``to_vector``.
        """


class QuaternionManifold(Manifold[geometry.Quaternion]):
    """
    A quaternion with retraction applied on the right:

    .. math::

        \\mathbf{q}' = \\mathbf{q} \\cdot \\text{exp}\\left(\\delta\\mathbf{x}\\right)
    """

    @property
    def ambient_dimension(self) -> int:
        """Quaternion has 4 dimensions: (w, x, y, z)"""
        return 4

    def construct_from_symbols(self, expressions: typing.Sequence[sym.Expr]) -> geometry.Quaternion:
        """Construct from scalar-first sequence: (w, x, y, z)"""
        return geometry.Quaternion.from_wxyz(expressions)

    def retract_jacobian(self, x: geometry.Quaternion) -> sym.MatrixExpr:
        """
        Return the 4x3 Jacobian of the 4 scalar-first quaternion elements with
        respect to a tangent-space perturbation composed on the right:

        .. math::

          \\frac{\\left[\\mathbf{q} \\cdot \\text{exp}\\left(\\delta\\mathbf{x}\\right)\\right]}
            {\\partial \\delta\\mathbf{x}}

        Evaluated at :math:`\\delta\\mathbf{x} = 0`.
        """
        return x.right_retract_derivative()
