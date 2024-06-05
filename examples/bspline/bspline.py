"""
Example of generating b-splines functions.

For an order `k` b-spline, this script generates the `2*(k - 1)` unique piecewise polynomials
required to evaluate the b-spline. Technically, some of the generated polynomials are mirror-images
of each other - with a bit of work we could eliminate them.

This method of generating the b-splines yields a set of reusable polynomials that we can repeat
along the x-axis to create a spline with an arbitrary number of knots. However, it does induce some
additional complexity that may not suit every use case.

To build the piecewise polynomials, we first construct the order `k` basis functions via the
cox-de-boor formula. Then the polynomial segments for each sub-interval between uniformly spaced
knots are combined into piecewise continuous (to derivative k - 1) polynomials.
"""
import argparse
import typing as T
import collections

from wrenfold.type_annotations import FloatScalar
from wrenfold import code_generation
from wrenfold.code_generation import OutputArg, CppGenerator, RustGenerator
from wrenfold import sym


def weight(x: sym.Expr, i: int, k: int, knots: T.Sequence[sym.Expr]) -> sym.Expr:
    """
    Linear interpolation weight function. The knots are assumed to be known at code-generation time,
    and must be arranged in non-decreasing order.
    """
    if knots[i].is_identical_to(knots[i + k]):
        # Knots will be repeated at both ends to deal with endpoints.
        return sym.zero
    return (x - knots[i]) / (knots[i + k] - knots[i])


def B(x: sym.Expr, i: int, k: int, degree_zero: T.Sequence[sym.Expr],
      knots: T.Sequence[sym.Expr]) -> sym.Expr:
    """
    Construct b-spline basis function `i` of order `k + 1` using the De-Boor recursive
    relation: https://en.wikipedia.org/wiki/B-spline#Definition

    :param x: Variable in which to create the polynomials.
    :param i: Index of the basis function.
    :param k: The degree of the resulting polynomial (1 = linear, 2 = quadratic, etc...)
    :param degree_zero: Symbolic values that represent the degree zero bases B(i, 0).
    :param knots: Symbolic positions of the knots in the normalized interval [0, 1] (including repeated knots).
    """
    assert i >= 0, f'i = {i}'
    assert k >= 0, f'k = {k}'
    if k == 0:
        assert i < len(
            degree_zero), f'Insufficient degree-zero bases. i = {i}, len = {len(degree_zero)}'
        # B(i, 0)
        return degree_zero[i]

    result = weight(x, i, k, knots) * B(x=x, i=i, k=k - 1, degree_zero=degree_zero, knots=knots) + \
        (1 - weight(x, i + 1, k, knots)) * B(x=x, i=i + 1, k=k - 1, degree_zero=degree_zero, knots=knots)

    return result


def create_bases(x: sym.Expr, knots: T.Sequence[sym.Expr], order: int,
                 degree_zero_bases: T.Sequence[sym.Expr]) -> T.List[sym.Expr]:
    """
    Create the degree `order` b-spline basis functions.

    :param x: Variable in which to create the polynomials.
    :param knots: Symbolic positions of the knots in the normalized interval [0, 1] (including repeated knots).
    :param order: Order of the b-spline.
    :param degree_zero_bases: Symbolic values that represent the degree zero bases B(i, 0).
    """
    bases: T.List[sym.Expr] = []
    for i in range(0, len(knots) - order):
        b_func = B(x=x, i=i, k=order - 1, degree_zero=degree_zero_bases, knots=knots)
        b_func = b_func.distribute().collect(degree_zero_bases)
        bases.append(b_func)

    return bases


def create_cumulative_bases(number_of_knots: int, order: int, bases: T.Sequence[sym.Expr],
                            degree_zero_bases: T.Sequence[sym.Expr]) -> T.List[sym.Expr]:
    """
    Given the result of `create_bases`, form the cumulative b-spline basis functions.

    :param number_of_knots: The number of knots (including repeated knots).
    :param order: The order of the b-spline.
    :param order: Order of the b-spline.
    :param degree_zero_bases: Symbolic values that represent the degree zero bases B(i, 0).
    """
    cumulative_bases: T.List[sym.Expr] = []
    for i in range(0, number_of_knots - order):
        b_sum = sym.zero
        for s in range(i, number_of_knots - order):
            b_sum = b_sum + bases[s]
        b_sum = b_sum.distribute().collect(degree_zero_bases)
        cumulative_bases.append(b_sum)

    return cumulative_bases


def create_piecewise_polynomials(x: sym.Expr, order: int, bases: T.Sequence[sym.Expr],
                                 degree_zero_bases: T.Sequence[sym.Expr],
                                 is_cumulative: bool) -> T.List[sym.Expr]:
    """
    Convert order `order` b-spline basis functions into `order` piecewise polynomials. The resulting
    expressions are piecewise functions over the interval `x` = [0, 1].

    :param x: Variable in which the bases are defined.
    :param order: The order of the spline.
    :param bases: The symbolically defined basis functions of order `order`.
    :param degree_zero_bases: Variables that represent the degree zero bases.
    """

    # Symbolic expression that indicates which interval of the bspline the sample
    # point `x` is located, between [0, order - 1].
    x_interval = sym.min(sym.floor(x * order), order - 1)

    polynomials: T.List[sym.Expr] = []
    for i in range(0, order + (order - 1)):
        # Create the segments that make up each piecewise polynomial. Each of these segments is a
        # polynomial that is active between two knots, and zero everywhere else.
        interval_expressions: T.List[sym.Expr] = []
        for j in range(0, order):
            # one hot vector where the j'th zero order basis is active:
            values = [sym.zero] * len(degree_zero_bases)
            values[j + (order - 1)] = sym.one
            key_and_value = list(zip(degree_zero_bases, values))
            interval_expressions.append(bases[i].subs(key_and_value))

        # Cumulative spline is 1 after its relevant interval:
        terminal_value = sym.integer(1 if is_cumulative else 0)
        piecewise_poly = terminal_value

        for interval_index, poly_piece in zip(range(0, order), interval_expressions):
            # If the piece is 0 or 1 on this interval, we don't need another branch for it:
            if poly_piece == terminal_value:
                continue
            piecewise_poly = sym.where(
                sym.eq(x_interval, interval_index), poly_piece, piecewise_poly)

        polynomials.append(piecewise_poly)

    return polynomials


def create_polynomial_functions(x: sym.Expr, polynomials: T.Sequence[sym.Expr], order: int,
                                is_cumulative: bool) -> T.List[code_generation.FunctionDescription]:
    """
    Convert the output of `create_basis_polynomials` into function code-generated functions
    that evaluate the polynomials, and their first `order - 2` derivatives.

    :param x: Variable in which the normalized bases are defined.
    :param order: The order of the spline.
    :param polynomials: The result of `create_basis_polynomials`.
    :param is_cumulative: If true, assume the output is cumulative.
    """
    descriptions: T.List[code_generation.FunctionDescription] = []
    for i in range(0, len(polynomials)):

        def bspline(arg: FloatScalar, arg_scale: FloatScalar):
            # Output the function, plus (order - 2) derivatives that are continuous.
            # The last non-zero derivative is discontinuous.
            rows = [polynomials[i].subs(x, arg)]
            for _ in range(0, order - 2):
                rows.append(rows[-1].diff(arg) * arg_scale)

            return [OutputArg(expression=sym.vector(*rows).transpose(), name=f"b_{i}")]

        if is_cumulative:
            name = f'bspline_cumulative_order{order}_poly_{i}'
        else:
            name = f'bspline_order{order}_poly_{i}'
        desc = code_generation.create_function_description(func=bspline, name=name)
        descriptions.append(desc)

    return descriptions


def plot_polynomials(polynomials: T.List[sym.Expr], x: sym.Expr, order: int, num_knots: int):
    """
    Plot the symbolic polynomial segments.
    """
    # Local imports since these are not actually required to run the script at build time.
    import matplotlib.pyplot as plt
    import numpy as np

    cmap = plt.colormaps.get_cmap('hsv')

    # Determine the width of intervals for the fundamental basis functions:
    base_interval_width = 1.0 / order

    assert num_knots > order, f"order = {order}, num_knots = {num_knots}"

    # Width of the intervals that we'll plot:
    num_intervals = num_knots - 1
    scaled_interval_width = 1.0 / num_intervals
    scale_factor = base_interval_width / scaled_interval_width

    # Total # of basis polynomials
    total_num_bases = num_intervals + order - 1

    computed_polys = collections.defaultdict(list)
    for x_val in np.linspace(0.0, 1.0, 1000):
        # Determine interval x belongs in:
        interval = min(int(np.floor(x_val * num_intervals)), num_intervals - 1)

        # Evaluate the relevant polynomials
        for i in range(0, order):
            shifted_i = i + interval

            # Determine which polynomial we need from the original set of fundamental bases:
            if shifted_i < order - 1:
                poly_index = shifted_i
            elif shifted_i < num_intervals:
                poly_index = order - 1
            else:
                poly_index = order - (num_intervals - shifted_i)

            poly_source_origin = max(0, shifted_i - order + 1) * scaled_interval_width
            poly_dest_origin = max(0, poly_index - order + 1) * base_interval_width

            # Scale and shift into the range of the target polynomial:
            scaled_x = (x_val - poly_source_origin) * scale_factor + poly_dest_origin

            y_val = polynomials[poly_index].subs(x, scaled_x).eval()
            computed_polys[shifted_i].append((x_val, y_val))

    plt.figure()
    plt.title(f"Order {order}")

    for i, poly_vals in computed_polys.items():
        poly_vals = np.array(poly_vals)
        alpha = (i + 0.5) / float(total_num_bases)
        plt.plot(poly_vals[:, 0], poly_vals[:, 1], label=f'Function {i}', color=cmap(alpha))

    for k in np.linspace(0.0, 1.0, num_knots):
        plt.axvline(x=k, linestyle=':', color='black')

    plt.grid()
    plt.legend()
    plt.show()


def create_bspline_functions(
        order: int, visualize: bool = False) -> T.List[code_generation.FunctionDescription]:
    """
    Construct function descriptions for an order `order` b-spline.
    """
    assert order >= 2, f'order = {order}'

    # Number of knots, not considering the repeated endpoints.
    n = order + 1

    # Number of knots, including repetitions.
    number_of_knots = n + 2 * (order - 1)

    # Construct uniformly spaced knots, and pad on the right and left.
    interval_width = 1 / sym.integer(n - 1)
    knots = [interval_width * i for i in range(0, n)]
    knots = [sym.zero] * (order - 1) + knots + [sym.one] * (order - 1)

    # Define the zero order bases as symbolic variables.
    # We will later substitute 0 or 1, depending on whether the interval is active.
    degree_zero_bases = sym.symbols(f'b_{i}_0' for i in range(0, number_of_knots - 1))

    # Variable for the argument to the spline.
    x = sym.symbols("x")

    # The basis functions of our specified order:
    bases = create_bases(x=x, knots=knots, order=order, degree_zero_bases=degree_zero_bases)

    # Cumulative b-spline basis functions:
    cumulative_bases = create_cumulative_bases(
        number_of_knots=len(knots), order=order, bases=bases, degree_zero_bases=degree_zero_bases)

    # Convert the basis functions into piecewise continuous polynomials:
    polynomials = create_piecewise_polynomials(
        x=x, order=order, bases=bases, degree_zero_bases=degree_zero_bases, is_cumulative=False)

    cumulative_polynomials = create_piecewise_polynomials(
        x=x,
        order=order,
        bases=cumulative_bases,
        degree_zero_bases=degree_zero_bases,
        is_cumulative=True)

    # Create function descriptions that we can code-generate.
    descriptions = create_polynomial_functions(
        x=x, polynomials=polynomials, order=order, is_cumulative=False)
    descriptions += create_polynomial_functions(
        x=x, polynomials=cumulative_polynomials, order=order, is_cumulative=True)

    # Set to true to visualize the polynomial pieces we are code-generating.
    if visualize:
        plot_polynomials(polynomials=polynomials, x=x, order=order, num_knots=order + 1)
        plot_polynomials(polynomials=cumulative_polynomials, x=x, order=order, num_knots=order + 1)

    return descriptions


def main(args: argparse.Namespace):
    descriptions = []
    for order in [3, 4, 5, 6]:
        descriptions += create_bspline_functions(order=order, visualize=args.visualize)

    definitions = code_generation.transpile(descriptions)
    if args.language == "cpp":
        code = CppGenerator().generate(definitions)
        code = CppGenerator.apply_preamble(code, namespace="gen")
    elif args.language == "rust":
        code = RustGenerator().generate(definitions)
        code = RustGenerator.apply_preamble(code)
    else:
        raise RuntimeError("Invalid language selection")

    code_generation.mkdir_and_write_file(code=code, path=args.output)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--language", type=str, choices=["cpp", "rust"], required=True, help="Target language.")
    parser.add_argument("--visualize", action="store_true", help="Plot stuff.")
    parser.add_argument("output", type=str, help="Output path")
    return parser.parse_args()


if __name__ == '__main__':
    main(parse_args())
