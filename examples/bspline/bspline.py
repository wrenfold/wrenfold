"""
Example of generating b-splines functions.

For an order `k` b-spline, this script generates k unique functions. Each function
covers one interval of the spline. The first function covers the interval between the
first two knots [x0, x1]. The second function covers the interval between second two
knots [x1, x2] - and so on. These functions generalize to any number of knots because
all the other intervals are translations or flips of the first `k`.
"""
import argparse
import typing as T

from wrenfold.type_annotations import RealScalar
from wrenfold import code_generation
from wrenfold.code_generation import OutputArg, CppGenerator, RustGenerator
from wrenfold import sym


def weight(x: sym.Expr, i: int, k: int, knots: T.Sequence[sym.Expr]) -> sym.Expr:
    """
    Linear interpolation weight function. The knots are assumed to be known at code-generation time,
    and must be arranged in non-decreasing order.
    """
    if knots[i] == knots[i + k]:
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


def create_bspline_function_descriptions(
    x: sym.Expr, number_of_non_repeated_knots: int, order: int, bases: T.Sequence[sym.Expr],
    degree_zero_bases: T.Sequence[sym.Expr], is_cumulative: bool
) -> T.Tuple[T.List[code_generation.codegen.FunctionDescription], T.List[sym.MatrixExpr]]:
    """
    Convert order `order` b-spline basis functions into a series of functions that can be
    code-generated. Each output function covers an interval between two knots in the final
    spline. We only generate the first `order - 1` intervals, because the rest are all scaled or
    shifted repetitions of the first few.

    We iterate over the `number_of_non_repeated_knots - 1` intervals, and in each one set the
    relevant degree-zero basis funtion to 1, and all others to zero. This determines the set of
    polynomial segments required to evaluate the b-spline in that interval. We concatenate these
    (and their derivatives) into a single generated function for that interval.

    :param x: Variable in which the bases are defined.
    :param number_of_non_repeated_knots: The number of non-repeated knots over [0, 1].
    :param order: The order of the spline.
    :param bases: The symbolically defined basis functions of order `order`.
    :param degree_zero_bases: Variables that represent the degree zero bases.
    :param is_cumulative: If true, assume the output is cumulative.
    """

    descriptions: T.List[code_generation.codegen.FunctionDescription] = []
    interval_functions: T.List[sym.MatrixExpr] = []

    # The width of each interval:
    interval_width = 1 / sym.integer(number_of_non_repeated_knots - 1)

    # Overate over the `n - 1` intervals that cover [0, 1]:
    for i in range(0, number_of_non_repeated_knots - 1):
        # Create a one-hot vector where the i'th zero-order basis is one, and all other zero.
        # Then we substitute these into the order `order` bases, in order to see which polynomial
        # segments are active between the i'th knot, and the i'th + 1 knot.
        values = [sym.zero] * len(degree_zero_bases)
        values[i + (order - 1)] = sym.one  # Add order - 1 to skip the repeated knots.
        key_and_value = list(zip(degree_zero_bases, values))

        relevant_bases: T.List[sym.Expr] = []
        for b in bases:
            b_sub = b.subs_variables(key_and_value)
            if not b_sub.is_identical_to(sym.zero):
                relevant_bases.append(b_sub)

        # For cumulative, we take only the last `order - 1` bases (since the rest are analytically one).
        if is_cumulative:
            relevant_bases = relevant_bases[-(order - 1):]
        else:
            assert len(relevant_bases
                      ) == order, f'Should be {order} active zero-order bases in every interval'

        # Stack all the functions into a vector for plotting later:
        interval_functions.append(sym.vector(*relevant_bases))

        if i >= order:
            # We only generate the first `order` functions. The rest are all mirrors or
            # translations of these first few.
            continue

        # Now we create a function that evaluates the relevant bases.
        def bspline(arg: RealScalar, arg_scale: RealScalar):
            # Scale and translate the argument to be with respect to the polynomials we defined.
            interval_start = i * interval_width
            scaled_sub_pairs = [(x, arg * interval_width + interval_start)]
            b_subbed = sym.vector(*[b.subs_variables(scaled_sub_pairs) for b in relevant_bases])

            # Output the function, plus (order - 2) derivatives that are continuous.
            # The last non-zero derivative is discontinuous.
            columns = [b_subbed]
            for _ in range(0, order - 2):
                columns.append(columns[-1].diff(arg) * arg_scale)

            return [OutputArg(expression=sym.hstack(columns), name="b")]

        if is_cumulative:
            name = f'bspline_cumulative_order{order}_interval_{i}'
        else:
            name = f'bspline_order{order}_interval_{i}'
        desc = code_generation.create_function_description(func=bspline, name=name)
        descriptions.append(desc)

    return descriptions, interval_functions


def plot_bases(intervals: T.List[sym.MatrixExpr], x: sym.Expr, interval_width: sym.Expr,
               knots: T.Sequence[float]):
    """
    Plot the symbolic polynomial segments.
    """
    # Local imports since these are not actually required to run the script at build time.
    import matplotlib.pyplot as plt
    import numpy as np

    cmap = plt.colormaps.get_cmap('hsv')

    plt.figure()
    interval_width_float = interval_width.eval()
    for i, interval_polynomials in enumerate(intervals):
        x_vals = np.linspace(0.0, 1.0, 100) * interval_width_float + (interval_width_float * i)
        y_vals = []
        for x_val in x_vals:
            pair = [(x, x_val)]
            y_vals.append([b.subs_variables(pair).eval() for b in interval_polynomials])

        alpha = (i + 0.5) / float(len(intervals))
        plt.plot(x_vals, np.array(y_vals), label=f'Interval {i}', color=cmap(alpha))

    for k in knots:
        plt.axvline(x=k, linestyle=':', color='black')

    plt.grid()
    plt.legend()
    plt.show()


def create_bspline_functions(order: int,
                             visualize: bool = False
                            ) -> T.List[code_generation.codegen.FunctionDescription]:
    """
    Construct function descriptions for an order `order` b-spline.
    """
    assert order >= 2, f'order = {order}'

    # Number of knots, not considering the repeated endpoints.
    # We pad by `order - 1` so that the last interval we generate is bordered on either side by cardinal bases.
    n = (order + 1) + (order - 1)

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

    # Create function descriptions for the ordinary b-splines.
    descriptions, interval_functions = create_bspline_function_descriptions(
        x=x,
        order=order,
        number_of_non_repeated_knots=n,
        bases=bases,
        degree_zero_bases=degree_zero_bases,
        is_cumulative=False)

    # Now create functions for cumulative b-splines...
    cumulative_descriptions, interval_cumulative_functions = create_bspline_function_descriptions(
        x=x,
        order=order,
        number_of_non_repeated_knots=n,
        bases=cumulative_bases,
        degree_zero_bases=degree_zero_bases,
        is_cumulative=True)

    descriptions.extend(cumulative_descriptions)

    # Set to true to visualize the polynomial pieces we are code-generating.
    if visualize:
        plot_bases(
            intervals=interval_functions,
            x=x,
            interval_width=interval_width,
            knots=[k.eval() for k in knots])
        plot_bases(
            intervals=interval_cumulative_functions,
            x=x,
            interval_width=interval_width,
            knots=[k.eval() for k in knots])

    return descriptions


def main(args: argparse.Namespace):
    descriptions = create_bspline_functions(order=4, visualize=args.visualize)
    descriptions += create_bspline_functions(order=7, visualize=args.visualize)
    definitions = code_generation.transpile(descriptions=descriptions)
    if args.language == "cpp":
        code = CppGenerator().generate(definitions=definitions)
        code = code_generation.apply_cpp_preamble(code, namespace="gen")
    elif args.language == "rust":
        code = RustGenerator().generate(definitions=definitions)
        code = code_generation.apply_rust_preamble(code)
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
