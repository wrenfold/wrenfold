"""
Example of generating b-splines functions.

For an order `k` b-spline, this script generates k unique functions. Each function
covers one interval of the spline. The first function covers the interval between the
first two knots [x0, x1]. The second function covers the interval between second two
knots [x1, x2] - and so on. These functions generalize to any number of knots because
all the other intervals are translations or flips of the first `k`.
"""
import argh
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


def plot_bases(intervals: T.List[T.List[sym.Expr]], x: sym.Expr, interval_width: sym.Expr):
    """
    Plot the symbolic polynomial segments.
    """
    # Local imports since these are not actually required to run the script at build time.
    import matplotlib.pyplot as plt
    import numpy as np
    plt.figure()
    interval_width_float = interval_width.eval()
    for i, interval_polynomials in enumerate(intervals):
        x_vals = np.linspace(0.0, 1.0, 100) * interval_width_float + (interval_width_float * i)
        y_vals = []
        for x_val in x_vals:
            pair = [(x, x_val)]
            y_vals.append([b.subs_variables(pair).eval() for b in interval_polynomials])

        plt.plot(x_vals, np.array(y_vals), label=f'Interval {i}')

    plt.grid()
    plt.legend()
    plt.show()


def create_bspline_functions(order: int) -> T.List[code_generation.codegen.FunctionDescription]:
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

    # Define the zero order bases as symbolic variables for now.
    degree_zero_bases = sym.symbols(f'b_{i}_0' for i in range(0, number_of_knots - 1))

    # Variable for the argument to the spline.
    x = sym.symbols("x")

    # The basis functions of our specified order:
    bases: T.List[sym.Expr] = []
    for i in range(0, n - 1):
        b_func = B(x=x, i=i, k=order - 1, degree_zero=degree_zero_bases, knots=knots)
        b_func = b_func.distribute()
        b_func = b_func.collect(degree_zero_bases)
        bases.append(b_func)

    descriptions: T.List[code_generation.codegen.FunctionDescription] = []
    interval_functions: T.List[T.List[sym.Expr]] = []
    for i in range(0, order):
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

        assert len(
            relevant_bases) == order, f'Should be {order} active zero-order bases in every interval'

        interval_functions.append(relevant_bases)

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

        desc = code_generation.create_function_description(
            func=bspline, name=f'bspline_order{order}_interval_{i}')
        descriptions.append(desc)

    # Set to true to visualize the polynomial pieces we are code-generating.
    visualize = False
    if visualize:
        plot_bases(intervals=interval_functions, x=x, interval_width=interval_width)

    return descriptions


@argh.arg("--language", default=None, choices=["cpp", "rust"], help="Target language.")
def main(output: str, *, language: str):
    descriptions = create_bspline_functions(order=4)
    descriptions += create_bspline_functions(order=7)
    definitions = code_generation.transpile(descriptions=descriptions)
    if language == "cpp":
        code = CppGenerator().generate(definitions=definitions)
        code = code_generation.apply_cpp_preamble(code, namespace="gen")
        code_generation.mkdir_and_write_file(code=code, path=output)
    elif language == "rust":
        code = RustGenerator().generate(definitions=definitions)
        code_generation.mkdir_and_write_file(code=code, path=output)


if __name__ == '__main__':
    argh.dispatch_command(main)
