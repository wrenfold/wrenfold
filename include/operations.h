// Copyright 2023 Gareth Cross

namespace math {
class Expr;

// Replace instances of `target` w/ `replacement` in the input expression tree `input`.
// Implemented in substitute.cc
Expr Substitute(const Expr& input, const Expr& target, const Expr& replacement);

// Take the derivative of `differentiand` wrt `arg`. The derivative is taken `reps` times.
// Implemented in derivative.cc
Expr Diff(const Expr& differentiand, const Expr& arg, const int reps);

// Distribute over multiplications in the input expression.
// Expands terms like: (a + b) * (c * d) --> a*c + a*d + b*c + b*d
// Implemented in distribute.cc
Expr Distribute(const Expr& arg);

// Evaluate to a number. If the result is a matrix, returns a matrix expression of numbers.
// Implemented in evaluate.cc
Expr Eval(const Expr& arg);

}  // namespace math
