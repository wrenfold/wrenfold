# Wrenfold Speculation Prevention & Division-by-Zero Fix

This document summarizes the division-by-zero speculation hoisting bug, the architecture of the current fix, and the active status of debugging.

---

## 1. Problem Statement
In Wrenfold, common subexpression elimination (CSE) and IR optimization hoist identical expressions to their common dominator scope. When these expressions involve **unsafe operations** (such as division, square roots, logarithms, or inverse trigonometric functions), hoisting them outside their guarding conditional blocks causes them to be evaluated unconditionally.

For example, in `rotation_error2.py` (and the generated C++ in `output.py`), a guard condition ensures the norm is greater than $\epsilon$ before dividing. However, because the resulting expression was shared across all output arguments, it was hoisted to the outer scope, leading to a division-by-zero crash.

---

## 2. Potential Solution

If an expression is only used inside one particular branch of a conditional, we should not hoist it to a larger scope
where it will be evaluated outside of that branch.

For example, in the expression:

  sym.where(x > 0, 1 / x, 0)

The value 1/x should not be computed outside of the branch `if (x > 0) {}` in the generated code.