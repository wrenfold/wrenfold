# Contributing to wrenfold

Contributions to wrenfold are welcome. For instructions on setting up your environment for development, see [Building from source](https://wrenfold.org/building).

**Legal notice**: By submitting to this project, you represent that you have the necessary rights to your contribution and that the content you contribute may be provided under the project license (MIT).

When submitting a PR, please adhere to the following:
- All changes must be formatted by the [pre-commit](https://pre-commit.com) rules. Pre-commit will automatically format C++ (clang-format), Python (yapf), and Rust (rustfmt) code.
- C++ code uses snake-case convention throughout.
- Python code uses snake-case for functions and camel-case for types.
- Python methods should be type-annotated whenever possible, and docstrings should adhere to [Google Style](https://www.sphinx-doc.org/en/master/usage/extensions/example_google.html).
