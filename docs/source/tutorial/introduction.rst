Introduction
============

wrenfold is composed of two parts:

#. A symbolic math frontend accessible in python.
#. A code-generation backend that converts symbolic math to runtime code.

Conceptually, the framework behaves like a `transpiler <https://en.wikipedia.org/wiki/Source-to-source_compiler>`_
that converts math into production code. At a high level, the steps involved are:

#. The user creates a python function that implements a mathematical expression. As math operations
   are composed in python, a symbolic expression tree is constructed.
#. The symbolic expression tree is flattened into to a simple intermediate representation (IR).
   Common sub-expressions are eliminated during this step.
#. An abstract syntax tree (AST) matching the simplified IR is assembled.
#. A code-generator converts the AST into a target language (eg. C++). The code-generator may be
   customized in order to suit the needs of a particular project.

At present, wrenfold can target C++ and Rust, but it can be extended to other languages relatively
easily.
