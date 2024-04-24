wrenfold
========

Introduction
------------

.. include:: ../../README.md
   :parser: myst_parser.sphinx_
   :start-after: <!--- intro_start --->
   :end-before: <!--- intro_end --->


.. warning::

  The wrenfold documentation is presently under construction. Please report missing information by
  opening a github issue.

Getting started
---------------

To begin with, first :doc:`install wrenfold <installation>` and then check out the
:doc:`tutorials <tutorial/index>` and
`examples <https://github.com/wrenfold/wrenfold/tree/main/examples>`_.

Goals
-----
* Enable fast iteration time. Symbolic operations and code generation must be quick enough to allow
  rapid experimentation with complex expressions.
* Generate reasonably performant code. TODO: Link to benchmark section of the documentation.
* Emphasize ease of integration of generated code. This manifests in three ways:

  - We provide a :doc:`straightforward mechanism <tutorial/custom_types>` for incorporating
    existing types from the target code-base in generated functions.
  - The transpilation step emits a general purpose abstract syntax tree (AST) that can be
    :doc:`converted to a new language <tutorial/new_language>` with relative ease.
  - Generated code can :doc:`invoke external handwritten functions <tutorial/external_functions>`.

To get a sense of what features are in development, check out the roadmap on the
`GitHub Wiki <https://github.com/wrenfold/wrenfold/wiki/Roadmap>`_ and the
`Issue Tracker <https://github.com/wrenfold/wrenfold/issues>`_.

Non-goals
---------

* Provide a full-featured computer algebra system (CAS) - realistically this is too ambitious.
  Instead, we implement mathematical operations on a need-to-have basis. wrenfold expressions can be
  converted to and from SymPy in order to unlock additional functionality.
* Be a one-stop shop for optimization. In most cases we expect the user to have an existing
  optimizer. Instead, it should be easy to customize generated code to better integrate
  with existing solutions.

.. toctree::
   :maxdepth: 1
   :hidden:
   :caption: Documentation

   installation
   tutorial/index
   python_api/index
   cpp_api/index
   faq
   license


.. toctree::
    :hidden:
    :caption: Links

    GitHub <https://github.com/wrenfold/wrenfold/>
