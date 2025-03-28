wrenfold
========

.. include:: ../../README.md
  :parser: myst_parser.sphinx_
  :start-after: <!--- badges_start --->
  :end-before: <!--- badges_end --->


Introduction
------------

.. include:: ../../README.md
   :parser: myst_parser.sphinx_
   :start-after: <!--- intro_start --->
   :end-before: <!--- intro_end --->


Motivation
----------

.. include:: ../../README.md
   :parser: myst_parser.sphinx_
   :start-after: <!--- motivation_start --->
   :end-before: <!--- motivation_end --->


Getting started
---------------

#. Begin with the :doc:`quick start guide <quick_start>`.
#. Peruse the :doc:`user guide <reference/index>`.
#. Check out `examples <https://github.com/wrenfold/wrenfold/tree/main/examples>`_ in the repository.
   There are also `additional examples <https://github.com/wrenfold/wrenfold-extra-examples>`_ that
   demonstrate integration of generated code into third-party optimizers.

Goals
-----
* Enable fast iteration time. Symbolic operations and code generation must be quick enough to allow
  rapid experimentation with complex expressions.
* Generate :doc:`reasonably performant code <performance>`.
* Emphasize ease of integration of generated code. This manifests in three ways:

  - We provide a :doc:`straightforward mechanism <reference/custom_types>` for incorporating
    existing types from the target code-base in generated functions.
  - The transpilation step emits a general purpose abstract syntax tree (AST) that can be
    :doc:`converted to a new language <reference/new_language>` with relative ease.
  - Generated code can :doc:`invoke external handwritten functions <reference/external_functions>`.

To get a sense of what features are in development, refer to the
`Issue Tracker <https://github.com/wrenfold/wrenfold/issues>`_.

Non-goals
---------

* Provide a full-featured computer algebra system (CAS) - realistically this is too ambitious.
  Instead, we implement mathematical operations on a need-to-have basis. wrenfold expressions can be
  converted to and from SymPy in order to unlock additional functionality.
* Be a one-stop shop for optimization. In most cases we expect the user to have an existing
  optimizer. Instead, it should be easy to customize generated code to better integrate
  with existing solutions.

Citation
--------

.. include:: ../../README.md
   :parser: myst_parser.sphinx_
   :start-after: <!--- citation_start --->
   :end-before: <!--- citation_end --->

.. toctree::
   :maxdepth: 1
   :hidden:
   :caption: Documentation

   quick_start
   building
   reference/index
   performance
   python_api/index
   cpp_api/index
   faq
   license


.. toctree::
    :hidden:
    :caption: Links

    GitHub <https://github.com/wrenfold/wrenfold/>
    Rust Documentation <https://docs.rs/wrenfold-traits>
