# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'wrenfold'
copyright = '2024, Wrenfold Authors'
author = 'Gareth Cross'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

IMPORT_PATH_REPLACEMENTS = [('pywrenfold.', 'wrenfold.')]


def apply_replacements(string: str) -> str:
    for (target, replacement) in IMPORT_PATH_REPLACEMENTS:
        string = string.replace(target, replacement)
    return string


def process_signature(app, what, name, obj, options, signature, return_annotation):
    """
    Emitted when autodoc has formatted a signature for an object. The event handler can return a new
    tuple (signature, return_annotation) to change what Sphinx puts into the output.

    Use this to fix the pybind11
    """
    if signature is not None:
        signature = apply_replacements(signature)
    if return_annotation is not None:
        return_annotation = apply_replacements(return_annotation)
    return signature, return_annotation


def process_docstring(app, what, name, obj, options, lines):
    """
    Emitted when autodoc has read and processed a docstring. lines is a list of strings - the lines
    of the processed docstring - that the event handler can modify in place to change what Sphinx
    puts into the output.
    """
    for i in range(len(lines)):
        lines[i] = apply_replacements(lines[i])


def setup(app):
    app.connect('autodoc-process-signature', process_signature)
    app.connect('autodoc-process-docstring', process_docstring)


extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    # Napoleon adds support for google-style docstrings.
    "sphinx.ext.napoleon",
    # Breathe is used to convert C++ doxygen to sphinx.
    "breathe",
    # MyST enables inclusion of markdown.
    "myst_parser"
]

templates_path = ['_templates']
exclude_patterns = []

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'furo'
html_static_path = ['_static']
html_logo = "_static/logo.png"
html_favicon = "_static/favicon.ico"

# -- Breathe configuration ---------------------------------------------------
breathe_default_project = "wf_runtime"
