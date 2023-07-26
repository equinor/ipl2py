import os
import sys

# Configuration file for Sphinx documentation builder.

# -- Setup function ----------------------------------------------------------

# Defines custom steps in the process.


def autodoc_skip_member(app, what, name, obj, skip, options):
    """Exclude all private attributes, methods, and dunder methods from Sphinx."""
    import re

    exclude = re.findall(r"\._.*", str(obj))
    return skip or exclude


def remove_module_docstring(app, what, name, obj, options, lines):
    """Remove everything after 'Author: '."""
    if what == "module":
        keep = [i for i, line in enumerate(lines) if line.startswith("Author: ")]
        if keep:
            del lines[keep[0] :]
    return


def setup(app):
    app.connect("autodoc-skip-member", autodoc_skip_member)
    app.connect("autodoc-process-docstring", remove_module_docstring)
    return


# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.

sys.path.insert(0, os.path.abspath(".."))


# -- Project information -----------------------------------------------------

project = "ipl2py"
copyright = "Equinor"
author = "Equinor"


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "myst_parser",
    "sphinx_copybutton",
    "sphinx.ext.autodoc",
    "sphinx.ext.coverage",
    "sphinx.ext.githubpages",
    "sphinx.ext.napoleon",
]

# Autodo API reference generator
autodoc_class_signature = "separated"
autodoc_inherit_docstrings = False

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ["_build"]


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
# https://sphinx-themes.org/sample-sites/furo/
html_theme = "furo"
html_title = "ipl2py"
html_theme_options = {
    "source_repository": "https://github.com/equinor/ipl2py/",
    "source_branch": "main",
    "source_directory": "docs/",
}
