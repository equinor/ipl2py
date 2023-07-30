# ipl2py

[![ipl2py](https://github.com/equinor/ipl2py/actions/workflows/ipl2py.yml/badge.svg)](https://github.com/equinor/ipl2py/actions/workflows/ipl2py.yml)
![Python
Version](https://img.shields.io/badge/python-3.8%20|%203.9%20|%203.10%20|%203.11-blue.svg)
[![License: GPL v3](https://img.shields.io/github/license/equinor/ipl2py)](https://www.gnu.org/licenses/gpl-3.0)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)

**ipl2py** is a source-to-source compiler. It compiles the
[RMS](https://www.aspentech.com/en/products/sse/aspen-rms) IPL scripting
language into Python compatible with the RMS Python environment.

## Current status

**In development!** This package does not yet generate Python code.

---

## Quick Reference

* [**Installation**](#installation)
* [**Usage**](#usage)
* [**Developing**](#developing)
* [**Documentation**](#documentation)

---

## Installation

```sh
git clone git@github.com:equinor/ipl2py.git
cd ipl2py
pip install .
```

## Usage

ipl2py works on individual files. To compile `script.ipl` to Python simply

```shell
ipl2py script.ipl  # creates script.ipl.py
```

You can specify an output file as well.

```shell
ipl2py script.ipl -o script.py
```

By default comments are preserved. They can be discarded by passing the
`--no-comments` flag.

```shell
ipl2py script.ipl -o script.py --no-comments
```

## Developing

To install with development and testing dependencies,
```shell
pip install -e ".[dev]"
```

This package relies on [Lark](https://github.com/lark-parser/lark/) to
generate and transform parse trees before code generation.

To run the tests, just invoke:
```shell
pytest tests
```

Before creating a pull request ensure that all of the following pass:
```shell
black .
flake8 .
isort .
mypy src
```

## Documentation

The documentation is [available here](https://equinor.github.io/ipl2py).
