# Developing

To install with development and testing dependencies,
```shell
pip install -e ".[dev]"
```

This package relies on [Lark](https://github.com/lark-parser/lark/) to
generate and transform parse trees before code generation.

To run the tests, just invoke:
```shell
pytest
```

Before creating a pull request ensure that all of the following pass:
error:
```shell
black .
flake8 .
isort .
mypy .
```
## Building the docs

You can build the docs with the following commands:

    cd docs
    make html
