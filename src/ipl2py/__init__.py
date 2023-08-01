from importlib.metadata import PackageNotFoundError, version

from .compiler import compile
from .parser import parse

try:
    _version = version(__name__)
except PackageNotFoundError:
    try:
        from .version import version as _version
    except ImportError:
        raise ImportError("Could not find version.py.")

__author__ = "Equinor"
__version__ = _version

__all__ = [
    "compile",
    "parse",
]
