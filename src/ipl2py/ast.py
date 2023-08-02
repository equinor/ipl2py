from dataclasses import dataclass
from typing import List, Union

import numpy as np

from .ipl import Type

ArrayType = Union["Array1D", "Array2D", "Array3D"]
ExprType = Union["Compare", ArrayType, "Constant", "Name"]
Statement = Union["Assign"]


@dataclass
class Meta:
    line: int
    column: int
    start_pos: int
    end_line: int
    end_column: int
    end_pos: int
    header_comments: List[str]
    inline_comments: List[str]
    footer_comments: List[str]


@dataclass
class _Base:
    meta: Meta


@dataclass
class Name:
    id: str
    type: Union[None, Type]


@dataclass
class Constant:
    value: Union[bool, int, float, str, None]


@dataclass
class Array1D:
    value: np.ndarray


@dataclass
class Array2D:
    value: np.ndarray


@dataclass
class Array3D:
    value: np.ndarray


@dataclass
class Lt:
    left: Constant
    right: ExprType


@dataclass
class Compare:
    op: Union[Lt]
    left: ExprType
    right: ExprType


@dataclass
class Expr(_Base):
    value: ExprType


@dataclass
class Assign(_Base):
    targets: List[Name]
    value: ExprType


@dataclass
class Module(_Base):
    body: List[Statement]
