from dataclasses import dataclass
from typing import List, Union

import numpy as np

import ipl2py.ipl as ipl

ArrayType = Union["Array1D", "Array2D", "Array3D"]
BinOpsType = Union["Add", "Sub", "Mult", "Div"]
ExprType = Union[ArrayType, BinOpsType, "Compare", "Constant", "Name"]
IndexType = Union["Index1D", "Index2D", "Index3D"]
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

    def __repr__(self) -> str:
        return ""


@dataclass
class _Base:
    meta: Meta


@dataclass
class Constant:
    value: Union[bool, int, float, str, ipl.Constant, ipl.SysDef, None]


@dataclass
class Name:
    id: str
    type: Union[None, ipl.Type]


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
class Assign(_Base):
    targets: List[Name]
    value: ExprType


@dataclass
class Attribute(_Base):
    value: Union[ArrayType, Name]
    attr: str


@dataclass
class Index1D(_Base):
    i: ExprType


@dataclass
class Index2D(Index1D):
    j: ExprType


@dataclass
class Index3D(Index2D):
    k: ExprType


@dataclass
class Subscript(_Base):
    value: Name
    index: IndexType


@dataclass
class Add:
    pass


@dataclass
class Sub:
    pass


@dataclass
class Mult:
    pass


@dataclass
class Div:
    pass


@dataclass
class BinOp(_Base):
    lhs: ExprType
    op: BinOpsType
    rhs: ExprType


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
class Module(_Base):
    body: List[Statement]
