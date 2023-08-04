from dataclasses import dataclass
from typing import List, Optional, Union

import numpy as np

import ipl2py.ipl as ipl

ArrayType = Union["Array1D", "Array2D", "Array3D"]
IndexType = Union["Index1D", "Index2D", "Index3D"]

UnaryOpsType = Union["UAdd", "USub", "UNot"]
BinOpsType = Union["Add", "Sub", "Mult", "Div"]
CompareOpsType = Union["Lt", "LtE", "Gt", "GtE", "Eq", "NotEq"]
BoolOpsType = Union["And", "Or"]

ExprType = Union[
    ArrayType, IndexType, "BinOp", "Compare", "Constant", "Name", "UnaryOp"
]
TestType = Union["BoolOp", ExprType]

Statement = Union["Assign", "If", "Halt"]
Body = List[Statement]


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
class UAdd:
    pass


@dataclass
class USub:
    pass


@dataclass
class UNot:
    pass


@dataclass
class UnaryOp(_Base):
    op: UnaryOpsType
    operand: ExprType


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
    left: ExprType
    op: BinOpsType
    right: ExprType


@dataclass
class Lt:
    pass


@dataclass
class LtE:
    pass


@dataclass
class Gt:
    pass


@dataclass
class GtE:
    pass


@dataclass
class Eq:
    pass


@dataclass
class NotEq:
    pass


# IPL's compare is left-recursive.
@dataclass
class Compare(_Base):
    left: ExprType
    op: CompareOpsType
    right: ExprType


@dataclass
class And:
    pass


@dataclass
class Or:
    pass


@dataclass
class BoolOp(_Base):
    left: ExprType
    op: BoolOpsType
    right: ExprType


@dataclass
class If(_Base):
    test: TestType
    body: Body
    orelse: Optional[Body]


@dataclass
class Halt(_Base):
    pass


@dataclass
class Expr(_Base):
    value: ExprType


@dataclass
class Module(_Base):
    body: List[Statement]
