from dataclasses import dataclass
from typing import List, Union

from lark import Token
from lark.visitors import Visitor_Recursive

from .symtable import SymbolTable

ExprType = Union["Compare", "Constant", "Name"]


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


@dataclass
class Constant:
    value: Union[bool, int, float, str]


@dataclass
class Lt:
    left: Constant
    right: ExprType


class Compare:
    op: Union[Lt]
    left: ExprType
    right: ExprType


class Expr(_Base):
    value: Union[Compare, Compare, Constant, Name]


@dataclass
class Assign(_Base):
    targets: List[Name]
    value: ExprType


class AstVisitor(Visitor_Recursive):
    def __init__(self, symtable: SymbolTable) -> None:
        super().__init__()
        self.symtable = symtable

    def _meta(self, meta: Meta) -> Meta:
        """Ensure the opaque Lark Meta type isn't used."""
        return Meta(
            line=meta.line,
            column=meta.column,
            start_pos=meta.start_pos,
            end_line=meta.end_line,
            end_column=meta.end_line,
            end_pos=meta.end_pos,
            header_comments=meta.header_comments,
            inline_comments=meta.inline_comments,
            footer_comments=meta.footer_comments,
        )

    def STRING(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def FLOAT(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def INT(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def BOOL(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def NAME(self, token: Token) -> Name:
        return Name(id=token.value)
