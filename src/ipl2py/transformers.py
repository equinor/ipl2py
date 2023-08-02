import itertools
import logging
from typing import Any, Dict, Generic, List, TypeVar, Union

import numpy as np
from lark import Discard, Token, Tree
from lark.exceptions import GrammarError, VisitError

from .ast import (
    Array1D,
    Array2D,
    Array3D,
    Assign,
    Constant,
    Meta,
    Module,
    Name,
    Statement,
)
from .exceptions import CompilationError
from .ipl import Type
from .symtable import ScopeStackBase, SymbolTable

logger = logging.getLogger(__name__)

_Return_T = TypeVar("_Return_T")
_Leaf_T = TypeVar("_Leaf_T")


class TreeToAstTransformer(ScopeStackBase, Generic[_Leaf_T, _Return_T]):
    def __init__(self, base: SymbolTable) -> None:
        super().__init__(base)

    def default_value(self, type_: Type) -> Union[int, float, bool, str, None]:
        defaults: Dict[str, Any] = {
            "INT": 0,
            "FLOAT": 0.0,
            "BOOL": False,
            "STRING": "",
        }
        return defaults.get(type_.name)

    def _meta(self, meta: Meta) -> Meta:
        """Ensure the opaque Lark Meta type isn't used. If a module is
        empty some attributes are not created."""
        return Meta(
            line=meta.line,
            column=meta.column if hasattr(meta, "column") else 0,
            start_pos=meta.start_pos if hasattr(meta, "start_pos") else 0,
            end_line=meta.end_line if hasattr(meta, "end_line") else 0,
            end_column=meta.end_column if hasattr(meta, "end_column") else 0,
            end_pos=meta.end_pos if hasattr(meta, "end_pos") else 0,
            header_comments=meta.header_comments,
            inline_comments=meta.inline_comments,
            footer_comments=meta.footer_comments,
        )

    def _call_userfunc(self, tree, new_children=None):
        # Assumes tree is already transformed
        children = new_children if new_children is not None else tree.children
        try:
            f = getattr(self, tree.data)
        except AttributeError:
            return self.__default__(tree.data, children, tree.meta)
        else:
            try:
                wrapper = getattr(f, "visit_wrapper", None)
                if wrapper is not None:
                    return f.visit_wrapper(f, tree.data, children, tree.meta)
                else:
                    meta = self._meta(tree.meta)
                    return f(meta, children)
            except GrammarError:
                raise
            except Exception as e:
                raise VisitError(tree.data, tree, e)

    def _call_userfunc_exit(self, tree, node):
        try:
            return getattr(self, f"{tree.data}_exit")(tree, node)
        except AttributeError:
            return node

    def _call_userfunc_token(self, token) -> Union[Constant, Name]:
        try:
            f = getattr(self, token.type)
        except AttributeError:
            raise CompilationError(f"No callback for token type {token.type}")
        return f(token)

    def _transform_children(self, children):
        for c in children:
            res = c
            if isinstance(c, Tree):
                res = self._transform_tree(c)
            elif isinstance(c, Token):
                res = self._call_userfunc_token(c)

            if res is not Discard:
                yield res

    def _transform_tree(self, tree):
        children = list(self._transform_children(tree.children))
        node = self._call_userfunc(tree, children)
        exit_node = self._call_userfunc_exit(tree, node)
        return exit_node

    def transform(self, tree: Tree[_Leaf_T]) -> _Return_T:
        return self._transform_tree(tree)

    def start(self, meta: Meta, children):
        # decl_stmt returns a list that may contain lists: flatten it
        statements: List[Statement] = list(itertools.chain.from_iterable(children))
        return Module(body=statements, meta=meta)

    def decl_stmt(self, meta: Meta, assigns: List[Assign]) -> List[Assign]:
        if isinstance(assigns[0], Assign):
            return assigns
        assign, *_ = assigns
        return assign

    def decl_list(self, meta: Meta, assigns: List[Assign]) -> List[Assign]:
        return assigns

    def decl_assign(self, meta: Meta, children) -> Assign:
        name, value, *_ = children
        return Assign(targets=[name], value=value, meta=meta)

    def decl_3d(self, meta: Meta, children) -> Assign:
        name, *_ = children
        value = Array3D(value=np.array([]))
        return Assign(targets=[name], value=value, meta=meta)

    def decl_2d(self, meta: Meta, children) -> Assign:
        name, *_ = children
        value = Array2D(value=np.array([]))
        return Assign(targets=[name], value=value, meta=meta)

    def decl_1d(self, meta: Meta, children) -> Assign:
        name, *_ = children
        value = Array1D(value=np.array([]))
        return Assign(targets=[name], value=value, meta=meta)

    def decl(self, meta: Meta, children) -> Assign:
        name, *_ = children
        value = Constant(value=self.default_value(name.type))
        return Assign(targets=[name], value=value, meta=meta)

    def TYPE(self, token: Token):
        return Discard

    def STRING(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def FLOAT(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def INT(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def BOOL(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def NAME(self, token: Token) -> Name:
        symbol = self.lookup(token.value)
        return Name(id=symbol.name, type=symbol.type)

    def __default__(self, data, children, meta) -> Tree[_Leaf_T]:
        return Tree(data, children, meta)
