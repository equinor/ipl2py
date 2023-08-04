import logging
from enum import Enum, auto
from typing import Any, Generic, List, Mapping, TypeVar, Union

import numpy as np
from lark import Discard, Token, Tree
from lark.visitors import _DiscardType

import ipl2py.ast as ast
import ipl2py.ipl as ipl

from .exceptions import CompilationError
from .symtable import ScopeStackBase, SymbolTable

logger = logging.getLogger(__name__)

ContextStack = List["Context"]
_Return_T = TypeVar("_Return_T")
_Leaf_T = TypeVar("_Leaf_T")


class Context(Enum):
    Attribute = auto()
    Subscript = auto()
    SubscriptList = auto()


class TreeToAstTransformer(ScopeStackBase, Generic[_Leaf_T, _Return_T]):
    def __init__(self, base: SymbolTable) -> None:
        super().__init__(base)
        self._context_stack: ContextStack = []

    def get_context(self) -> Union[None, Context]:
        if len(self._context_stack) == 0:
            return None
        return self._context_stack[-1]

    def push_context(self, ctx: Context) -> ContextStack:
        self._context_stack.append(ctx)
        return self._context_stack

    def pop_context(self) -> Union[None, Context]:
        if len(self._context_stack) == 0:
            return None
        ctx = self._context_stack[-1]
        self._context_stack = self._context_stack[:-1]
        return ctx

    def default_value(self, type_: ipl.Type) -> Union[int, float, bool, str, None]:
        defaults: Mapping[str, Any] = {
            "INT": 0,
            "FLOAT": 0.0,
            "BOOL": False,
            "STRING": "",
        }
        return defaults.get(type_.name)

    def _meta(self, meta: ast.Meta) -> ast.Meta:
        """Ensure the opaque Lark Meta type isn't used. If a module is
        empty some attributes are not created."""
        return ast.Meta(
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

    def _call_userfunc_exit(self, tree, node):
        try:
            return getattr(self, f"{tree.data}_exit")(tree, node)
        except AttributeError:
            return node

    def _call_userfunc(self, tree, new_children=None):
        # Assumes tree is already transformed
        children = new_children if new_children is not None else tree.children
        try:
            meta = self._meta(tree.meta)
            return getattr(self, tree.data)(meta, children)
        except AttributeError:
            return self.__default__(tree.data, children, tree.meta)

    def _call_userfunc_enter(self, tree: Tree) -> Tree:
        try:
            return getattr(self, f"{tree.data}_enter")(tree)
        except AttributeError:
            return tree

    def _call_userfunc_token(self, token) -> Union[ast.Constant, ast.Name]:
        try:
            return getattr(self, token.type)(token)
        except AttributeError:
            raise CompilationError(
                f"No callback for token type {token.type}",
                token.line,
                token.column,
            )

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
        tree = self._call_userfunc_enter(tree)
        children = list(self._transform_children(tree.children))
        node = self._call_userfunc(tree, children)
        exit_node = self._call_userfunc_exit(tree, node)
        return exit_node

    def transform(self, tree: Tree[_Leaf_T]) -> _Return_T:
        return self._transform_tree(tree)

    def start(self, meta: ast.Meta, children):
        statements: List[ast.Statement] = []
        for child in children:
            # decl_stmt returns a list: flatten it
            if isinstance(child, list):
                for subchild in child:
                    statements.append(subchild)
                continue
            statements.append(child)

        return ast.Module(body=statements, meta=meta)

    def decl_stmt(self, meta: ast.Meta, assigns: List[ast.Assign]) -> List[ast.Assign]:
        if isinstance(assigns[0], ast.Assign):
            return assigns
        assign, *_ = assigns
        return assign

    def decl_list(self, meta: ast.Meta, assigns: List[ast.Assign]) -> List[ast.Assign]:
        return assigns

    def decl_assign(self, meta: ast.Meta, children) -> ast.Assign:
        name, value, *_ = children
        return ast.Assign(targets=[name], value=value, meta=meta)

    def decl_3d(self, meta: ast.Meta, children) -> ast.Assign:
        name, *_ = children
        value = ast.Array3D(value=np.array([]))
        return ast.Assign(targets=[name], value=value, meta=meta)

    def decl_2d(self, meta: ast.Meta, children) -> ast.Assign:
        name, *_ = children
        value = ast.Array2D(value=np.array([]))
        return ast.Assign(targets=[name], value=value, meta=meta)

    def decl_1d(self, meta: ast.Meta, children) -> ast.Assign:
        name, *_ = children
        value = ast.Array1D(value=np.array([]))
        return ast.Assign(targets=[name], value=value, meta=meta)

    def decl(self, meta: ast.Meta, children) -> ast.Assign:
        name, *_ = children
        value = ast.Constant(value=self.default_value(name.type))
        return ast.Assign(targets=[name], value=value, meta=meta)

    def assign(self, meta: ast.Meta, children) -> ast.Assign:
        name, value, *_ = children
        return ast.Assign(targets=[name], value=value, meta=meta)

    def lt(self, meta: ast.Meta, children) -> ast.Compare:
        left, right = children
        return ast.Compare(left=left, op=ast.Lt(), right=right, meta=meta)

    def lte(self, meta: ast.Meta, children) -> ast.Compare:
        left, right = children
        return ast.Compare(left=left, op=ast.LtE(), right=right, meta=meta)

    def gt(self, meta: ast.Meta, children) -> ast.Compare:
        left, right = children
        return ast.Compare(left=left, op=ast.Gt(), right=right, meta=meta)

    def gte(self, meta: ast.Meta, children) -> ast.Compare:
        left, right = children
        return ast.Compare(left=left, op=ast.GtE(), right=right, meta=meta)

    def eq(self, meta: ast.Meta, children) -> ast.Compare:
        left, right = children
        return ast.Compare(left=left, op=ast.Eq(), right=right, meta=meta)

    def noteq(self, meta: ast.Meta, children) -> ast.Compare:
        left, right = children
        return ast.Compare(left=left, op=ast.NotEq(), right=right, meta=meta)

    def add(self, meta: ast.Meta, children) -> ast.BinOp:
        lhs, rhs = children
        return ast.BinOp(lhs=lhs, op=ast.Add(), rhs=rhs, meta=meta)

    def sub(self, meta: ast.Meta, children) -> ast.BinOp:
        lhs, rhs = children
        return ast.BinOp(lhs=lhs, op=ast.Sub(), rhs=rhs, meta=meta)

    def mult(self, meta: ast.Meta, children) -> ast.BinOp:
        lhs, rhs = children
        return ast.BinOp(lhs=lhs, op=ast.Mult(), rhs=rhs, meta=meta)

    def div(self, meta: ast.Meta, children) -> ast.BinOp:
        lhs, rhs = children
        return ast.BinOp(lhs=lhs, op=ast.Div(), rhs=rhs, meta=meta)

    def uadd(self, meta: ast.Meta, children) -> ast.UnaryOp:
        operand, *_ = children
        return ast.UnaryOp(op=ast.UAdd(), operand=operand, meta=meta)

    def usub(self, meta: ast.Meta, children) -> ast.UnaryOp:
        operand, *_ = children
        return ast.UnaryOp(op=ast.USub(), operand=operand, meta=meta)

    def unot(self, meta: ast.Meta, children) -> ast.UnaryOp:
        operand, *_ = children
        return ast.UnaryOp(op=ast.UNot(), operand=operand, meta=meta)

    def subscript_list_exit(self, tree: Tree, index: ast.IndexType) -> ast.IndexType:
        ctx = self.pop_context()
        if ctx != Context.SubscriptList:
            raise CompilationError(
                f"Expected `SubscriptList` context but got {ctx}",
                index.meta.line,
                index.meta.column,
            )
        return index

    def subscript_list(self, meta: ast.Meta, children) -> ast.IndexType:
        dimension = len(children)
        if dimension == 3:
            i, j, k = children
            return ast.Index3D(i=i, j=j, k=k, meta=meta)
        elif dimension == 2:
            i, j = children
            return ast.Index2D(i=i, j=j, meta=meta)
        i, *_ = children
        return ast.Index1D(i=i, meta=meta)

    def subscript_list_enter(self, tree) -> Tree:
        self.push_context(Context.SubscriptList)
        return tree

    def subscript_exit(self, tree: Tree, subscript: ast.Subscript) -> ast.Subscript:
        ctx = self.pop_context()
        if ctx != Context.Subscript:
            raise CompilationError(
                f"Expected `Subscript` context but got {ctx}",
                subscript.meta.line,
                subscript.meta.column,
            )
        return subscript

    def subscript(self, meta: ast.Meta, children) -> ast.Subscript:
        value, index, *_ = children
        return ast.Subscript(value=value, index=index, meta=meta)

    def subscript_enter(self, tree) -> Tree:
        self.push_context(Context.Subscript)
        return tree

    def attribute_exit(self, tree: Tree, attribute: ast.Attribute) -> ast.Attribute:
        ctx = self.pop_context()
        if ctx != Context.Attribute:
            raise CompilationError(
                f"Expected `Attribute` context but got {ctx}",
                attribute.meta.line,
                attribute.meta.column,
            )
        return attribute

    def attribute(self, meta: ast.Meta, children) -> ast.Attribute:
        value, attr, *_ = children
        # attr should always return as a Name with type None
        return ast.Attribute(value=value, attr=attr.id, meta=meta)

    def attribute_enter(self, tree) -> Tree:
        self.push_context(Context.Attribute)
        return tree

    def TYPE(self, token: Token) -> _DiscardType:
        return Discard

    def SYSDEF(self, token: Token) -> ast.Constant:
        return ast.Constant(value=token.value)

    def STRING(self, token: Token) -> ast.Constant:
        return ast.Constant(value=token.value)

    def FLOAT(self, token: Token) -> ast.Constant:
        return ast.Constant(value=token.value)

    def INT(self, token: Token) -> ast.Constant:
        return ast.Constant(value=token.value)

    def BOOL(self, token: Token) -> ast.Constant:
        return ast.Constant(value=token.value)

    def NAME(self, token: Token) -> Union[ast.Constant, ast.Name]:
        name = token.value
        symbol = self.lookup(name)

        # The lookup may return none, but we don't care if
        # the symbol is an attribute.
        if self.get_context() == Context.Attribute:
            return ast.Name(
                id=symbol.name if symbol else name,
                type=symbol.type if symbol else None,
            )

        if symbol is None:
            try:
                # This unknown symbol may be an IPL constant
                ipl_constant = ipl.Constant(name)
                if ipl_constant:
                    return ast.Constant(value=ipl_constant)
            except ValueError:
                pass
            raise CompilationError(
                f"Usage of unknown identifier {name}",
                token.line,
                token.column,
            )

        return ast.Name(id=symbol.name, type=symbol.type)

    def __default__(self, data, children, meta) -> Tree[_Leaf_T]:
        return Tree(data, children, meta)
