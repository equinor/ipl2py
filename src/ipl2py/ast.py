from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Generic, List, Mapping, Optional, TypeVar, Union

import numpy as np
import yaml
from lark import Discard, Token, Tree
from lark.visitors import _DiscardType

import ipl2py.ipl as ipl
from ipl2py.exceptions import CompilationError
from ipl2py.symtable import ScopeStackBase, SymbolTable

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

Statement = Union["Assign", "If", "While", "For", "Halt", "Function"]
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
class Halt(_Base):
    pass


@dataclass
class Return(_Base):
    value: ExprType


@dataclass
class Call(_Base):
    func: Name
    args: List[ExprType]


@dataclass
class If(_Base):
    test: TestType
    body: Body
    orelse: Optional[Body]


@dataclass
class While(_Base):
    test: TestType
    body: Body


@dataclass
class For(_Base):
    target: Name
    start: Union[Name, Constant]
    end: Union[Name, Constant]
    body: Body


@dataclass
class Param(Name):
    pass


@dataclass
class Function(_Base):
    name: Name
    params: List[Param]
    body: Body


@dataclass
class Module(_Base):
    body: List[Statement]

    def to_yaml(self) -> str:
        return yaml.dump(self)


ContextStack = List["Context"]
_Return_T = TypeVar("_Return_T")
_Leaf_T = TypeVar("_Leaf_T")


class Context(Enum):
    Attribute = auto()
    Subscript = auto()
    SubscriptList = auto()
    FunctionDecl = auto()
    Param = auto()


class AstTransformer(ScopeStackBase, Generic[_Leaf_T, _Return_T]):
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

    def _call_userfunc_token(self, token) -> Union[Constant, Name]:
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

    def _flatten_children(
        self, children: List[Union[Statement, List[Statement]]]
    ) -> List[Statement]:
        statements: List[Statement] = []
        for child in children:
            # decl_stmt returns a list
            if isinstance(child, list):
                for subchild in child:
                    statements.append(subchild)
                continue
            statements.append(child)
        return statements

    def transform(self, tree: Tree[_Leaf_T]) -> _Return_T:
        return self._transform_tree(tree)

    def start(self, meta: Meta, children):
        statements = self._flatten_children(children)
        return Module(body=statements, meta=meta)

    def func_def_exit(self, tree: Tree, procedure: Function) -> Function:
        self.pop_scope()
        return procedure

    def func_def(self, meta: Meta, children) -> Function:
        name, params, suite, *_ = children
        if not isinstance(params, list):
            params = [params] if params else []
        statements = self._flatten_children(suite)
        return Function(name=name, params=params, body=statements, meta=meta)

    def func_def_enter(self, tree: Tree) -> Tree:
        _, name, *_ = tree.children
        assert isinstance(name, Token)
        scope = self.child_lookup(name.value)
        if self.callable_lookup(name.value) is None or scope is None:
            raise CompilationError(
                f"Usage of unknown identifier {name}",
                name.line,
                name.column,
            )
        self.push_scope(scope)
        self.push_context(Context.FunctionDecl)
        return tree

    def proc_def_exit(self, tree: Tree, procedure: Function) -> Function:
        self.pop_scope()
        return procedure

    def proc_def(self, meta: Meta, children) -> Function:
        name, params, suite, *_ = children
        if not isinstance(params, list):
            params = [params] if params else []
        statements = self._flatten_children(suite)
        return Function(name=name, params=params, body=statements, meta=meta)

    def proc_def_enter(self, tree: Tree) -> Tree:
        name, *_ = tree.children
        assert isinstance(name, Token)
        scope = self.child_lookup(name.value)
        if self.callable_lookup(name.value) is None or scope is None:
            raise CompilationError(
                f"Usage of unknown identifier {name}",
                name.line,
                name.column,
            )
        self.push_scope(scope)
        self.push_context(Context.FunctionDecl)
        return tree

    def param_list(self, meta: Meta, assigns: List[Param]) -> List[Param]:
        return assigns

    def param_exit(self, tree: Tree, param: Param) -> Param:
        ctx = self.pop_context()
        if ctx != Context.Param:
            raise CompilationError(
                f"Expected {Context.Param} context but got {ctx}",
                tree.meta.line,
                tree.meta.column,
            )
        return param

    def param(self, meta: Meta, assign: List[Assign]) -> Param:
        name = assign[0].targets[0]
        return Param(id=name.id, type=name.type)

    def param_enter(self, tree: Tree) -> Tree:
        self.push_context(Context.Param)
        return tree

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

    def for_stmt(self, meta: Meta, children) -> For:
        target, start, end, suite, *_ = children
        return For(target=target, start=start, end=end, body=suite, meta=meta)

    def while_stmt(self, meta: Meta, children) -> While:
        test, suite, *_ = children
        return While(test=test, body=suite, meta=meta)

    def if_stmt(self, meta: Meta, children) -> If:
        test, if_suite, else_suite, *_ = children
        return If(test=test, body=if_suite, orelse=else_suite, meta=meta)

    def suite(self, meta: Meta, children) -> Body:
        return children

    def suite_enter(self, tree: Tree) -> Tree:
        if self.get_context() == Context.FunctionDecl:
            ctx = self.pop_context()
            if ctx != Context.FunctionDecl:
                raise CompilationError(
                    f"Expected {Context.FunctionDecl} context but got {ctx}",
                    tree.meta.line,
                    tree.meta.column,
                )
        return tree

    def arg_list(self, meta: Meta, children) -> List[ExprType]:
        return children

    def call(self, meta: Meta, children) -> Call:
        name, args, *_ = children
        return Call(func=name, args=args or [], meta=meta)

    def return_stmt(self, meta: Meta, children) -> Return:
        expr, *_ = children
        return Return(value=expr, meta=meta)

    def halt_stmt(self, meta: Meta, children) -> Halt:
        return Halt(meta=meta)

    def and_test(self, meta: Meta, children) -> BoolOp:
        left, right = children
        return BoolOp(left=left, op=And(), right=right, meta=meta)

    def or_test(self, meta: Meta, children) -> BoolOp:
        left, right = children
        return BoolOp(left=left, op=Or(), right=right, meta=meta)

    def assign(self, meta: Meta, children) -> Assign:
        name, value, *_ = children
        return Assign(targets=[name], value=value, meta=meta)

    def lt(self, meta: Meta, children) -> Compare:
        left, right = children
        return Compare(left=left, op=Lt(), right=right, meta=meta)

    def lte(self, meta: Meta, children) -> Compare:
        left, right = children
        return Compare(left=left, op=LtE(), right=right, meta=meta)

    def gt(self, meta: Meta, children) -> Compare:
        left, right = children
        return Compare(left=left, op=Gt(), right=right, meta=meta)

    def gte(self, meta: Meta, children) -> Compare:
        left, right = children
        return Compare(left=left, op=GtE(), right=right, meta=meta)

    def eq(self, meta: Meta, children) -> Compare:
        left, right = children
        return Compare(left=left, op=Eq(), right=right, meta=meta)

    def noteq(self, meta: Meta, children) -> Compare:
        left, right = children
        return Compare(left=left, op=NotEq(), right=right, meta=meta)

    def add(self, meta: Meta, children) -> BinOp:
        left, right = children
        return BinOp(left=left, op=Add(), right=right, meta=meta)

    def sub(self, meta: Meta, children) -> BinOp:
        left, right = children
        return BinOp(left=left, op=Sub(), right=right, meta=meta)

    def mult(self, meta: Meta, children) -> BinOp:
        left, right = children
        return BinOp(left=left, op=Mult(), right=right, meta=meta)

    def div(self, meta: Meta, children) -> BinOp:
        left, right = children
        return BinOp(left=left, op=Div(), right=right, meta=meta)

    def uadd(self, meta: Meta, children) -> UnaryOp:
        operand, *_ = children
        return UnaryOp(op=UAdd(), operand=operand, meta=meta)

    def usub(self, meta: Meta, children) -> UnaryOp:
        operand, *_ = children
        return UnaryOp(op=USub(), operand=operand, meta=meta)

    def unot(self, meta: Meta, children) -> UnaryOp:
        operand, *_ = children
        return UnaryOp(op=UNot(), operand=operand, meta=meta)

    def subscript_list_exit(self, tree: Tree, index: IndexType) -> IndexType:
        ctx = self.pop_context()
        if ctx != Context.SubscriptList:
            raise CompilationError(
                f"Expected {Context.SubscriptList} context but got {ctx}",
                index.meta.line,
                index.meta.column,
            )
        return index

    def subscript_list(self, meta: Meta, children) -> IndexType:
        dimension = len(children)
        if dimension == 3:
            i, j, k = children
            return Index3D(i=i, j=j, k=k, meta=meta)
        elif dimension == 2:
            i, j = children
            return Index2D(i=i, j=j, meta=meta)
        i, *_ = children
        return Index1D(i=i, meta=meta)

    def subscript_list_enter(self, tree) -> Tree:
        self.push_context(Context.SubscriptList)
        return tree

    def subscript_exit(self, tree: Tree, subscript: Subscript) -> Subscript:
        ctx = self.pop_context()
        if ctx != Context.Subscript:
            raise CompilationError(
                f"Expected {Context.Subscript} context but got {ctx}",
                subscript.meta.line,
                subscript.meta.column,
            )
        return subscript

    def subscript(self, meta: Meta, children) -> Subscript:
        value, index, *_ = children
        return Subscript(value=value, index=index, meta=meta)

    def subscript_enter(self, tree) -> Tree:
        self.push_context(Context.Subscript)
        return tree

    def attribute_exit(self, tree: Tree, attribute: Attribute) -> Attribute:
        ctx = self.pop_context()
        if ctx != Context.Attribute:
            raise CompilationError(
                f"Expected {Context.Attribute} context but got {ctx}",
                attribute.meta.line,
                attribute.meta.column,
            )
        return attribute

    def attribute(self, meta: Meta, children) -> Attribute:
        value, attr, *_ = children
        # attr should always return as a Name with type None
        return Attribute(value=value, attr=attr.id, meta=meta)

    def attribute_enter(self, tree) -> Tree:
        self.push_context(Context.Attribute)
        return tree

    def TYPE(self, token: Token) -> _DiscardType:
        return Discard

    def SYSDEF(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def STRING(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def FLOAT(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def INT(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def BOOL(self, token: Token) -> Constant:
        return Constant(value=token.value)

    def NAME(self, token: Token) -> Union[Constant, Name]:
        name = token.value

        # We don't care if a symbol exists for this name
        # if this is an attribute
        if self.get_context() == Context.Attribute:
            return Name(id=name, type=None)

        symbol = self.lookup(name)
        # This name could be the name of the callable but even though
        # we would be in its scope now, it could have a parameter or
        # local variable of the same name. FunctionDecl context is
        # popped when we enter the body. Parameters push their own
        # wrapping context so this condition won't be true for them.
        if self.get_context() == Context.FunctionDecl:
            symbol = self.callable_lookup(name)

        if symbol is not None:
            return Name(id=symbol.name, type=symbol.type)

        # If it's not a variable, parameter, or identifier within
        # a callable, it could be an IPL constant
        try:
            ipl_constant = ipl.Constant(name)
            if ipl_constant:
                return Constant(value=ipl_constant)
        except ValueError:
            pass

        symbol = self.callable_lookup(name)
        if symbol is None:
            raise CompilationError(
                f"Usage of unknown identifier {name}",
                token.line,
                token.column,
            )
        return Name(id=symbol.name, type=symbol.type)

    def __default__(self, data, children, meta) -> Tree[_Leaf_T]:
        return Tree(data, children, meta)


def create_ast(tree: Tree, symtable: SymbolTable):
    return AstTransformer(symtable).transform(tree)
