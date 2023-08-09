from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, Generator, List, Mapping, Optional, Union

import lark
import numpy as np
import yaml
from lark import Discard, Token, Tree
from lark.visitors import _DiscardType

import ipl2py.ipl as ipl
from ipl2py.exceptions import CompilationError
from ipl2py.symtable import ScopeStack, SymbolTable

PyPrimitives = Union[bool, int, float, str, None]
Primitives = Union[ipl.Constant, ipl.SysDef, PyPrimitives]

ArrayType = Union["Array1D", "Array2D", "Array3D"]
IndexType = Union["Index1D", "Index2D", "Index3D"]

UnaryOpsType = Union["UAdd", "USub", "UNot"]
BinOpsType = Union["Add", "Sub", "Mult", "Div"]
CompareOpsType = Union["Lt", "LtE", "Gt", "GtE", "Eq", "NotEq"]
BoolOpsType = Union["And", "Or"]

ExprType = Union[
    ArrayType,
    "Attribute",
    "BinOp",
    "Call",
    "Compare",
    "Constant",
    "Name",
    "Point",
    "Subscript",
    "UnaryOp",
]
TestType = Union["BoolOp", ExprType]

Body = List["Statement"]


@dataclass
class Node:
    pass


@dataclass(repr=False)
class Meta(Node):
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
class _Base(Node):
    meta: Meta


@dataclass
class Constant(Node):
    value: Primitives


@dataclass
class Name(Node):
    id: str
    type: Optional[ipl.Type]


@dataclass
class Param(Name):
    pass


@dataclass
class Point(Node):
    x: ExprType
    y: ExprType
    z: Optional[ExprType]


@dataclass
class Array1D(Node):
    value: np.ndarray


@dataclass
class Array2D(Node):
    value: np.ndarray


@dataclass
class Array3D(Node):
    value: np.ndarray


@dataclass
class Index1D(Node):
    i: ExprType


@dataclass
class Index2D(Index1D):
    j: ExprType


@dataclass
class Index3D(Index2D):
    k: ExprType


@dataclass
class UAdd(Node):
    pass


@dataclass
class USub(Node):
    pass


@dataclass
class UNot(Node):
    pass


@dataclass
class Add(Node):
    pass


@dataclass
class Sub(Node):
    pass


@dataclass
class Mult(Node):
    pass


@dataclass
class Div(Node):
    pass


@dataclass
class Lt(Node):
    pass


@dataclass
class LtE(Node):
    pass


@dataclass
class Gt(Node):
    pass


@dataclass
class GtE(Node):
    pass


@dataclass
class Eq(Node):
    pass


@dataclass
class NotEq(Node):
    pass


@dataclass
class And(Node):
    pass


@dataclass
class Or(Node):
    pass


@dataclass
class Attribute(_Base):
    value: Union[ArrayType, Name]
    attr: str


@dataclass
class Subscript(_Base):
    value: Name
    index: IndexType


@dataclass
class UnaryOp(_Base):
    op: UnaryOpsType
    operand: ExprType


@dataclass
class BinOp(_Base):
    left: ExprType
    op: BinOpsType
    right: ExprType


@dataclass
class Compare(_Base):
    left: ExprType
    op: CompareOpsType
    right: ExprType


@dataclass
class BoolOp(_Base):
    left: ExprType
    op: BoolOpsType
    right: ExprType


@dataclass
class Statement(_Base):
    pass


@dataclass
class Assign(Statement):
    targets: List[Name]
    value: ExprType


@dataclass
class Halt(Statement):
    pass


@dataclass
class Return(Statement):
    value: ExprType


@dataclass
class Call(Statement):
    func: Name
    args: List[ExprType]


@dataclass
class If(Statement):
    test: TestType
    body: Body
    orelse: Optional[Body]


@dataclass
class While(Statement):
    test: TestType
    body: Body


@dataclass
class For(Statement):
    target: Name
    start: ExprType
    end: ExprType
    body: Body


@dataclass
class Function(Statement):
    name: Name
    params: List[Param]
    body: Body


@dataclass
class Module(_Base):
    body: List[Statement]

    def to_yaml(self) -> str:
        return yaml.dump(self)


@dataclass
class AST(_Base):
    modules: List[Module]

    def to_yaml(self) -> str:
        return yaml.dump(self)


class Context(Enum):
    ATTRIBUTE = auto()
    SUBSCRIPT = auto()
    SUBSCRIPT_LIST = auto()
    FUNCTION_DECL = auto()
    PARAM = auto()


class AstTransformer(ScopeStack):
    def __init__(self, base: SymbolTable) -> None:
        super().__init__(base)
        self._context_stack: List[Context] = []

    def get_context(self) -> Optional[Context]:
        if len(self._context_stack) == 0:
            return None
        return self._context_stack[-1]

    def push_context(self, ctx: Context) -> List[Context]:
        self._context_stack.append(ctx)
        return self._context_stack

    def pop_context(self) -> Optional[Context]:
        if len(self._context_stack) == 0:
            return None
        ctx = self._context_stack[-1]
        self._context_stack = self._context_stack[:-1]
        return ctx

    def default_value(self, type_: ipl.Type) -> PyPrimitives:
        defaults: Mapping[str, Any] = {
            "INT": 0,
            "FLOAT": 0.0,
            "BOOL": False,
            "STRING": "",
        }
        return defaults.get(type_.name)

    def _meta(self, meta: lark.tree.Meta) -> Meta:
        """Ensure the opaque Lark Meta type isn't used. If a module is
        empty some attributes are not created."""
        assert hasattr(meta, "header_comments")
        assert hasattr(meta, "inline_comments")
        assert hasattr(meta, "footer_comments")
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

    def _call_userfunc_exit(self, tree: Tree, node: Node) -> Node:
        try:
            return getattr(self, f"{tree.data}_exit")(tree, node)
        except AttributeError:
            return node

    def _call_userfunc(self, tree: Tree, new_children: Optional[List[Node]]) -> Node:
        children = new_children if new_children is not None else tree.children
        try:
            meta = self._meta(tree.meta)
            return getattr(self, tree.data)(meta, children)
        except AttributeError:
            raise CompilationError(
                f"No AST callback for rule type `{tree.data}`",
                tree.meta.line,
                tree.meta.column,
            )

    def _call_userfunc_enter(self, tree: Tree) -> Tree:
        try:
            return getattr(self, f"{tree.data}_enter")(tree)
        except AttributeError:
            return tree

    def _call_userfunc_token(self, token: Token) -> Union[Constant, Name]:
        try:
            return getattr(self, token.type)(token)
        except AttributeError:
            raise CompilationError(
                f"No AST callback for token type `{token.type}`",
                token.line,
                token.column,
            )

    def _transform_children(
        self, children: List[Union[Tree, Token]]
    ) -> Generator[Node, None, None]:
        for child in children:
            node = None
            if isinstance(child, Tree):
                node = self._transform_tree(child)
            elif isinstance(child, Token):
                node = self._call_userfunc_token(child)

            if node is not Discard:
                yield node

    def _transform_tree(self, tree: Tree) -> Node:
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

    def transform(self, tree: Tree) -> Module:
        module = self._transform_tree(tree)
        assert isinstance(module, Module)
        return module

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
        self.push_context(Context.FUNCTION_DECL)
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
        self.push_context(Context.FUNCTION_DECL)
        return tree

    def param_list(self, meta: Meta, assigns: List[Param]) -> List[Param]:
        return assigns

    def param_exit(self, tree: Tree, param: Param) -> Param:
        ctx = self.pop_context()
        if ctx != Context.PARAM:
            raise CompilationError(
                f"Expected {Context.PARAM} context but got {ctx}",
                tree.meta.line,
                tree.meta.column,
            )
        return param

    def param(self, meta: Meta, assign: List[Assign]) -> Param:
        name = assign[0].targets[0]
        return Param(id=name.id, type=name.type)

    def param_enter(self, tree: Tree) -> Tree:
        self.push_context(Context.PARAM)
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
        if self.get_context() == Context.FUNCTION_DECL:
            ctx = self.pop_context()
            if ctx != Context.FUNCTION_DECL:
                raise CompilationError(
                    f"Expected {Context.FUNCTION_DECL} context but got {ctx}",
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
        if ctx != Context.SUBSCRIPT_LIST:
            raise CompilationError(
                f"Expected {Context.SUBSCRIPT_LIST} context but got {ctx}",
                tree.meta.line,
                tree.meta.column,
            )
        return index

    def subscript_list(self, meta: Meta, children) -> IndexType:
        dimension = len(children)
        if dimension == 3:
            i, j, k = children
            return Index3D(i=i, j=j, k=k)
        elif dimension == 2:
            i, j = children
            return Index2D(i=i, j=j)
        i, *_ = children
        return Index1D(i=i)

    def subscript_list_enter(self, tree) -> Tree:
        self.push_context(Context.SUBSCRIPT_LIST)
        return tree

    def subscript_exit(self, tree: Tree, subscript: Subscript) -> Subscript:
        ctx = self.pop_context()
        if ctx != Context.SUBSCRIPT:
            raise CompilationError(
                f"Expected {Context.SUBSCRIPT} context but got {ctx}",
                subscript.meta.line,
                subscript.meta.column,
            )
        return subscript

    def subscript(self, meta: Meta, children) -> Subscript:
        value, index, *_ = children
        return Subscript(value=value, index=index, meta=meta)

    def subscript_enter(self, tree) -> Tree:
        self.push_context(Context.SUBSCRIPT)
        return tree

    def attribute_exit(self, tree: Tree, attribute: Attribute) -> Attribute:
        ctx = self.pop_context()
        if ctx != Context.ATTRIBUTE:
            raise CompilationError(
                f"Expected {Context.ATTRIBUTE} context but got {ctx}",
                attribute.meta.line,
                attribute.meta.column,
            )
        return attribute

    def attribute(self, meta: Meta, children) -> Attribute:
        value, attr, *_ = children
        return Attribute(value=value, attr=attr.id, meta=meta)

    def attribute_enter(self, tree) -> Tree:
        self.push_context(Context.ATTRIBUTE)
        return tree

    def point(self, meta: Meta, children) -> Point:
        x, y, z, *_ = children
        if z is None:
            z = Constant(value=0)
        return Point(x=x, y=y, z=z)

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
        if self.get_context() == Context.ATTRIBUTE:
            return Name(id=name, type=None)

        symbol = self.lookup(name)
        # This name could be the name of the callable but even though
        # we would be in its scope now, it could have a parameter or
        # local variable of the same name. FunctionDecl context is
        # popped when we enter the body. Parameters push their own
        # wrapping context so this condition won't be true for them.
        if self.get_context() == Context.FUNCTION_DECL:
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


def create_ast(tree: Tree, symtable: SymbolTable) -> Module:
    """Transforms a parse tree into an AST, with the help of a symbol
    table.

    :param tree: The parse tree as created by ``ipl2py.parse``.
    :param symtable: The symbol table generated from the provided
        parse tree.
    """
    module: Module = AstTransformer(symtable).transform(tree)
    return module
