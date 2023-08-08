import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum, auto
from typing import Dict, List, Optional, Union

from lark import Token, Tree
from lark.visitors import Visitor_Recursive

import ipl2py.ipl as ipl
from ipl2py.exceptions import CompilationError, SymbolCollisionError

logger = logging.getLogger(__name__)

CallableSymbolTable = Union["ProcedureSymbolTable", "FunctionSymbolTable"]
SymbolTableNode = Union["SymbolTable", CallableSymbolTable]


class SymbolTableType(Enum):
    MODULE = auto()
    FUNCTION = auto()
    PROCEDURE = auto()


@dataclass
class Symbol:
    """A symbol used in a module.

    If a variable is used in a code block but not defined there,
    it will be marked as is_free. If a symbol identifies a function
    or procedure it is callable."""

    name: str
    type: Optional[ipl.Type]  # None exception for procedure callable type
    is_referenced: bool = False
    is_parameter: bool = False
    is_global: bool = False
    is_free: bool = False
    is_callable: bool = False
    is_assigned: bool = False


class SymbolTableBase(ABC):
    """Used for module, procedure, and function symbols.

    Note that we make identifiers (variables) distinct from
    procedures and functions (callables) because callables are
    not first-class in IPL."""

    def __init__(self, name: str, type: SymbolTableType) -> None:
        self.name = name
        self.type = type
        self.is_optimized = False
        self.symbols: Dict[str, Symbol] = {}
        self.callables: Dict[str, Symbol] = {}

    def insert_symbol(self, symbol: Symbol) -> None:
        if symbol.name in self.symbols:
            raise SymbolCollisionError(
                f"Symbol {symbol.name} already declared in table {self.name}"
            )
        if self.type == SymbolTableType.MODULE:
            symbol.is_global = True
        self.symbols[symbol.name] = symbol

    def insert_callable(self, symbol: Symbol) -> None:
        if symbol.name in self.callables:
            raise SymbolCollisionError(
                f"Symbol {symbol.name} already declared in table {self.name}"
            )
        symbol.is_global = True
        symbol.is_callable = True
        symbol.is_assigned = True
        self.callables[symbol.name] = symbol

    @abstractmethod
    def lookup(self, name: str) -> Optional[Symbol]:
        raise NotImplementedError()

    @abstractmethod
    def callable_lookup(self, name: str) -> Optional[Symbol]:
        raise NotImplementedError()

    @property
    @abstractmethod
    def identifiers(self) -> List[str]:
        """Returns a list of all accessible identifiers, including those in
        global scope."""
        raise NotImplementedError()

    @property
    @abstractmethod
    def callable_identifiers(self) -> List[str]:
        raise NotImplementedError()


class SymbolTable(SymbolTableBase):
    def __init__(self, name: str) -> None:
        super().__init__(name, SymbolTableType.MODULE)
        self.children: Dict[str, SymbolTableNode] = {}

    def lookup(self, name: str) -> Optional[Symbol]:
        symbol = self.symbols.get(name)
        if symbol is None:
            return None
        # This could have been set true from some nested scope but
        # should always be false here.
        symbol.is_free = False
        return symbol

    def callable_lookup(self, name: str) -> Optional[Symbol]:
        return self.callables.get(name)

    @property
    def identifiers(self) -> List[str]:
        return list(self.symbols.keys())

    @property
    def callable_identifiers(self) -> List[str]:
        return list(self.callables.keys())

    def insert_child(self, child: SymbolTableNode) -> None:
        if child.name in self.children:
            raise SymbolCollisionError(
                f"Symbol table {child.name} already declared in table {self.name}"
            )
        type = None
        if isinstance(child, FunctionSymbolTable):
            type = child.return_type
        symbol = Symbol(name=child.name, type=type)
        self.insert_callable(symbol)
        self.children[child.name] = child

    def get_child(self, name: str) -> Optional[SymbolTableNode]:
        return self.children.get(name)

    @property
    def has_children(self) -> bool:
        return len(self.children) > 0

    def __repr__(self) -> str:
        return (
            "SymbolTable("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_optimized={self.is_optimized}, "
            f"identifiers={self.identifiers}, "
            f"callable_identifiers={self.callable_identifiers}, "
            f"symbols={self.symbols}, "
            f"children={self.children})"
        )

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.is_optimized == other.is_optimized
            and self.symbols == other.symbols
            and self.callables == other.callables
            and self.children == other.children
        )


class ProcedureSymbolTable(SymbolTableBase):
    def __init__(self, name: str, parent: SymbolTable) -> None:
        super().__init__(name, SymbolTableType.PROCEDURE)
        self.parent = parent
        self.is_called = False

    def lookup(self, name: str) -> Optional[Symbol]:
        symbol = self.symbols.get(name)
        if symbol is None:
            symbol = self.parent.lookup(name)
            if symbol is None:
                return None
            symbol.is_free = True
        return symbol

    def callable_lookup(self, name: str) -> Optional[Symbol]:
        return self.parent.callable_lookup(name)

    @property
    def identifiers(self) -> List[str]:
        local_ = list(self.symbols.keys())
        global_ = self.parent.identifiers
        return local_ + global_

    @property
    def callable_identifiers(self) -> List[str]:
        return self.parent.callable_identifiers

    @property
    def params(self) -> List[Symbol]:
        return [symbol for symbol in self.symbols.values() if symbol.is_parameter]

    @property
    def locals(self) -> List[Symbol]:
        return [symbol for symbol in self.symbols.values() if not symbol.is_parameter]

    def __repr__(self) -> str:
        return (
            "ProcedureSymbol("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_optimized={self.is_optimized}, "
            f"identifiers={self.identifiers}, "
            f"callable_identifiers={self.callable_identifiers}, "
            f"symbols={self.symbols})"
        )

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.parent == other.parent
            and self.is_optimized == other.is_optimized
            and self.symbols == other.symbols
        )


class FunctionSymbolTable(ProcedureSymbolTable):
    def __init__(self, name: str, parent: SymbolTable, return_type: ipl.Type) -> None:
        super().__init__(name, parent)
        self.type = SymbolTableType.FUNCTION
        self.return_type = return_type

    def __repr__(self) -> str:
        return (
            "FunctionSymbol("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_optimized={self.is_optimized}, "
            f"identifiers={self.identifiers}, "
            f"callable_identifiers={self.callable_identifiers}, "
            f"symbols={self.symbols}, "
            f"return_type={self.return_type})"
        )

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.parent == other.parent
            and self.is_optimized == other.is_optimized
            and self.symbols == other.symbols
            and self.return_type == other.return_type
        )


class ScopeStack:
    def __init__(self, base: SymbolTable) -> None:
        self._base = base
        self._scope_stack: List[SymbolTableNode] = [base]

    def get_global(self) -> SymbolTable:
        return self._base

    def get_scope(self) -> SymbolTableNode:
        return self._scope_stack[-1]

    def push_scope(self, node: SymbolTableNode) -> List[SymbolTableNode]:
        self._scope_stack.append(node)
        return self._scope_stack

    def pop_scope(self) -> Optional[SymbolTableNode]:
        # Don't pop global state
        if len(self._scope_stack) == 1:
            return None
        table = self._scope_stack[-1]
        self._scope_stack = self._scope_stack[:-1]
        return table

    def lookup(self, name: str) -> Optional[Symbol]:
        return self.get_scope().lookup(name)

    def callable_lookup(self, name: str) -> Optional[Symbol]:
        return self.get_scope().callable_lookup(name)

    def child_lookup(self, name: str) -> Optional[SymbolTableNode]:
        return self.get_global().get_child(name)


class SymbolTableVisitor(ScopeStack, Visitor_Recursive):
    """This visitor generates a symbol table from the top down. This is done
    before the first-pass AST is constructed because IPL requires top declarations
    before any statements can operate on them. Converting that directly to an AST
    would mean setting empty defaults for these variables. Generating the symbol
    table first should allow us to generate a more Pythonic AST. The intent, with
    hope, is to not need secondary passes on the AST after it is generated.

    We are fortunate in that scoping is fairly flat in IPL: we only need to worry
    about globals and procedures/functions, which cannot nest.

    This visitor overrides the default `visit_topdown` function provided in its
    derived class. This is done to also call an exit function upon leaving some
    node to help keep track of scoping.
    """

    decl_types = ("decl", "decl_1d", "decl_2d", "decl_3d", "decl_assign")

    def __init__(self, base: SymbolTable) -> None:
        super().__init__(base)

    def __default_exit__(self, node: Tree) -> Tree:
        return node

    def _call_exit_userfunc(self, node: Tree) -> Tree:
        return getattr(self, f"{node.data}_exit", self.__default_exit__)(node)

    def visit_topdown(self, node: Tree) -> Tree:
        """Overrides the default visit_topdown visitor function to add an additional
        exit call after all child trees have been visited."""
        self._call_userfunc(node)  # Inherited from derived class
        for child in node.children:
            if isinstance(child, Tree):
                self.visit_topdown(child)
        self._call_exit_userfunc(node)
        return node

    def _get_lhs_name(self, node: Tree) -> Token:
        """Recursively find the name of the variable being subscripted or
        having its attribute accessed. If given a tree representing something
        like `a.length[b[0,1,2].something].size = 1` this will return
        `Token("NAME", "a")`"""
        child, *_ = node.children
        if isinstance(child, Token):
            return child
        return self._get_lhs_name(child)

    def _create_symbol(
        self, token: Token, type: ipl.Type, is_assigned=False, is_parameter=False
    ) -> None:
        assert token.type == "NAME"
        symbol = Symbol(
            token.value,
            type,
            is_assigned=is_assigned,
            is_parameter=is_parameter,
        )
        self.get_scope().insert_symbol(symbol)

    def _update_referenced_identifiers(self, node: Tree) -> None:
        # At this point all variables should be declared so we can
        # raise exceptions for undeclared identifiers.
        for child in node.children:
            if not (isinstance(child, Token) and child.type == "NAME"):
                continue
            name = child.value
            symbol = self.get_scope().lookup(name)
            if symbol is None:
                raise CompilationError(
                    f"Reference to undeclared identifier {name}",
                    child.line,
                    child.column,
                )
            symbol.is_referenced = True

    def func_def(self, node: Tree) -> None:
        type_token, id_token, *_ = node.children
        assert isinstance(type_token, Token)
        assert isinstance(id_token, Token)
        type = type_token.value
        assert isinstance(type, ipl.Type)

        root_table = self.get_global()
        function = FunctionSymbolTable(id_token.value, root_table, type)
        root_table.insert_child(function)
        self.push_scope(function)

    def func_def_exit(self, node: Tree) -> None:
        self.pop_scope()

    def proc_def(self, node: Tree) -> None:
        identifier, *_ = node.children
        assert isinstance(identifier, Token)

        root_table = self.get_global()
        procedure = ProcedureSymbolTable(identifier.value, root_table)
        root_table.insert_child(procedure)
        self.push_scope(procedure)

    def proc_def_exit(self, node: Tree) -> None:
        self.pop_scope()

    def param(self, node: Tree) -> None:
        token, *_ = node.children
        assert isinstance(token, Token)
        type = token.value
        assert isinstance(type, ipl.Type)
        self._handle_decl(node.children[1], type, is_parameter=True)

    def _handle_decl(self, node: Tree, type: ipl.Type, is_parameter=False) -> None:
        token, *_ = node.children
        assert isinstance(token, Token)

        # IPL allows assignment syntax in function parameter declarations
        # but does not assign them. This creates an unlikely edge case with
        # is_assigned
        if node.data == "decl_assign":
            self._create_symbol(
                token,
                type,
                is_assigned=False if is_parameter else True,
                is_parameter=is_parameter,
            )
            return
        self._create_symbol(token, type, is_parameter=is_parameter)

    def _decl_list(self, nodes: List[Tree], type: ipl.Type) -> None:
        for node in nodes:
            if node.data not in self.decl_types:
                raise CompilationError(
                    f"decl_list contained child with `data` attribute {node.data}",
                    node.meta.line,
                    node.meta.column,
                )
            self._handle_decl(node, type)

    def decl_stmt(self, node: Tree) -> None:
        token, *_ = node.children
        assert isinstance(token, Token)
        type = token.value
        assert isinstance(type, ipl.Type)

        # node.children may contain `decl_types` or decl_list,
        # but the type is child to the decl_stmt only.
        for child in node.children[1:]:
            if child.data in self.decl_types:
                self._handle_decl(child, type)
            elif child.data == "decl_list":
                self._decl_list(child.children, type)
            else:
                raise CompilationError(
                    f"decl_stmt contained child with `data` attribute {child.data}",
                    node.meta.line,
                    node.meta.column,
                )

    def assign(self, node: Tree) -> None:
        lhs, rhs, *_ = node.children

        if isinstance(lhs, Token) and lhs.type == "NAME":
            lhs_name = lhs.value
        elif lhs.data in ("subscript", "attribute"):
            lhs_name = self._get_lhs_name(lhs).value

        lhs_symbol = self.get_scope().lookup(lhs_name)
        if lhs_symbol is None:
            raise CompilationError(
                f"Assignment to undeclared identifier {lhs_name}",
                node.meta.line,
                node.meta.column,
            )
        lhs_symbol.is_assigned = True

        # A single assignment e.g. a = b would mark a reference
        # of b. This won't be caught elsewhere.
        if isinstance(rhs, Token) and rhs.type == "NAME":
            rhs_symbol = self.get_scope().lookup(rhs.value)
            if rhs_symbol is None:
                raise CompilationError(
                    f"Reference of undeclared identifier {rhs.value}",
                    node.meta.line,
                    node.meta.column,
                )
            rhs_symbol.is_referenced = True

    def call(self, node: Tree) -> None:
        name, *_ = node.children
        assert isinstance(name, Token)

        symbol = self.get_global().callable_lookup(name)
        if symbol is None:
            raise CompilationError(
                f"Called undeclared callable {name}",
                node.meta.line,
                node.meta.column,
            )
        symbol.is_referenced = True

    def for_stmt(self, node: Tree) -> None:
        loop_variant, *_ = node.children
        assert isinstance(loop_variant, Token)

        symbol = self.get_scope().lookup(loop_variant.value)
        if symbol is None:
            raise CompilationError(
                f"Assignment to undeclared identifier {loop_variant}",
                node.meta.line,
                node.meta.column,
            )
        symbol.is_assigned = True
        self._update_referenced_identifiers(node)

    # Prefer to allow list rather than use __default__
    def while_stmt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def if_stmt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def return_stmt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def and_test(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def or_test(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def arg_list(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def subscript_list(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def lt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def lte(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def gt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def gte(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def eq(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def noteq(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def add(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def sub(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def mult(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def div(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def uadd(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def usub(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def unot(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)


def create_symtable(tree: Tree) -> SymbolTable:
    """Create a symbol table based upon a parse tree.

    :param tree: The Lark parse tree.
    """
    symtable = SymbolTable("top")
    SymbolTableVisitor(symtable).visit_topdown(tree)
    return symtable
