import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum, auto
from typing import Dict, List, Union

from lark import Token, Tree
from lark.visitors import Visitor_Recursive

from .exceptions import CompilationError, SymbolCollisionError
from .ipl import Type

logger = logging.getLogger(__name__)


class SymbolTableType(Enum):
    MODULE = auto()
    FUNCTION = auto()
    PROCEDURE = auto()


@dataclass
class Symbol:
    """A symbol used in a module.

    If a variable is used in a code block but not defined there,
    it will be marked as is_free."""

    name: str
    type: Union[None, Type]
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

    def add_symbol(self, symbol: Symbol) -> None:
        if symbol.name in self.symbols:
            raise SymbolCollisionError(
                f"Symbol {symbol.name} already declared in table {self.name}"
            )
        if self.type == SymbolTableType.MODULE:
            symbol.is_global = True
        self.symbols[symbol.name] = symbol

    def add_callable(self, symbol: Symbol) -> None:
        if symbol.name in self.callables:
            raise SymbolCollisionError(
                f"Symbol {symbol.name} already declared in table {self.name}"
            )
        symbol.is_global = True
        symbol.is_callable = True
        symbol.is_assigned = True
        self.callables[symbol.name] = symbol

    @abstractmethod
    def lookup(self, name: str) -> Union[None, Symbol]:
        pass

    @abstractmethod
    def callable_lookup(self, name: str) -> Union[None, Symbol]:
        pass

    @property
    @abstractmethod
    def identifiers(self) -> List[str]:
        """Returns a tuple of all accessible identifiers, including those in
        global scope."""
        pass

    @property
    @abstractmethod
    def callable_identifiers(self) -> List[str]:
        pass


class ProcedureSymbol(SymbolTableBase):
    def __init__(self, name: str, type: SymbolTableType, parent: "SymbolTable") -> None:
        super().__init__(name, type)
        self.parent = parent
        self.is_called = False

    def add_param(self, symbol: Symbol) -> None:
        if symbol.name in self.symbols:
            raise SymbolCollisionError(
                f"Symbol {symbol.name} already declared in table {self.name}"
            )
        symbol.is_parameter = True
        self.symbols[symbol.name] = symbol

    def lookup(self, name: str) -> Union[None, Symbol]:
        symbol = self.symbols.get(name)
        if symbol is None:
            symbol = self.parent.lookup(name)
        return symbol

    def callable_lookup(self, name: str) -> Union[None, Symbol]:
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
        return [param for param in self.symbols.values() if param.is_parameter]

    @property
    def locals(self) -> List[Symbol]:
        return [param for param in self.symbols.values() if not param.is_parameter]

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


class FunctionSymbol(ProcedureSymbol):
    def __init__(
        self,
        name: str,
        type: SymbolTableType,
        parent: "SymbolTable",
        return_type: Type,
    ) -> None:
        super().__init__(name, type, parent)
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


class SymbolTable(SymbolTableBase):
    def __init__(self, name: str, type: SymbolTableType) -> None:
        super().__init__(name, type)
        self.children: Dict[str, Union[ProcedureSymbol, FunctionSymbol]] = {}

    def lookup(self, name: str) -> Union[None, Symbol]:
        return self.symbols.get(name)

    def callable_lookup(self, name: str) -> Union[None, Symbol]:
        return self.callables.get(name)

    @property
    def identifiers(self) -> List[str]:
        return list(self.symbols.keys())

    @property
    def callable_identifiers(self) -> List[str]:
        return list(self.callables.keys())

    def add_child(self, symtable: Union[ProcedureSymbol, FunctionSymbol]) -> None:
        """A child can only be a procedure/function"""
        if symtable.name in self.children:
            raise SymbolCollisionError(
                f"Symbol table {symtable.name} already declared in table {self.name}"
            )
        type = None
        if isinstance(symtable, FunctionSymbol):
            type = symtable.return_type
        symbol = Symbol(name=symtable.name, type=type)
        self.add_callable(symbol)
        self.children[symtable.name] = symtable

    def get_child(self, name: str) -> Union[None, SymbolTableBase]:
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


class SymbolTableVisitor(Visitor_Recursive):
    """This visitor generates a symbol table from the top down. This is done
    before the first-pass AST is constructed because IPL requires top declarations
    before any statements can operate on them. Converting that directly to an AST
    would mean setting empty defaults for these variables. Generating the symbol
    table first should allow us to generate a more Pythonic AST. The intent, with
    hope, is to not need secondary passes on the AST after it is generated.

    We are fortunate in that scoping is fairly flat in IPL: we only need to worry
    about globals and procedures/functions, which cannot nest.
    """

    decl_types = ("decl", "decl_1d", "decl_2d", "decl_3d", "decl_assign")

    def __init__(self, top: SymbolTable) -> None:
        super().__init__()
        self._top = top
        self._current_table = top

    def _get_lhs_name(self, node: Tree) -> Token:
        """Recursively find the name of the variable being subscripted or
        having its attribute accessed. If given a tree representing something
        like `a.length[b[0,1,2].something].size = 1` this will return
        `Token("NAME", "a")`"""
        if isinstance(node.children[0], Token):
            return node.children[0]
        return self._get_lhs_name(node.children[0])  # type: ignore

    def _create_symbol(self, token: Token, type_: Type, is_assigned=False) -> None:
        assert token.type == "NAME"
        symbol = Symbol(
            token.value,
            type_,
            is_assigned=is_assigned,
        )
        self._current_table.add_symbol(symbol)

    def _handle_decl(self, node: Tree, type_: Type) -> None:
        if node.data == "decl_assign":
            self._create_symbol(
                node.children[0], type_, is_assigned=True  # type: ignore
            )
            return
        self._create_symbol(node.children[0], type_)  # type: ignore

    def _decl_list(self, nodes: List[Tree], type_) -> None:
        for node in nodes:
            if node.data in self.decl_types:
                self._handle_decl(node, type_)  # type: ignore
                continue
            raise ValueError(
                f"decl_list contained child with `data` attribute {node.data}"
            )

    def decl_stmt(self, node: Tree) -> None:
        type_ = node.children[0].value  # type: ignore

        # node.children may contain `decl_types` or decl_list,
        # but the type is child to the decl_stmt only.
        for child in node.children[1:]:
            if child.data in self.decl_types:
                self._handle_decl(child, type_)  # type: ignore
            elif child.data == "decl_list":
                self._decl_list(child.children, type_)  # type: ignore
            else:
                raise ValueError(
                    f"decl_stmt contained child with `data` attribute {child.data}"
                )

    def _update_referenced_symbols(self, node: Tree) -> None:
        # At this point all variables should be declared so we can
        # raise exceptions for undeclared identifiers.
        for child in node.children:
            if not (isinstance(child, Token) and child.type == "NAME"):
                continue
            name = child.value
            symbol = self._current_table.lookup(name)
            if not symbol:
                raise CompilationError(f"Reference to undeclared identifier {name}")
            symbol.is_referenced = True

    # Prefer to allow list rather than use __default__
    def while_stmt(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def if_stmt(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def and_test(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def or_test(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def subscript_list(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def lt(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def lte(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def gt(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def gte(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def eq(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def noteq(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def add(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def sub(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def mult(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def div(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def uadd(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def usub(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def unot(self, node: Tree) -> None:
        self._update_referenced_symbols(node)

    def assign(self, node: Tree) -> None:
        lhs = node.children[0]
        rhs = node.children[1]

        if isinstance(lhs, Token) and lhs.type == "NAME":
            lhs_name = lhs.value
        elif lhs.data in ("subscript", "attribute"):
            lhs_name = self._get_lhs_name(lhs).value  # type: ignore

        lhs_symbol = self._current_table.lookup(lhs_name)
        if not lhs_symbol:
            raise CompilationError(f"Assignment to undeclared identifier {lhs_name}")
        lhs_symbol.is_assigned = True

        # A single assignment e.g. a = b would mark a reference
        # of b. This won't be caught elsewhere.
        if isinstance(rhs, Token) and rhs.type == "NAME":
            rhs_symbol = self._current_table.lookup(rhs.value)
            if not rhs_symbol:
                raise CompilationError(
                    f"Reference of undeclared identifier {rhs.value}"
                )
            rhs_symbol.is_referenced = True


def create_symbol_table(tree: Tree) -> SymbolTable:
    symbol_table = SymbolTable("top", SymbolTableType.MODULE)
    SymbolTableVisitor(symbol_table).visit_topdown(tree)
    return symbol_table
