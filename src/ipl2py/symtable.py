import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum, auto
from typing import Dict, List, Union

from .exceptions import SymbolCollisionError
from .ipl import Type

logger = logging.getLogger(__name__)

ChildSymbolTable = Union["ProcedureSymbolTable", "FunctionSymbolTable"]
SymbolTableNode = Union["SymbolTable", ChildSymbolTable]
ScopeStack = List[SymbolTableNode]


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
    type: Union[None, Type]  # None exception for procedure callable type
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
    def lookup(self, name: str) -> Union[None, Symbol]:
        raise NotImplementedError()

    @abstractmethod
    def callable_lookup(self, name: str) -> Union[None, Symbol]:
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
    def __init__(self, name: str, type: SymbolTableType) -> None:
        super().__init__(name, type)
        self.children: Dict[str, ChildSymbolTable] = {}

    def lookup(self, name: str) -> Union[None, Symbol]:
        symbol = self.symbols.get(name)
        if not symbol:
            return None
        # This could have been set true from some nested scope should
        # always be false here.
        symbol.is_free = False
        return symbol

    def callable_lookup(self, name: str) -> Union[None, Symbol]:
        return self.callables.get(name)

    @property
    def identifiers(self) -> List[str]:
        return list(self.symbols.keys())

    @property
    def callable_identifiers(self) -> List[str]:
        return list(self.callables.keys())

    def insert_child(self, child: ChildSymbolTable) -> None:
        """A child can only be a procedure/function"""
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

    def get_child(self, name: str) -> Union[None, ChildSymbolTable]:
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

    def lookup(self, name: str) -> Union[None, Symbol]:
        symbol = self.symbols.get(name)
        if symbol is None:
            symbol = self.parent.lookup(name)
            if not symbol:
                return None
            symbol.is_free = True
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
    def __init__(self, name: str, parent: SymbolTable, return_type: Type) -> None:
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


class ScopeStackBase:
    def __init__(self, base: SymbolTable) -> None:
        self._base = base
        self._scope_stack: ScopeStack = [base]

    def get_global(self) -> SymbolTable:
        return self._base

    def get_scope(self) -> SymbolTableNode:
        return self._scope_stack[-1]

    def push_scope(self, node: SymbolTableNode) -> ScopeStack:
        self._scope_stack.append(node)
        return self._scope_stack

    def pop_scope(self) -> Union[None, SymbolTableNode]:
        # Don't pop global state
        if len(self._scope_stack) == 1:
            return None
        table = self._scope_stack[-1]
        self._scope_stack = self._scope_stack[:-1]
        return table

    def lookup(self, name: str) -> Union[None, Symbol]:
        return self.get_scope().lookup(name)

    def callable_lookup(self, name: str) -> Union[None, Symbol]:
        return self.get_scope().callable_lookup(name)

    def child_lookup(self, name: str) -> Union[None, SymbolTableNode]:
        return self.get_global().get_child(name)
