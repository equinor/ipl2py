import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum, auto
from typing import Dict, List, Union

from .exceptions import SymbolCollisionError
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
        """Returns a list of all accessible identifiers, including those in
        global scope."""
        pass

    @property
    @abstractmethod
    def callable_identifiers(self) -> List[str]:
        pass


class ProcedureSymbol(SymbolTableBase):
    def __init__(self, name: str, parent: "SymbolTable") -> None:
        super().__init__(name, SymbolTableType.PROCEDURE)
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
        parent: "SymbolTable",
        return_type: Type,
    ) -> None:
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


class SymbolTable(SymbolTableBase):
    def __init__(self, name: str, type: SymbolTableType) -> None:
        super().__init__(name, type)
        self.children: Dict[str, Union[ProcedureSymbol, FunctionSymbol]] = {}

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
