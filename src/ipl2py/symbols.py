from enum import Enum
from typing import Dict, List, Tuple, Union

from lark import Token
from lark.visitors import Visitor_Recursive

from .exceptions import CompilationError, SymbolCollision
from .ipl import Type
from .tree import Tree


class SymbolTableType(str, Enum):
    MODULE = "module"
    FUNCTION = "function"
    PROCEDURE = "procedure"


class Symbol:
    """A symbol used in a module.

    If a variable is used in a code block but not defined there,
    it will be marked as is_free."""

    def __init__(
        self,
        name: str,
        type: Type,
        is_referenced=False,
        is_parameter=False,
        is_global=False,
        is_free=False,
        is_assigned=False,
    ) -> None:
        self.name = name
        self.type = type
        self.is_referenced = is_referenced
        self.is_parameter = is_parameter
        self.is_global = is_global
        self.is_free = is_free
        self.is_assigned = is_assigned

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.is_referenced == other.is_referenced
            and self.is_parameter == other.is_parameter
            and self.is_global == other.is_global
            and self.is_free == other.is_free
            and self.is_assigned == other.is_assigned
        )

    def __repr__(self) -> str:
        return (
            "Symbol("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_referenced={self.is_referenced}, "
            f"is_parameter={self.is_parameter}, "
            f"is_global={self.is_global}, "
            f"is_free={self.is_free}, "
            f"is_assigned={self.is_assigned})"
        )


class SymbolTable:
    def __init__(self, name: str, type: SymbolTableType) -> None:
        self.name = name
        self.type = type
        self.is_optimized = False
        self.symbols: Dict[str, Symbol] = {}
        self.children: List[SymbolTable] = []

    def add_symbol(self, symbol: Symbol) -> None:
        if symbol.name not in self.symbols:
            self.symbols[symbol.name] = symbol
        else:
            raise SymbolCollision(
                f"Symbol {symbol.name} already declared in table {self.name}"
            )

    def add_child(self, symbol_table: "SymbolTable") -> None:
        self.children.append(symbol_table)

    def lookup(self, name: str) -> Union[None, Symbol]:
        if name not in self.symbols:
            return None
        return self.symbols[name]

    @property
    def identifiers(self) -> Tuple[str, ...]:
        symbols = tuple(self.symbols.keys())
        symbols += tuple(table.name for table in self.children)
        return symbols

    @property
    def has_children(self) -> bool:
        return len(self.children) > 0

    def __repr__(self) -> str:
        return (
            "SymbolTable("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_optimized={self.is_optimized}, "
            f"symbols={self.symbols}, "
            f"children={self.children})"
        )

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.is_optimized == other.is_optimized
            and self.symbols == other.symbols
            and self.children == other.children
        )


class ProcedureSymbol(SymbolTable):
    def __init__(
        self, name: str, type: SymbolTableType, params: Tuple[Symbol, ...] = tuple()
    ) -> None:
        super().__init__(name, type)
        self.params = params
        self.locals: Tuple[Symbol, ...] = tuple()
        for param in params:
            self.add_symbol(param)

    def __repr__(self) -> str:
        return (
            "ProcedureSymbol("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_optimized={self.is_optimized}, "
            f"symbols={self.symbols}, "
            f"params={self.params}, "
            f"locals={self.locals})"
        )

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.is_optimized == other.is_optimized
            and self.symbols == other.symbols
            and self.params == other.params
            and self.locals == other.locals
            and self.children == other.children
        )


class FunctionSymbol(ProcedureSymbol):
    def __init__(
        self,
        name: str,
        type: SymbolTableType,
        return_type: Type,
        params: Tuple[Symbol, ...] = tuple(),
    ) -> None:
        super().__init__(name, type, params)
        self.return_type = return_type

    def __repr__(self) -> str:
        return (
            "FunctionTable("
            f"name={self.name}, "
            f"type={self.type}, "
            f"is_optimized={self.is_optimized}, "
            f"symbols={self.symbols}, "
            f"params={self.params}, "
            f"locals={self.locals}, "
            f"return_type={self.return_type})"
        )

    def __eq__(self, other) -> bool:
        return (
            self.name == other.name
            and self.type == other.type
            and self.is_optimized == other.is_optimized
            and self.symbols == other.symbols
            and self.params == other.params
            and self.locals == other.locals
            and self.children == other.children
            and self.return_type == other.return_type
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

    def _is_global(self) -> bool:
        return self._current_table.name == "top"

    def _reference_lookup(self, token: Token) -> Symbol:
        """Checks if a symbol is in the current scope, and if not,
        if it's in the global scope."""
        symbol = self._current_table.lookup(token.value)
        # Don't qualify simple assignment as a reference
        if not symbol:
            symbol = self._top.lookup(token.value)
            if not symbol:
                raise CompilationError(
                    f"Attempted to use undeclared variable `{token.value}` "
                    f"on line {token.line}, column {token.column}"
                )
        return symbol

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
        is_global = self._is_global()
        symbol = Symbol(
            token.value,
            type_,
            is_global=is_global,
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

    def assign(self, node: Tree) -> None:
        lhs = node.children[0]
        rhs = node.children[1]
        if isinstance(lhs, Token) and lhs.type == "NAME":
            symbol = self._reference_lookup(lhs)
            symbol.is_assigned = True
        elif lhs.data in ("subscript", "attribute"):
            token = self._get_lhs_name(lhs)  # type: ignore
            symbol = self._reference_lookup(token)  # type: ignore
            symbol.is_assigned = True

        # A single assignment e.g. a = b would mark a reference
        # of b. This won't be caught elsewhere.
        if isinstance(rhs, Token) and rhs.type == "NAME":
            symbol = self._reference_lookup(rhs)
            symbol.is_referenced = True


def create_symbol_table(tree: Tree) -> SymbolTable:
    symbol_table = SymbolTable("top", SymbolTableType.MODULE)
    SymbolTableVisitor(symbol_table).visit_topdown(tree)
    return symbol_table
