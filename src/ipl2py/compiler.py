import logging

from lark import Tree

from .parser import parse
from .symtable import SymbolTable, SymbolTableType
from .visitors import SymbolTableVisitor

logger = logging.getLogger(__name__)


def create_symbol_table(tree: Tree) -> SymbolTable:
    symbol_table = SymbolTable("top", SymbolTableType.MODULE)
    SymbolTableVisitor(symbol_table).visit_topdown(tree)
    return symbol_table


def compile(content: str, include_comments=True) -> Tree:
    tree = parse(content, include_comments=include_comments)
    symbol_table = create_symbol_table(tree)
    logger.debug("symbol_table=%s", symbol_table)
    print(symbol_table)
    return tree
