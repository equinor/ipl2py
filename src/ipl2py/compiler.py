import logging

from lark import Tree

from .ast import AstVisitor
from .parser import parse
from .symtable import SymbolTable, SymbolTableType
from .visitors import SymbolTableVisitor

logger = logging.getLogger(__name__)


def create_symtable(tree: Tree) -> SymbolTable:
    symtable = SymbolTable("top", SymbolTableType.MODULE)
    SymbolTableVisitor(symtable).visit_topdown(tree)
    return symtable


def create_ast(tree: Tree, symtable: SymbolTable) -> None:
    AstVisitor(symtable).visit_topdown(tree)


def compile(content: str, include_comments=True) -> Tree:
    tree = parse(content, include_comments=include_comments)
    symtable = create_symtable(tree)
    create_ast(tree, symtable)
    return tree
