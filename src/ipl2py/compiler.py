import logging

from lark import Tree

from .parser import parse
from .symtable import SymbolTable, SymbolTableType
from .transformers import AstTransformer
from .visitors import SymbolTableVisitor

logger = logging.getLogger(__name__)


def create_symtable(tree: Tree) -> SymbolTable:
    symtable = SymbolTable("top", SymbolTableType.MODULE)
    SymbolTableVisitor(symtable).visit_topdown(tree)
    return symtable


def create_ast(tree: Tree, symtable: SymbolTable):
    return AstTransformer(symtable).transform(tree)


def compile(
    content: str, include_comments=True, print_parse_tree=False, pretty=False
) -> Tree:
    tree = parse(content, include_comments=include_comments)

    if print_parse_tree:
        print(tree.pretty() if pretty else tree)

    symtable = create_symtable(tree)
    ast = create_ast(tree, symtable)
    return ast
