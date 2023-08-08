import logging

from lark import Tree

from ipl2py.ast import create_ast
from ipl2py.parser import parse
from ipl2py.symtable import create_symtable

logger = logging.getLogger(__name__)


def compile(
    content: str, include_comments=True, print_parse_tree=False, pretty=False
) -> Tree:
    tree = parse(content, include_comments=include_comments)

    if print_parse_tree:
        print(tree.pretty() if pretty else tree)

    symtable = create_symtable(tree)
    ast = create_ast(tree, symtable)
    return ast
