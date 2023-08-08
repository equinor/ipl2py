import logging

from ipl2py.ast import Module, create_ast
from ipl2py.parser import parse
from ipl2py.symtable import create_symtable

logger = logging.getLogger(__name__)


def compile(
    content: str, include_comments=True, print_parse_tree=False, pretty=False
) -> Module:
    """This function takes a string representing a raw IPL file and compiles it
    into Python code that is compatible with the RMS Python environment. By
    default it includes comments from the original IPL file and it tries its
    best to place these in the same semantic position. It can also receive
    some arguments to print the parse tree as well.

    :param content: A string containing the raw IPL file
    :param include_comments: Preserve comments from the origin source. True
        by default.
    :param print_parse_tree: Print the full parse tree before it's converted
        into an AST. False by default.
    :param pretty: Prettify the print of the parse tree. Only relevant if
        ``print_parse_tree`` is set to True. False by default.
    """
    tree = parse(content, include_comments=include_comments)

    if print_parse_tree:
        print(tree.pretty() if pretty else tree)

    symtable = create_symtable(tree)
    ast = create_ast(tree, symtable)
    return ast
