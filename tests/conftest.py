import pytest
from lark import Lark

from ipl2py import parse
from ipl2py.compiler import create_ast, create_symtable
from ipl2py.grammar import GRAMMAR

# Gives assert introspection to helper functions
pytest.register_assert_rewrite("tests.asserts")


@pytest.fixture(scope="session")
def lark_parse():
    return Lark(GRAMMAR, parser="lalr").parse


@pytest.fixture()
def parse_tree(lark_parse):
    def _parse_tree(content: str):
        return lark_parse(f"{content}\n")

    return _parse_tree


@pytest.fixture()
def iter_parse_tree(lark_parse):
    def _iter_parse_tree(content: str):
        return lark_parse(f"{content}\n").iter_subtrees()

    return _iter_parse_tree


@pytest.fixture()
def symbol_table():
    def _symbol_table(content: str):
        tree = parse(content)
        return create_symtable(tree)

    return _symbol_table


@pytest.fixture()
def to_ast(symbol_table):
    def _to_ast(content: str):
        tree = parse(content)
        symtable = symbol_table(content)
        return create_ast(tree, symtable)

    return _to_ast
