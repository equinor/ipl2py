import pytest
from lark import Lark

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
