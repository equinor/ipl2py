from lark import Token

from ipl2py.tree import Tree

# These are meant to be used when a parse tree becomes too deeply nested
# to sanely reconstruct it as an expected case.


def assert_root(tree: Tree, num_children) -> None:
    assert_tree(tree, "start", num_children)


def assert_tree(tree: Tree, rule: str, num_children: int) -> None:
    assert tree.data == rule
    assert len(tree.children) == num_children


def assert_terminal(token: Token, type: str, value: str) -> None:
    assert token.type == type
    assert token.value == value


def assert_comments(
    node: Tree,
    header_comments=[],
    inline_comments=[],
    footer_comments=[],
) -> None:
    assert node.meta.header_comments == header_comments  # type: ignore
    assert node.meta.inline_comments == inline_comments  # type: ignore
    assert node.meta.footer_comments == footer_comments  # type: ignore
