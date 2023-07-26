import pytest
from lark import Token, Tree
from lark.exceptions import UnexpectedToken

from ipl2py import parse


def test_parse_can_parse_direct_ipl():
    tree = parse("a")
    assert tree == Tree("start", [Token("NAME", "a")])


def test_parse_can_parse_a_comment():
    tree = parse("// a")
    assert tree == Tree("start", [])


def test_parse_raises_on_invalid_ipl():
    with pytest.raises(UnexpectedToken):
        parse("1a")


def test_parse_collects_comments_by_default():
    tree = parse(
        """
// a
// 1
        """
    )
    assert len(tree.meta.header_comments) == 2


def test_parse_collect_start_footer_comments():
    tree = parse(
        """
Int a
x // 1
// 2
// 3
        """
    )
    assert len(tree.meta.footer_comments) == 2


def test_parse_collects_no_comments_if_disabled():
    tree = parse(
        """
// a
Int b // b c d

// eof
        """,
        include_comments=False,
    )
    assert not hasattr(tree.meta, "header_comments")
    assert not hasattr(tree.meta, "footer_comments")
    assert not hasattr(tree.children[0].meta, "inline_comments")


def test_assign_comments_in_if_statement():
    # Numbers correspond to line number
    content = """
// 2
IF TRUE THEN // 3
    // 4
    HALT // 5
    // 6
    // 7
ENDIF // 8
// 9
    """
    tree = parse(content)
    assert hasattr(tree.meta, "header_comments")
    assert tree.meta.header_comments == [(2, Token("COMMENT", "2"))]
    assert hasattr(tree.meta, "footer_comments")
    assert tree.meta.footer_comments == [(9, Token("COMMENT", "9"))]
    if_node = tree.children[0]
    assert if_node.data == "if_stmt"
    assert hasattr(if_node.meta, "inline_comments")
    assert if_node.meta.inline_comments == [(3, Token("COMMENT", "3"))]
    if_suite = if_node.children[1]
    assert if_suite.data == "suite"
    assert hasattr(if_suite.meta, "header_comments")
    assert if_suite.meta.header_comments == [(4, Token("COMMENT", "4"))]
    assert hasattr(if_suite.meta, "footer_comments")
    assert if_suite.meta.footer_comments == [
        (6, Token("COMMENT", "6")),
        (7, Token("COMMENT", "7")),
        (8, Token("COMMENT", "8")),
    ]
    halt = if_suite.children[0]
    assert halt.data == "halt_stmt"
    assert hasattr(halt.meta, "inline_comments")
    assert halt.meta.inline_comments == [(5, Token("COMMENT", "5"))]


def test_assign_comments_in_if_else_statement():
    # Numbers correspond to line number
    content = """
// 2
IF TRUE THEN // 3
    // 4
    HALT // 5
    // 6
    // 7
ELSE // 8
    Int x // 9
    // 10
ENDIF // 11
// 12
    """
    tree = parse(content)
    assert hasattr(tree.meta, "header_comments")
    assert tree.meta.header_comments == [(2, Token("COMMENT", "2"))]
    assert hasattr(tree.meta, "footer_comments")
    assert tree.meta.footer_comments == [(12, Token("COMMENT", "12"))]
    if_node = tree.children[0]
    assert if_node.data == "if_stmt"
    assert hasattr(if_node.meta, "inline_comments")
    assert if_node.meta.inline_comments == [(3, Token("COMMENT", "3"))]
    if_suite = if_node.children[1]
    assert if_suite.data == "suite"
    assert hasattr(if_suite.meta, "header_comments")
    assert if_suite.meta.header_comments == [(4, Token("COMMENT", "4"))]
    assert hasattr(if_suite.meta, "footer_comments")
    assert if_suite.meta.footer_comments == []
    halt = if_suite.children[0]
    assert halt.data == "halt_stmt"
    assert hasattr(halt.meta, "inline_comments")
    assert halt.meta.inline_comments == [(5, Token("COMMENT", "5"))]
    else_suite = if_node.children[2]
    assert else_suite.data == "suite"
    assert hasattr(else_suite.meta, "header_comments")
    assert else_suite.meta.header_comments == [
        (6, Token("COMMENT", "6")),
        (7, Token("COMMENT", "7")),
        (8, Token("COMMENT", "8")),
    ]
    assert hasattr(else_suite.meta, "footer_comments")
    assert else_suite.meta.footer_comments == [
        (10, Token("COMMENT", "10")),
        (11, Token("COMMENT", "11")),
    ]
    decl_stmt = else_suite.children[0]
    assert hasattr(decl_stmt.meta, "inline_comments")
    assert decl_stmt.meta.inline_comments == [(9, Token("COMMENT", "9"))]


@pytest.mark.parametrize(
    "stmt_type,content",
    [
        (
            "for_stmt",
            """
// 2
FOR i FROM 1 TO 10 DO // 3
    // 4
    // 5
    HALT // 6
    // 7
    // 8
ENDFOR // 9
// 10
        """,
        ),
        (
            "while_stmt",
            """
// 2
WHILE 1 DO // 3
    // 4
    // 5
    HALT // 6
    // 7
    // 8
ENDWHILE // 9
// 10
        """,
        ),
        (
            "func_def",
            """
// 2
Int FUNCTION f(Int a, Int b) // 3
    // 4
    // 5
    HALT // 6
    // 7
    // 8
ENDFUNCTION // 9
// 10
        """,
        ),
        (
            "proc_def",
            """
// 2
FUNCTION f(Int a, Int b) // 3
    // 4
    // 5
    HALT // 6
    // 7
    // 8
ENDFUNCTION // 9
// 10
        """,
        ),
    ],
)
def test_assign_comments_compound_statements(stmt_type, content):
    tree = parse(content)
    assert hasattr(tree.meta, "header_comments")
    assert tree.meta.header_comments == [(2, Token("COMMENT", "2"))]
    assert hasattr(tree.meta, "footer_comments")
    assert tree.meta.footer_comments == [(10, Token("COMMENT", "10"))]
    stmt_node = tree.children[0]
    assert stmt_node.data == stmt_type
    assert hasattr(stmt_node.meta, "inline_comments")
    assert stmt_node.meta.inline_comments == [(3, Token("COMMENT", "3"))]
    stmt_suite = stmt_node.children[1]
    if stmt_type == "proc_def":
        stmt_suite = stmt_node.children[2]
    if stmt_type in ("for_stmt", "func_def"):
        stmt_suite = stmt_node.children[3]
    assert stmt_suite.data == "suite"
    assert hasattr(stmt_suite.meta, "header_comments")
    assert stmt_suite.meta.header_comments == [
        (4, Token("COMMENT", "4")),
        (5, Token("COMMENT", "5")),
    ]
    assert hasattr(stmt_suite.meta, "footer_comments")
    assert stmt_suite.meta.footer_comments == [
        (7, Token("COMMENT", "7")),
        (8, Token("COMMENT", "8")),
        (9, Token("COMMENT", "9")),
    ]
    halt = stmt_suite.children[0]
    assert halt.data == "halt_stmt"
    assert hasattr(halt.meta, "inline_comments")
    assert halt.meta.inline_comments == [(6, Token("COMMENT", "6"))]


def test_comments_in_nested_if_stmt():
    content = """
// 2
IF TRUE THEN // 3
    // 4
    IF TRUE THEN // 5
        HALT // 6
    ELSE // 7
        HALT // 8
    ENDIF // 9
    // 10
ENDIF // 11
// 12
    """
    tree = parse(content)
    assert hasattr(tree.meta, "header_comments")
    assert tree.meta.header_comments == [(2, Token("COMMENT", "2"))]
    assert hasattr(tree.meta, "footer_comments")
    assert tree.meta.footer_comments == [(12, Token("COMMENT", "12"))]

    outer_if_stmt = tree.children[0]
    assert outer_if_stmt.data == "if_stmt"
    assert hasattr(outer_if_stmt.meta, "inline_comments")
    assert outer_if_stmt.meta.inline_comments == [(3, Token("COMMENT", "3"))]

    outer_if_suite = outer_if_stmt.children[1]
    assert outer_if_suite.data == "suite"
    assert hasattr(outer_if_suite.meta, "header_comments")
    assert outer_if_suite.meta.header_comments == [(4, Token("COMMENT", "4"))]
    assert hasattr(outer_if_suite.meta, "footer_comments")
    assert outer_if_suite.meta.footer_comments == [
        (10, Token("COMMENT", "10")),
        (11, Token("COMMENT", "11")),
    ]

    inner_if_stmt = outer_if_suite.children[0]
    assert inner_if_stmt.data == "if_stmt"
    assert hasattr(inner_if_stmt.meta, "inline_comments")
    assert inner_if_stmt.meta.inline_comments == [(5, Token("COMMENT", "5"))]

    inner_if_suite = inner_if_stmt.children[1]
    assert inner_if_suite.data == "suite"
    assert inner_if_suite.meta.header_comments == []
    assert inner_if_suite.meta.footer_comments == []
    if_halt = inner_if_suite.children[0]
    assert if_halt.data == "halt_stmt"
    assert hasattr(if_halt.meta, "inline_comments")
    assert if_halt.meta.inline_comments == [(6, Token("COMMENT", "6"))]

    inner_else_suite = inner_if_stmt.children[2]
    assert inner_else_suite.data == "suite"
    assert hasattr(inner_else_suite.meta, "header_comments")
    assert inner_else_suite.meta.header_comments == [(7, Token("COMMENT", "7"))]
    assert hasattr(inner_else_suite.meta, "footer_comments")
    assert inner_else_suite.meta.footer_comments == [(9, Token("COMMENT", "9"))]

    else_halt = inner_else_suite.children[0]
    assert else_halt.data == "halt_stmt"
    assert hasattr(else_halt.meta, "inline_comments")
    assert else_halt.meta.inline_comments == [(8, Token("COMMENT", "8"))]


def test_int_token_transform():
    tree = parse("1")
    assert tree.children[0] == Token("INT", 1)
    tree = parse("-1234")
    assert tree.children[0] == Token("INT", -1234)
    tree = parse("-0")
    assert tree.children[0] == Token("INT", 0)


def test_float_token_transform():
    tree = parse("3.14")
    assert tree.children[0] == Token("FLOAT", 3.14)
    tree = parse("-0.123")
    assert tree.children[0] == Token("FLOAT", -0.123)
    tree = parse("0.0")
    assert tree.children[0] == Token("FLOAT", 0.0)


def test_bool_token_transform():
    tree = parse("TRUE")
    assert tree.children[0] == Token("BOOL", True)
    tree = parse("FALSE")
    assert tree.children[0] == Token("BOOL", False)


def test_string_token_transform():
    tree = parse('"a"')
    assert tree.children[0] == Token("STRING", "a")
    tree = parse('"I\'m"')
    assert tree.children[0] == Token("STRING", "I'm")
    tree = parse('"\\"Hello World\\""')
    assert tree.children[0] == Token("STRING", '\\"Hello World\\"')
