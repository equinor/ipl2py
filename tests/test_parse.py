import pytest
from lark import Token, Tree
from lark.exceptions import UnexpectedToken

from ipl2py import parse
from ipl2py.ipl import SysDef, Type

from .asserts import assert_comments


def test_parse_can_parse_direct_ipl():
    tree = parse("a", cache=True)
    assert tree == Tree("start", [Token("NAME", "a")])


def test_parse_can_parse_a_comment():
    tree = parse("// a", cache=True)
    assert tree == Tree("start", [])


def test_parse_raises_on_invalid_ipl():
    with pytest.raises(UnexpectedToken):
        parse("1a", cache=True)


def test_parse_collects_comments_by_default():
    # Numbers correspond to line number
    tree = parse(
        """
// 2
// 3
        """,
        cache=True,
    )
    assert_comments(
        tree, header_comments=[(2, Token("COMMENT", "2")), (3, Token("COMMENT", "3"))]
    )


def test_parse_collect_start_footer_comments():
    tree = parse(
        """
Int a
x // 3
// 4
// 5
        """,
        cache=True,
    )
    assert_comments(
        tree, footer_comments=[(4, Token("COMMENT", "4")), (5, Token("COMMENT", "5"))]
    )


def test_parse_collects_no_comments_if_disabled():
    tree = parse(
        """
// a
Int b // b c d

// eof
        """,
        include_comments=False,
        cache=True,
    )
    assert tree.meta.header_comments == []
    assert tree.meta.inline_comments == []
    assert tree.meta.footer_comments == []


def test_assign_comments_in_if_statement():
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
    tree = parse(content, cache=True)
    assert_comments(
        tree,
        header_comments=[(2, Token("COMMENT", "2"))],
        footer_comments=[(9, Token("COMMENT", "9"))],
    )

    if_node = tree.children[0]
    assert if_node.data == "if_stmt"
    assert_comments(if_node, inline_comments=[(3, Token("COMMENT", "3"))])

    if_suite = if_node.children[1]
    assert if_suite.data == "suite"
    assert_comments(
        if_suite,
        header_comments=[(4, Token("COMMENT", "4"))],
        footer_comments=[
            (6, Token("COMMENT", "6")),
            (7, Token("COMMENT", "7")),
            (8, Token("COMMENT", "8")),
        ],
    )

    halt = if_suite.children[0]
    assert halt.data == "halt_stmt"
    assert_comments(halt, inline_comments=[(5, Token("COMMENT", "5"))])


def test_assign_comments_in_if_else_statement():
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
    tree = parse(content, cache=True)
    assert_comments(
        tree,
        header_comments=[(2, Token("COMMENT", "2"))],
        footer_comments=[(12, Token("COMMENT", "12"))],
    )

    if_node = tree.children[0]
    assert if_node.data == "if_stmt"
    assert_comments(if_node, inline_comments=[(3, Token("COMMENT", "3"))])

    if_suite = if_node.children[1]
    assert if_suite.data == "suite"
    assert_comments(if_suite, header_comments=[(4, Token("COMMENT", "4"))])

    halt = if_suite.children[0]
    assert halt.data == "halt_stmt"
    assert_comments(halt, inline_comments=[(5, Token("COMMENT", "5"))])

    else_suite = if_node.children[2]
    assert else_suite.data == "suite"
    assert_comments(
        else_suite,
        header_comments=[
            (6, Token("COMMENT", "6")),
            (7, Token("COMMENT", "7")),
            (8, Token("COMMENT", "8")),
        ],
        footer_comments=[
            (10, Token("COMMENT", "10")),
            (11, Token("COMMENT", "11")),
        ],
    )

    decl_stmt = else_suite.children[0]
    assert_comments(decl_stmt, inline_comments=[(9, Token("COMMENT", "9"))])


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
    tree = parse(content, cache=True)
    assert_comments(
        tree,
        header_comments=[(2, Token("COMMENT", "2"))],
        footer_comments=[(10, Token("COMMENT", "10"))],
    )

    stmt_node = tree.children[0]
    assert stmt_node.data == stmt_type
    assert_comments(stmt_node, inline_comments=[(3, Token("COMMENT", "3"))])

    stmt_suite = stmt_node.children[1]
    if stmt_type == "proc_def":
        stmt_suite = stmt_node.children[2]
    if stmt_type in ("for_stmt", "func_def"):
        stmt_suite = stmt_node.children[3]
    assert stmt_suite.data == "suite"
    assert_comments(
        stmt_suite,
        header_comments=[
            (4, Token("COMMENT", "4")),
            (5, Token("COMMENT", "5")),
        ],
        footer_comments=[
            (7, Token("COMMENT", "7")),
            (8, Token("COMMENT", "8")),
            (9, Token("COMMENT", "9")),
        ],
    )

    halt = stmt_suite.children[0]
    assert halt.data == "halt_stmt"
    assert_comments(halt, inline_comments=[(6, Token("COMMENT", "6"))])


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
    tree = parse(content, cache=True)
    assert_comments(
        tree,
        header_comments=[(2, Token("COMMENT", "2"))],
        footer_comments=[(12, Token("COMMENT", "12"))],
    )

    outer_if_stmt = tree.children[0]
    assert outer_if_stmt.data == "if_stmt"
    assert_comments(outer_if_stmt, inline_comments=[(3, Token("COMMENT", "3"))])

    outer_if_suite = outer_if_stmt.children[1]
    assert outer_if_suite.data == "suite"
    assert_comments(
        outer_if_suite,
        header_comments=[(4, Token("COMMENT", "4"))],
        footer_comments=[
            (10, Token("COMMENT", "10")),
            (11, Token("COMMENT", "11")),
        ],
    )

    inner_if_stmt = outer_if_suite.children[0]
    assert inner_if_stmt.data == "if_stmt"
    assert_comments(inner_if_stmt, inline_comments=[(5, Token("COMMENT", "5"))])

    inner_if_suite = inner_if_stmt.children[1]
    assert inner_if_suite.data == "suite"
    assert_comments(inner_if_suite)

    if_halt = inner_if_suite.children[0]
    assert if_halt.data == "halt_stmt"
    assert_comments(if_halt, inline_comments=[(6, Token("COMMENT", "6"))])

    inner_else_suite = inner_if_stmt.children[2]
    assert inner_else_suite.data == "suite"
    assert_comments(
        inner_else_suite,
        header_comments=[(7, Token("COMMENT", "7"))],
        footer_comments=[(9, Token("COMMENT", "9"))],
    )

    else_halt = inner_else_suite.children[0]
    assert else_halt.data == "halt_stmt"
    assert_comments(else_halt, inline_comments=[(8, Token("COMMENT", "8"))])


def test_int_token_transform():
    tree = parse("1", cache=True)
    assert tree.children[0] == Token("INT", 1)
    tree = parse("-1234", cache=True)
    assert tree.children[0] == Token("INT", -1234)
    tree = parse("-0", cache=True)
    assert tree.children[0] == Token("INT", 0)


def test_float_token_transform():
    tree = parse("3.14", cache=True)
    assert tree.children[0] == Token("FLOAT", 3.14)
    tree = parse("-0.123", cache=True)
    assert tree.children[0] == Token("FLOAT", -0.123)
    tree = parse("0.0", cache=True)
    assert tree.children[0] == Token("FLOAT", 0.0)


def test_bool_token_transform():
    tree = parse("TRUE", cache=True)
    assert tree.children[0] == Token("BOOL", True)
    tree = parse("FALSE", cache=True)
    assert tree.children[0] == Token("BOOL", False)


def test_string_token_transform():
    tree = parse('"a"', cache=True)
    assert tree.children[0] == Token("STRING", "a")
    tree = parse('"I\'m"', cache=True)
    assert tree.children[0] == Token("STRING", "I'm")
    tree = parse('"\\"Hello World\\""', cache=True)
    assert tree.children[0] == Token("STRING", '\\"Hello World\\"')


def test_type_token_transform():
    for ipl_type in [t for t in Type]:
        tree = parse(f"{ipl_type.value} a", cache=True)
        assert tree.children[0].children[0] == Token("TYPE", ipl_type)
        content = f"""
    {ipl_type.value} FUNCTION f()
        TRUE
    ENDFUNCTION
            """
        tree = parse(content, cache=True)
        assert tree.children[0].children[0] == Token("TYPE", ipl_type)


def test_sysdef_token_transform():
    for sysdef in [s for s in SysDef]:
        tree = parse(sysdef.value, cache=True)
        assert tree.children[0] == Token("SYSDEF", sysdef)
