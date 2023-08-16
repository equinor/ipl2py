import copy

import pytest
from lark import Token, Tree

from ipl2py.ipl import Type

from .expressions import expressions


@pytest.mark.parametrize(
    "content",
    [
        "IF TRUE THEN HALT ENDIF",
        """
IF TRUE THEN
    HALT
ENDIF""",
    ],
)
def test_simple_if_stmt(parse_tree, content):
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "if_stmt",
                [
                    Token("BOOL", "TRUE"),
                    Tree("suite", [Tree("halt_stmt", [])]),
                    None,
                ],
            )
        ],
    )
    assert tree == expected


def test_if_stmt_with_comment(parse_tree):
    content = """
IF TRUE THEN
    // a
// b
        // c
    HALT // d
    // e
ENDIF
    """
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "if_stmt",
                [
                    Token("BOOL", "TRUE"),
                    Tree("suite", [Tree("halt_stmt", [])]),
                    None,
                ],
            )
        ],
    )
    assert tree == expected


@pytest.mark.parametrize(
    "content",
    [
        "IF TRUE THEN HALT ELSE DoSomething() ENDIF",
        """
IF TRUE THEN
    HALT
ELSE
    DoSomething()
ENDIF""",
    ],
)
def test_simple_if_else_stmt(parse_tree, content):
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "if_stmt",
                [
                    Token("BOOL", "TRUE"),
                    Tree("suite", [Tree("halt_stmt", [])]),
                    Tree(
                        "suite",
                        [
                            Tree(
                                "call",
                                [
                                    Token("NAME", "DoSomething"),
                                    None,
                                ],
                            ),
                        ],
                    ),
                ],
            )
        ],
    )
    assert tree == expected


@pytest.mark.parametrize(
    "content",
    [
        """
IF TRUE THEN
    IF x = 1 THEN
        RETURN(x)
    ELSE
        HALT
    ENDIF
ELSE
    IF x = 2 THEN
        HALT
    ENDIF
    Print(2)
ENDIF
        """,
        "IF TRUE THEN "  # Valid as all inlined as well
        "IF x = 1 THEN "
        "RETURN(x) "
        "ELSE "
        "HALT "
        "ENDIF "
        "ELSE "
        "IF x = 2 THEN "
        "HALT "
        "ENDIF "
        "Print(2) "
        "ENDIF",
    ],
)
def test_nested_if_else_stmts(parse_tree, content):
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "if_stmt",
                [
                    Token("BOOL", "TRUE"),
                    Tree(
                        "suite",
                        [
                            Tree(
                                "if_stmt",
                                [
                                    Tree("eq", [Token("NAME", "x"), Token("INT", "1")]),
                                    Tree(
                                        "suite",
                                        [
                                            Tree("return_stmt", [Token("NAME", "x")]),
                                        ],
                                    ),
                                    Tree(
                                        "suite",
                                        [
                                            Tree("halt_stmt", []),
                                        ],
                                    ),
                                ],
                            ),
                        ],
                    ),
                    Tree(
                        "suite",
                        [
                            Tree(
                                "if_stmt",
                                [
                                    Tree("eq", [Token("NAME", "x"), Token("INT", "2")]),
                                    Tree(
                                        "suite",
                                        [
                                            Tree("halt_stmt", []),
                                        ],
                                    ),
                                    None,
                                ],
                            ),
                            Tree(
                                "call",
                                [
                                    Token("NAME", "Print"),
                                    Tree("arg_list", [Token("INT", "2")]),
                                ],
                            ),
                        ],
                    ),
                ],
            )
        ],
    )
    assert tree == expected


@pytest.mark.parametrize("expr,expr_expected", expressions)
@pytest.mark.parametrize("direction", ["TO", "DOWNTO"])
@pytest.mark.parametrize(
    "content",
    [
        "FOR i FROM 0 {} 10 DO {} ENDFOR",
        """
FOR i FROM 0 {} 10 DO
    {}
ENDFOR
        """,
    ],
)
def test_simple_for_loop(parse_tree, content, direction, expr, expr_expected):
    formatted_content = content.format(direction, expr)
    tree = parse_tree(formatted_content)
    expected = Tree(
        "start",
        [
            Tree(
                f"for_{direction.lower()}_stmt",
                [
                    Token("NAME", "i"),
                    Token("INT", "0"),
                    Token("INT", "10"),
                    Tree("suite", [expr_expected]),
                ],
            ),
        ],
    )
    assert tree == expected


def test_compound_for_loop(parse_tree):
    content = """
FOR i FROM col_min TO col_max DO
    FOR j FROM row_min TO row_max DO
        t = 0
        FOR k FROM 1 TO lay DO
            IF 1 THEN
                HALT
            ENDIF
        ENDFOR
        Print(j)
    ENDFOR
ENDFOR
    """
    expected = """
start
  for_to_stmt
    i
    col_min
    col_max
    suite
      for_to_stmt
        j
        row_min
        row_max
        suite
          assign
            t
            0
          for_to_stmt
            k
            1
            lay
            suite
              if_stmt
                1
                suite
                  halt_stmt
                None
          call
            Print
            arg_list	j
"""
    tree = parse_tree(content)
    assert tree.pretty() == expected.lstrip()


@pytest.mark.parametrize(
    "condition,condition_tree",
    [
        ("TRUE", Token("BOOL", "TRUE")),
        *expressions,
    ],
)
@pytest.mark.parametrize(
    "content",
    [
        "WHILE {} DO x HALT ENDWHILE",
        """
WHILE {} DO
    x
    HALT
ENDWHILE
        """,
    ],
)
def test_simple_while_loop(parse_tree, content, condition, condition_tree):
    condition_expected = copy.copy(condition_tree)
    # Assignment expressions are evaluated as equality tests in this context
    # because in IPL both assignment and equality are =
    # This may break if new expressions are added to the list with equality
    # tests in some other location.
    if isinstance(condition_expected, Tree) and condition_expected.data == "assign":
        condition_expected.data = "eq"
    tree = parse_tree(content.format(condition))
    expected = Tree(
        "start",
        [
            Tree(
                "while_stmt",
                [
                    condition_expected,
                    Tree(
                        "suite",
                        [
                            Token("NAME", "x"),
                            Tree("halt_stmt", []),
                        ],
                    ),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize(
    "content",
    [
        "FUNCTION f() HALT x = y ENDFUNCTION",
        """
FUNCTION f()
    HALT
    x = y
ENDFUNCTION
        """,
    ],
)
def test_simple_procedures(parse_tree, content):
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "proc_def",
                [
                    Token("NAME", "f"),
                    None,  # No param_list
                    Tree(
                        "suite",
                        [
                            Tree("halt_stmt", []),
                            Tree(
                                "assign",
                                [
                                    Token("NAME", "x"),
                                    Token("NAME", "y"),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize(
    "content",
    [
        "Bool FUNCTION f() Int y = 1 x = y ENDFUNCTION",
        """
Bool FUNCTION f()
    Int y = 1
    x = y
ENDFUNCTION
        """,
    ],
)
def test_simple_functions(parse_tree, content):
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "func_def",
                [
                    Token("TYPE", Type.BOOL.value),
                    Token("NAME", "f"),
                    None,  # No param_list
                    Tree(
                        "suite",
                        [
                            Tree(
                                "decl_stmt",
                                [
                                    Token("TYPE", Type.INT.value),
                                    Tree(
                                        "decl_assign",
                                        [
                                            Token("NAME", "y"),
                                            Token("INT", "1"),
                                        ],
                                    ),
                                ],
                            ),
                            Tree(
                                "assign",
                                [
                                    Token("NAME", "x"),
                                    Token("NAME", "y"),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize("func_type", ["proc_def", "func_def"])
@pytest.mark.parametrize(
    "content",
    [
        "FUNCTION f(Int a[,,], Bool b = TRUE, String c) HALT x = y ENDFUNCTION",
        """FUNCTION f(Int a[,,], Bool b = TRUE, String c)
    HALT
    x = y
ENDFUNCTION
        """,
    ],
)
def test_function_param_list(parse_tree, content, func_type):
    # Unfortunately IPL allows what look like defaults in function parameters.
    # The values given in the function declaration are discarded but it is
    # therefore technically valid syntax.
    type_content = copy.copy(content)
    if func_type == "func_def":
        type_content = f"Int {content}"
    tree = parse_tree(type_content)
    expected = Tree(
        "start",
        [
            Tree(
                func_type,
                [
                    Token("NAME", "f"),
                    Tree(
                        "param_list",
                        [
                            Tree(
                                "param",
                                [
                                    Token("TYPE", Type.INT.value),
                                    Tree(
                                        "decl_3d",
                                        [Token("NAME", "a")],
                                    ),
                                ],
                            ),
                            Tree(
                                "param",
                                [
                                    Token("TYPE", Type.BOOL.value),
                                    Tree(
                                        "decl_assign",
                                        [
                                            Token("NAME", "b"),
                                            Token("BOOL", "TRUE"),
                                        ],
                                    ),
                                ],
                            ),
                            Tree(
                                "param",
                                [
                                    Token("TYPE", Type.STRING.value),
                                    Tree(
                                        "decl",
                                        [Token("NAME", "c")],
                                    ),
                                ],
                            ),
                        ],
                    ),
                    Tree(
                        "suite",
                        [
                            Tree("halt_stmt", []),
                            Tree(
                                "assign",
                                [
                                    Token("NAME", "x"),
                                    Token("NAME", "y"),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
        ],
    )
    if func_type == "func_def":
        expected_iter = expected.find_data("func_def")
        next(expected_iter).children.insert(0, Token("TYPE", Type.INT.value))
    assert tree == expected
