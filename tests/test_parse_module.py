from lark import Token, Tree

from ipl2py.types import IplTypes


def test_empty_module(parse_tree):
    tree = parse_tree("")
    assert tree == Tree("start", [])


def test_simple_multi_child_tree(parse_tree):
    content = """
Int a
Float b
Bool c
    """
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.INT.value),
                    Tree("decl", [Token("NAME", "a")]),
                ],
            ),
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.FLOAT.value),
                    Tree("decl", [Token("NAME", "b")]),
                ],
            ),
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.BOOL.value),
                    Tree("decl", [Token("NAME", "c")]),
                ],
            ),
        ],
    )
    assert tree == expected


def test_simple_multi_child_tree_ignoring_comments(parse_tree):
    content = """
// x
Int a
    // y
Float b // z
    """
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.INT.value),
                    Tree("decl", [Token("NAME", "a")]),
                ],
            ),
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.FLOAT.value),
                    Tree("decl", [Token("NAME", "b")]),
                ],
            ),
        ],
    )
    assert tree == expected


def test_comments_around_compound_statements(parse_tree):
    content = """
// func
FUNCTION f() // test
    Int y = 1 //a123
    x = y
ENDFUNCTION // abc
    """
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
                            Tree(
                                "decl_stmt",
                                [
                                    Token("TYPE", IplTypes.INT.value),
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


def test_very_efficient_hello_world_script(parse_tree):
    content = """
String text
Bool english = TRUE

String FUNCTION getHello()
    String hello = "Hello"
    RETURN (hello)
ENDFUNCTION

String FUNCTION getWorld()
    RETURN("world!")
ENDFUNCTION

FUNCTION doPrint(String text)
    Print(text)
ENDFUNCTION

IF english THEN
    text = getHello() + ", " + getWorld()
    doPrint(text)
ENDIF
    """
    tree = parse_tree(content)
    expected = Tree(
        "start",
        [
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.STRING.value),
                    Tree("decl", [Token("NAME", "text")]),
                ],
            ),
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", IplTypes.BOOL.value),
                    Tree(
                        "decl_assign",
                        [
                            Token("NAME", "english"),
                            Token("BOOL", "TRUE"),
                        ],
                    ),
                ],
            ),
            Tree(
                "func_def",
                [
                    Token("TYPE", IplTypes.STRING.value),
                    Token("NAME", "getHello"),
                    None,  # arg_list
                    Tree(
                        "suite",
                        [
                            Tree(
                                "decl_stmt",
                                [
                                    Token("TYPE", IplTypes.STRING.value),
                                    Tree(
                                        "decl_assign",
                                        [
                                            Token("NAME", "hello"),
                                            Token("STRING", '"Hello"'),
                                        ],
                                    ),
                                ],
                            ),
                            Tree("return_stmt", [Token("NAME", "hello")]),
                        ],
                    ),
                ],
            ),
            Tree(
                "func_def",
                [
                    Token("TYPE", IplTypes.STRING.value),
                    Token("NAME", "getWorld"),
                    None,  # arg_list
                    Tree(
                        "suite",
                        [
                            Tree("return_stmt", [Token("STRING", '"world!"')]),
                        ],
                    ),
                ],
            ),
            Tree(
                "proc_def",
                [
                    Token("NAME", "doPrint"),
                    Tree(
                        "param",
                        [
                            Token("TYPE", IplTypes.STRING.value),
                            Tree("decl", [Token("NAME", "text")]),
                        ],
                    ),
                    Tree(
                        "suite",
                        [
                            Tree(
                                "call",
                                [
                                    Token("NAME", "Print"),
                                    Tree("arg_list", [Token("NAME", "text")]),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
            Tree(
                "if_stmt",
                [
                    Token("NAME", "english"),
                    Tree(
                        "suite",
                        [
                            Tree(
                                "assign",
                                [
                                    Token("NAME", "text"),
                                    Tree(
                                        "add",
                                        [
                                            Tree(
                                                "add",
                                                [
                                                    Tree(
                                                        "call",
                                                        [
                                                            Token("NAME", "getHello"),
                                                            None,
                                                        ],
                                                    ),
                                                    Token("STRING", '", "'),
                                                ],
                                            ),
                                            Tree(
                                                "call",
                                                [
                                                    Token("NAME", "getWorld"),
                                                    None,
                                                ],
                                            ),
                                        ],
                                    ),
                                ],
                            ),
                            Tree(
                                "call",
                                [
                                    Token("NAME", "doPrint"),
                                    Tree("arg_list", [Token("NAME", "text")]),
                                ],
                            ),
                        ],
                    ),
                    None,  # no ELSE
                ],
            ),
        ],
    )
    assert tree == expected
