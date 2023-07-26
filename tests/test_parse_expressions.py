import pytest
from lark import Token, Tree

from .asserts import assert_root, assert_tree
from .expressions import expressions


def test_get_attribute(parse_tree):
    tree = parse_tree("x.length")
    expected = Tree(
        "start", [Tree("attribute", [Token("NAME", "x"), Token("NAME", "length")])]
    )
    assert tree == expected

    tree = parse_tree("ABC.z.simboxlayers")
    expected = Tree(
        "start",
        [
            Tree(
                "attribute",
                [
                    Tree("attribute", [Token("NAME", "ABC"), Token("NAME", "z")]),
                    Token("NAME", "simboxlayers"),
                ],
            )
        ],
    )
    assert tree == expected


def test_simple_subscript_array(parse_tree):
    tree = parse_tree("x[1]")
    expected = Tree(
        "start",
        [
            Tree(
                "subscript",
                [
                    Token("NAME", "x"),
                    Tree("subscript_list", [Token("INT", "1")]),
                ],
            ),
        ],
    )
    assert tree == expected

    tree = parse_tree("x[1,a,3]")
    expected = Tree(
        "start",
        [
            Tree(
                "subscript",
                [
                    Token("NAME", "x"),
                    Tree(
                        "subscript_list",
                        [
                            Token("INT", "1"),
                            Token("NAME", "a"),
                            Token("INT", "3"),
                        ],
                    ),
                ],
            ),
        ],
    )
    assert tree == expected


def test_simple_func_call(parse_tree):
    tree = parse_tree("Func(1)")
    expected = Tree(
        "start",
        [
            Tree(
                "call",
                [
                    Token("NAME", "Func"),
                    Tree("arg_list", [Token("INT", "1")]),
                ],
            ),
        ],
    )
    assert tree == expected

    tree = parse_tree('Func_B(-3.13,TRUE, ab, "fønc")')
    expected = Tree(
        "start",
        [
            Tree(
                "call",
                [
                    Token("NAME", "Func_B"),
                    Tree(
                        "arg_list",
                        [
                            Token("FLOAT", "-3.13"),
                            Token("BOOL", "TRUE"),
                            Token("NAME", "ab"),
                            Token("STRING", '"fønc"'),
                        ],
                    ),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize("op,alias", [("-", "usub"), ("+", "uadd")])
@pytest.mark.parametrize(
    "token,value",
    [
        ("INT", "-1"),
        ("INT", "453"),
        ("FLOAT", "3.14"),
        ("FLOAT", "-0.12"),
        ("BOOL", "TRUE"),
        ("BOOL", "FALSE"),
        ("NAME", "y"),
        ("STRING", '"Hello"'),
    ],
)
def test_unary_operators(parse_tree, op, alias, token, value):
    tree = parse_tree(f"{op}{value}")
    if token in ["INT", "FLOAT"] and value[0] != "-" and alias == "usub":
        expected = Tree("start", [Token(token, f"-{value}")])
    else:
        expected = Tree("start", [Tree(alias, [Token(token, value)])])
    assert tree == expected


@pytest.mark.parametrize(
    "op,alias",
    [
        ("/", "div"),
        ("*", "mult"),
        ("+", "add"),
        ("-", "sub"),
        ("<", "lt"),
        ("<=", "lte"),
        (">", "gt"),
        (">=", "gte"),
        ("=", "eq"),
        ("<>", "noteq"),
    ],
)
@pytest.mark.parametrize(
    "ltype,lvalue,rtype,rvalue",
    [
        ("INT", "1", "INT", "2"),
        ("FLOAT", "-1.3", "INT", "0"),
        ("NAME", "abc123", "BOOL", "FALSE"),
    ],
)
def test_simple_binary_ops(parse_tree, op, alias, ltype, lvalue, rtype, rvalue):
    # If not evaluated in an assignment statement (or something else like an IF)
    # the = comparator will be evaluated as assignment
    tree = parse_tree(f"x = {lvalue} {op} {rvalue}")
    expected = Tree(
        "start",
        [
            Tree(
                "assign",
                [
                    Token("NAME", "x"),
                    Tree(alias, [Token(ltype, lvalue), Token(rtype, rvalue)]),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize(
    "token,value",
    [
        ("INT", "-1"),
        ("FLOAT", "3.14"),
        ("BOOL", "TRUE"),
        ("BOOL", "FALSE"),
        ("STRING", '"Hei"'),
        ("STRING", '"It\'s \\"easy\\""'),
        ("NAME", "y"),
    ],
)
def test_simple_assign(parse_tree, token, value):
    tree = parse_tree(f"x = {value}")
    expected = Tree(
        "start", [Tree("assign", [Token("NAME", "x"), Token(token, value)])]
    )
    assert tree == expected


@pytest.mark.parametrize("expression,expected", expressions)
def test_simple_expressions(parse_tree, expression, expected):
    expected_with_root = Tree(Token("RULE", "start"), [expected])
    tree = parse_tree(expression)
    assert tree == expected_with_root


@pytest.mark.parametrize("op,op_alias", [("AND", "and_test"), ("OR", "or_test")])
@pytest.mark.parametrize("neg", ["", "NOT"])
@pytest.mark.parametrize(
    "lvalue,lclass,rvalue,rclass",
    [
        ("1", Token("INT", "1"), "3", Token("INT", "3")),
        (
            "TRUE",
            Token("BOOL", "TRUE"),
            "FALSE",
            Token("BOOL", "FALSE"),
        ),
    ],
)
def test_simple_logical_operators(
    iter_parse_tree, op, op_alias, neg, lvalue, lclass, rvalue, rclass
):
    # Test statement are only valid as conditionals in IF, FOR, WHILE, etc
    tree_iter = iter_parse_tree(f"IF {lvalue} {op} {neg} {rvalue} THEN HALT ENDIF")

    subtree = next(tree_iter)
    # Iterates from deepest leaf up and NOT op puts this statement deepest
    if neg:
        assert_tree(subtree, "not", 1)
        assert subtree.children[0] == rclass
        subtree = next(tree_iter)
    assert_tree(subtree, "halt_stmt", 0)
    subtree = next(tree_iter)
    assert_tree(subtree, op_alias, 2)
    assert subtree.children[0] == lclass
    if neg:
        assert_tree(subtree.children[1], "not", 1)
    else:
        assert subtree.children[1] == rclass
    subtree = next(tree_iter)
    assert_tree(subtree, "suite", 1)
    assert_tree(subtree.children[0], "halt_stmt", 0)

    subtree = next(tree_iter)
    assert_tree(subtree, "if_stmt", 3)
    assert subtree.children[2] is None  # No else_stmt
    subtree = next(tree_iter)
    assert_root(subtree, 1)
