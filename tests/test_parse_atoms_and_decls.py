import pytest
from lark import Token, Tree
from lark.exceptions import UnexpectedToken

from ipl2py.types import IplTypes

ipl_types = [t.value for t in IplTypes]


@pytest.mark.parametrize(
    "token,value",
    [
        ("INT", "0"),
        ("INT", "-65537"),
        ("FLOAT", "3.14"),
        ("FLOAT", "-100.0"),
        ("STRING", '"HELLO"'),
        ("STRING", "'''\"world\""),
        ("STRING", "'\"Hello\"'"),
        ("STRING", '"Hei øåÆ x"'),
        ("BOOL", "TRUE"),
        ("BOOL", "FALSE"),
        ("NAME", "x"),
        ("NAME", "__x"),
        ("NAME", "Valid_variable"),
        ("SYSDEF", "@GRIDMODELS"),
        ("SYSDEF", "@HORIZONS"),
        ("SYSDEF", "@X"),
        ("SYSDEF", "@X1"),
        ("SYSDEF", "@Y"),
        ("SYSDEF", "@Y4"),
        ("SYSDEF", "@Z"),
        ("SYSDEF", "@Z8"),
        ("SYSDEF", "@dX"),
        ("SYSDEF", "@dY"),
        ("SYSDEF", "@dZ"),
        ("SYSDEF", "@I"),
        ("SYSDEF", "@J"),
        ("SYSDEF", "@K"),
        ("SYSDEF", "@N_GRIDMODELS"),
        ("SYSDEF", "@N_WELLS"),
        ("SYSDEF", "@POPDOWN"),
    ],
)
def test_valid_simple_tokens(parse_tree, token, value):
    tree = parse_tree(value)
    expected = Tree("start", [Token(token, value)])
    assert tree == expected


def test_valid_point_token(parse_tree):
    tree = parse_tree("(1,2.1)")
    expected = Tree(
        "start", [Tree("point", [Token("INT", "1"), Token("FLOAT", "2.1")])]
    )
    assert tree == expected

    tree = parse_tree("(@X1,  Y2, -0)")
    expected = Tree(
        "start",
        [
            Tree(
                "point",
                [
                    Token("SYSDEF", "@X1"),
                    Token("NAME", "Y2"),
                    Token("INT", "-0"),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize(
    "token,value",
    [
        ("FLOAT", "3."),
        ("NAME", "1_x"),
        ("INT", "1e10"),
        ("FLOAT", "-1E-10"),
        ("POINT", "(1,2,3,4)"),
    ],
)
def test_invalid_simple_tokens(parse_tree, token, value):
    with pytest.raises(UnexpectedToken):
        parse_tree(f"{value}")


@pytest.mark.parametrize("ipl_type", ipl_types)
@pytest.mark.parametrize(
    "dimension,decl_type",
    [
        ("", "decl"),
        ("[]", "decl_1d"),
        ("[,]", "decl_2d"),
        ("[,,]", "decl_3d"),
    ],
)
def test_single_declaration(parse_tree, ipl_type, dimension, decl_type):
    tree = parse_tree(f"{ipl_type} x{dimension}")
    expected = Tree(
        "start",
        [
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", ipl_type),
                    Tree(decl_type, [Token("NAME", "x")]),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize("ipl_type", ipl_types)
def test_single_decl_assign(parse_tree, ipl_type):
    tree = parse_tree(f"{ipl_type} x = y")
    expected = Tree(
        "start",
        [
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", ipl_type),
                    Tree("decl_assign", [Token("NAME", "x"), Token("NAME", "y")]),
                ],
            ),
        ],
    )
    assert tree == expected


@pytest.mark.parametrize("ipl_type", ipl_types)
@pytest.mark.parametrize("dimension", ["[]", "[,]", "[,,]"])
def test_nonscalar_assigns_fail(parse_tree, ipl_type, dimension):
    with pytest.raises(UnexpectedToken):
        parse_tree(f"{ipl_type} x{dimension} = 0")


@pytest.mark.parametrize("ipl_type", ipl_types)
def test_list_declaration(parse_tree, ipl_type):
    tree = parse_tree(f'{ipl_type} a = "123", bc[],def[,], g[,,], hi')
    expected = Tree(
        "start",
        [
            Tree(
                "decl_stmt",
                [
                    Token("TYPE", ipl_type),
                    Tree(
                        "decl_list",
                        [
                            Tree(
                                "decl_assign",
                                [Token("NAME", "a"), Token("STRING", '"123"')],
                            ),
                            Tree("decl_1d", [Token("NAME", "bc")]),
                            Tree("decl_2d", [Token("NAME", "def")]),
                            Tree("decl_3d", [Token("NAME", "g")]),
                            Tree("decl", [Token("NAME", "hi")]),
                        ],
                    ),
                ],
            ),
        ],
    )
    assert tree == expected
