from lark import Token

from ipl2py.tree import Tree

# This file contains expressions and their expected parse tree representation
expressions = [
    ("(1+1)", Tree("add", [Token("INT", "1"), Token("INT", "1")])),
    (
        "a + 1 - b",
        Tree(
            "sub",
            [
                Tree("add", [Token("NAME", "a"), Token("INT", "1")]),
                Token("NAME", "b"),
            ],
        ),
    ),
    (
        "1.23 * a / (0 - 3)",
        Tree(  # type: ignore
            "div",
            [
                Tree("mult", [Token("FLOAT", "1.23"), Token("NAME", "a")]),
                Tree("sub", [Token("INT", "0"), Token("INT", "3")]),
            ],
        ),
    ),
    (
        "--3 + a / B + +(2 / 2.1)",
        Tree(  # type: ignore
            "add",
            [
                Tree(
                    "add",
                    [
                        Tree("usub", [Token("INT", "-3")]),
                        Tree("div", [Token("NAME", "a"), Token("NAME", "B")]),
                    ],
                ),
                Tree(
                    "uadd",
                    [
                        Tree("div", [Token("INT", "2"), Token("FLOAT", "2.1")]),
                    ],
                ),
            ],
        ),
    ),
    (
        "Func(a * 2).length <= a[i+1]",
        Tree(  # type: ignore
            "lte",
            [
                Tree(
                    "attribute",
                    [
                        Tree(
                            "call",
                            [
                                Token("NAME", "Func"),
                                Tree(
                                    Token("RULE", "arg_list"),
                                    [
                                        Tree(
                                            "mult",
                                            [Token("NAME", "a"), Token("INT", "2")],
                                        )
                                    ],
                                ),
                            ],
                        ),
                        Token("NAME", "length"),
                    ],
                ),
                Tree(
                    "subscript",
                    [
                        Token("NAME", "a"),
                        Tree(
                            Token("RULE", "subscript_list"),
                            [Tree("add", [Token("NAME", "i"), Token("INT", "1")])],
                        ),
                    ],
                ),
            ],
        ),
    ),
    (
        # This expression will parse as assignment as is, but we can sneak it to be
        # testing for equality when put into a test context
        "lay=@GRIDMODELS[grid_model_no].layers + GetSomething(grid_model_no)",
        Tree(  # type: ignore
            "assign",
            [
                Token("NAME", "lay"),
                Tree(  # type: ignore
                    "add",
                    [
                        Tree(
                            "attribute",
                            [
                                Tree(
                                    "subscript",
                                    [
                                        Token("SYSDEF", "@GRIDMODELS"),
                                        Tree(
                                            Token("RULE", "subscript_list"),
                                            [
                                                Token("NAME", "grid_model_no"),
                                            ],
                                        ),
                                    ],
                                ),
                                Token("NAME", "layers"),
                            ],
                        ),
                        Tree(
                            "call",
                            [
                                Token("NAME", "GetSomething"),
                                Tree(
                                    Token("RULE", "arg_list"),
                                    [Token("NAME", "grid_model_no")],
                                ),
                            ],
                        ),
                    ],
                ),
            ],
        ),
    ),
    (
        'f1(f1("a", "c"), f1("b", f2(d + (4 / 3)))) <> 0',
        Tree(  # type: ignore
            "noteq",
            [
                Tree(
                    "call",
                    [
                        Token("NAME", "f1"),
                        Tree(  # type: ignore
                            Token("RULE", "arg_list"),
                            [
                                Tree(
                                    "call",
                                    [
                                        Token("NAME", "f1"),
                                        Tree(
                                            Token("RULE", "arg_list"),
                                            [
                                                Token("STRING", '"a"'),
                                                Token("STRING", '"c"'),
                                            ],
                                        ),
                                    ],
                                ),
                                Tree(
                                    "call",
                                    [
                                        Token("NAME", "f1"),
                                        Tree(
                                            Token("RULE", "arg_list"),
                                            [
                                                Token("STRING", '"b"'),
                                                Tree(
                                                    "call",
                                                    [
                                                        Token("NAME", "f2"),
                                                        Tree(
                                                            Token("RULE", "arg_list"),
                                                            [
                                                                Tree(
                                                                    "add",
                                                                    [
                                                                        Token(
                                                                            "NAME", "d"
                                                                        ),
                                                                        Tree(
                                                                            "div",
                                                                            [
                                                                                Token(
                                                                                    "INT",  # noqa: E501
                                                                                    "4",
                                                                                ),
                                                                                Token(
                                                                                    "INT",  # noqa: E501
                                                                                    "3",
                                                                                ),
                                                                            ],
                                                                        ),
                                                                    ],
                                                                ),
                                                            ],
                                                        ),
                                                    ],
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
                Token("INT", "0"),
            ],
        ),
    ),
]
