import pytest

from ipl2py import compile


def test_hello_world():
    code = compile(
        """
String a = "Hello"
Print(a, " world!")
        """
    )
    assert code.splitlines() == ["a = 'Hello'", "print(a, ' world!')"]


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Int a = 1", "a = 1"),
        ("Float a = 1", "a = 1.0"),
        ('String a = "Hello"', "a = 'Hello'"),
        ("Bool a = TRUE", "a = True"),
        (
            """
String a = "Hello"
a[2] = "a"
            """,
            """a = 'Hello'\na[2] = 'a'""",
        ),
        ("Int a = 123\na.length = 1", "a = 123\na.length = 1"),
        ('Print("Hello world!")', "print('Hello world!')"),
    ],
)
def test_basic_statements(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Bool b = TRUE AND FALSE OR TRUE", "b = (True and False) or True"),
        ("Bool b = TRUE OR FALSE OR TRUE", "b = True or False or True"),
        (
            "Bool b = TRUE AND FALSE OR TRUE AND FALSE OR TRUE AND TRUE OR FALSE",
            "b = (True and False) or (True and False) or (True and True) or False",
        ),
    ],
)
def test_boolop_precedence_in_assignment(given, expected):
    assert compile(given) == expected
