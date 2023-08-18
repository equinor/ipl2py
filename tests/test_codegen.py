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
            """
a = 'Hello'
a[2 - 1] = 'a'
            """.strip(),
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


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Bool b = 1 = 2", "b = 1 == 2"),
        ("Bool b = 1 <> 2", "b = 1 != 2"),
        ("Bool b = 1 < 2", "b = 1 < 2"),
        ("Bool b = 1 <= 2", "b = 1 <= 2"),
        ("Bool b = 1 > 2", "b = 1 > 2"),
        ("Bool b = 1 >= 2", "b = 1 >= 2"),
        ("Bool b = 1 >= 2 = 1 < 2", "b = ((1 >= 2) == 1) < 2"),
    ],
)
def test_comparison_also_with_parantheses(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Bool b = 1 = 2 AND 2 < 3", "b = 1 == 2 and 2 < 3"),
        ("Bool b = 1 = 2 AND 2 < 3 OR 3 >= 3", "b = (1 == 2 and 2 < 3) or 3 >= 3"),
        ("Bool b = 1 AND 1 <> 1 AND 2", "b = 1 and 1 != 1 and 2"),
    ],
)
def test_boolop_and_cmpop_together(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Int a = 1 + 2", "a = 1 + 2"),
        ("Int a = 1 - 2", "a = 1 - 2"),
        ("Int a = 1 * 2", "a = 1 * 2"),
        ("Int a = 1 / 2", "a = 1 / 2"),
        ("Int a = 1 + 2 - 3", "a = (1 + 2) - 3"),
        ("Int a = 1 + (2 - 3)", "a = 1 + (2 - 3)"),
        ("Int a = 1 + 2 - 3 * 4 / 5", "a = (1 + 2) - ((3 * 4) / 5)"),
    ],
)
def test_binops_and_their_precedence(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Bool a = 1 + 2 < 3 + 0", "a = (1 + 2) < (3 + 0)"),
        (
            "Bool a = 1 + 2 < 3 + 0 OR 5 * 2 <> 3",
            "a = (1 + 2) < (3 + 0) or (5 * 2) != 3",
        ),
        (
            "Bool a = TRUE AND 1 + 1 > 100 * 0 / 1",
            "a = True and (1 + 1) > ((100 * 0) / 1)",
        ),
    ],
)
def test_operators_and_their_precedence(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Int a = --1", "a = --1"),
        ("Int a = +-1", "a = +-1"),
        ("Int a = -(1 - 2)", "a = -(1 - 2)"),
        ("Int a = +(1 - -2)", "a = +(1 - -2)"),
        ("Int a = --2 + 1", "a = --2 + 1"),
        ("Bool a = NOT TRUE", "a = not True"),
        ("Bool a = NOT (1 = 1)", "a = not (1 == 1)"),
    ],
)
def test_unary_operators_with_precedence_and_parens(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        (
            """
IF TRUE THEN
    HALT
ENDIF
            """,
            """
if True:
    exit()
            """.strip(),
        ),
        (
            """
IF TRUE THEN
    HALT
ELSE
    Print(FALSE)
ENDIF
            """,
            """
if True:
    exit()
else:
    print(False)
            """.strip(),
        ),
        (
            """
IF TRUE THEN
    HALT
ELSE
    IF FALSE THEN
        Print(FALSE)
    ENDIF
ENDIF
            """,
            """
if True:
    exit()
elif False:
    print(False)
            """.strip(),
        ),
        (
            """
IF TRUE THEN
    IF TRUE THEN
        HALT
    ELSE
        Print(FALSE)
    ENDIF
    Print(TRUE)
ELSE
    Print(TRUE)
    IF FALSE THEN
        Print(FALSE)
    ENDIF
ENDIF
            """,
            """
if True:
    if True:
        exit()
    else:
        print(False)
    print(True)
else:
    print(True)
    if False:
        print(False)
            """.strip(),
        ),
    ],
)
def test_simple_if_else_statements(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "test,expected",
    [
        (
            "1 = 1",
            """
if 1 == 1:
    exit()
            """.strip(),
        ),
        (
            "1 >= 1 OR 3 > 4 AND 1 + 1 = 2",
            """
if 1 >= 1 or (3 > 4 and (1 + 1) == 2):
    exit()
            """.strip(),
        ),
        (
            'Print(1) = TRUE AND Print(2) > "2"',
            """
if print(1) == True and print(2) > '2':
    exit()
            """.strip(),
        ),
    ],
)
def test_if_else_statements_with_various_tests(test, expected):
    code = f"""
IF {test} THEN
    HALT
ENDIF
    """
    assert compile(code) == expected
    elif_code = f"""
IF {test} THEN
    HALT
ELSE
    IF {test} THEN
        HALT
    ENDIF
ENDIF
    """
    assert (
        compile(elif_code)
        == f"""
{expected}
el{expected}
        """.strip()
    )


@pytest.mark.parametrize(
    "given,expected",
    [
        (
            """
WHILE TRUE DO
    Print("GOTO 10")
ENDWHILE
            """,
            """
while True:
    print('GOTO 10')
            """.strip(),
        ),
        (
            """
WHILE TRUE DO
    WHILE 1 = 1 AND 2 = 2 OR TRUE DO
        HALT
    ENDWHILE
    Print("GOTO 10")
ENDWHILE
            """,
            """
while True:
    while (1 == 1 and 2 == 2) or True:
        exit()
    print('GOTO 10')
            """.strip(),
        ),
        (
            """
Int a
IF a = 0 THEN
    WHILE a < 10 DO
        Print(a)
        a = a + 1
    ENDWHILE
ENDIF
            """,
            """
a = 0
if a == 0:
    while a < 10:
        print(a)
        a = a + 1
            """.strip(),
        ),
    ],
)
def test_simple_while_statement(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        (
            """
Int i
FOR i FROM 1 TO 10 DO
    Print(i)
ENDFOR
            """,
            """
i = 0
for i in range(1, 10 + 1):
    print(i)
            """.strip(),
        ),
        (
            """
Int i
FOR i FROM 1 TO 10 DO
    IF i = 2 THEN
        Print(i)
    ENDIF
ENDFOR
            """,
            """
i = 0
for i in range(1, 10 + 1):
    if i == 2:
        print(i)
            """.strip(),
        ),
        (
            """
Int i
FOR i FROM 10 DOWNTO 1 DO
    Print(i)
ENDFOR
            """,
            """
i = 0
for i in range(10, 1 - 1, -1):
    print(i)
            """.strip(),
        ),
        (
            """
Int i, a, b = 10
FOR i FROM a TO b DO
    Print(i)
ENDFOR
            """,
            """
i = 0
a = 0
b = 10
for i in range(a, b + 1):
    print(i)
            """.strip(),
        ),
        (
            """
Int i, a, b = 10
FOR i FROM a * 2 TO b + a - 1 DO
    Print(i)
ENDFOR
            """,
            """
i = 0
a = 0
b = 10
for i in range(a * 2, (b + a) - 1 + 1):
    print(i)
            """.strip(),
        ),
    ],
)
def test_simple_for_statement(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        (
            """
FUNCTION f()
    Print(1)
ENDFUNCTION
            """,
            """
def f():
    print(1)
            """.strip(),
        ),
        (
            """
FUNCTION f(Int a)
    Print(a)
ENDFUNCTION
            """,
            """
def f(a):
    print(a)
            """.strip(),
        ),
        (
            """
FUNCTION f(Int a, Float b, Bool c)
    Print(a, b, c)
ENDFUNCTION
            """,
            """
def f(a, b, c):
    print(a, b, c)
            """.strip(),
        ),
        (
            """
Int FUNCTION f(Int a, Float b, Bool c)
    RETURN(f(a, b, c))
ENDFUNCTION
            """,
            """
def f(a, b, c):
    return f(a, b, c)
            """.strip(),
        ),
        (
            """
Int FUNCTION mult(Int a, Int b)
    IF a = 0 OR b = 0 THEN
        RETURN(0)
    ELSE
        RETURN(a * b)
    ENDIF
ENDFUNCTION
            """,
            """
def mult(a, b):
    if a == 0 or b == 0:
        return 0
    else:
        return a * b
            """.strip(),
        ),
        (
            """
FUNCTION hello()
    Print("Hello")
ENDFUNCTION

FUNCTION world()
    Print("world!")
ENDFUNCTION

hello()
Print(" ")
world()
            """,
            """
def hello():
    print('Hello')


def world():
    print('world!')

hello()
print(' ')
world()
            """.strip(),
        ),
    ],
)
def test_simple_function_statements(given, expected):
    assert compile(given) == expected


@pytest.mark.parametrize(
    "given,expected",
    [
        ("Point a", "a = (0.0, 0.0, 0.0)"),
        ("Point a = (1, 2)", "a = (1.0, 2.0, 0.0)"),
        ("Point a = (1, 2, 3)", "a = (1.0, 2.0, 3.0)"),
        ("Point a\na = (1, 2, 3)", "a = (0.0, 0.0, 0.0)\na = (1.0, 2.0, 3.0)"),
        ("Int b\nPoint a = (1, a + 2, 3)", "b = 0\na = (1.0, a + 2.0, 3.0)"),
    ],
)
def test_simple_point_creation(given, expected):
    assert compile(given) == expected
