from ipl2py import compile


def test_hello_world():
    code = compile(
        """
String a = "Hello"
Print(a, " world!")
        """
    )
    assert code.splitlines()[1:] == ["a = 'Hello'", "print(a, ' world!')"]
