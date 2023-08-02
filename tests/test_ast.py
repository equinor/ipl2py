import pytest

from ipl2py.ast import Array1D, Array2D, Array3D, Assign, Constant, Module, Name
from ipl2py.ipl import Type


def test_empty_file_creates_empty_module(to_ast):
    ast = to_ast("")
    assert isinstance(ast, Module)
    assert ast.body == []


@pytest.mark.parametrize(
    "type,default",
    [
        (Type.INT, 0),
        (Type.FLOAT, 0.0),
        (Type.BOOL, False),
        (Type.STRING, ""),
        (Type.FACIES_MODEL, None),
    ],
)
def test_global_single_decl_statement_assigns_to_default(to_ast, type, default):
    ast = to_ast(f"{type.value} a")
    assert isinstance(ast, Module)
    assert len(ast.body) == 1
    assign = ast.body[0]
    assert isinstance(assign, Assign)
    assert len(assign.targets) == 1
    assert assign.targets[0] == Name(id="a", type=type)
    assert assign.value == Constant(value=default)


def test_global_single_assign_statement(to_ast):
    ast = to_ast("Int a = 1")
    assert isinstance(ast, Module)
    assert len(ast.body) == 1
    assign = ast.body[0]
    assert isinstance(assign, Assign)
    assert len(assign.targets) == 1
    assert assign.targets[0] == Name(id="a", type=Type.INT)
    assert assign.value == Constant(value=1)


def test_global_multiple_inline_assign_statements(to_ast):
    ast = to_ast("Int a = 1, b[], c[,], d[,,], e")
    assert isinstance(ast, Module)
    assert len(ast.body) == 5
    for stmt in ast.body:
        assert isinstance(stmt, Assign)

    assert ast.body[0].targets == [Name(id="a", type=Type.INT)]
    assert ast.body[0].value == Constant(value=1)

    assert ast.body[1].targets == [Name(id="b", type=Type.INT)]
    assert isinstance(ast.body[1].value, Array1D)

    assert ast.body[2].targets == [Name(id="c", type=Type.INT)]
    assert isinstance(ast.body[2].value, Array2D)

    assert ast.body[3].targets == [Name(id="d", type=Type.INT)]
    assert isinstance(ast.body[3].value, Array3D)

    assert ast.body[4].targets == [Name(id="e", type=Type.INT)]
    assert ast.body[4].value == Constant(value=0)


def test_global_multi_single_assign_statements(to_ast):
    ast = to_ast(
        """
Int a = 1
Bool b
Float c = 3.14
        """
    )
    assert isinstance(ast, Module)
    assert len(ast.body) == 3
    for stmt in ast.body:
        assert isinstance(stmt, Assign)

    assert ast.body[0].targets == [Name(id="a", type=Type.INT)]
    assert ast.body[0].value == Constant(value=1)

    assert ast.body[1].targets == [Name(id="b", type=Type.BOOL)]
    assert ast.body[1].value == Constant(value=False)

    assert ast.body[2].targets == [Name(id="c", type=Type.FLOAT)]
    assert ast.body[2].value == Constant(value=3.14)


def test_global_multi_muilti_assign_statements(to_ast):
    ast = to_ast(
        """
Int a = 1, b[]
Bool c, d = TRUE, e = FALSE
Float f = 3.14, g[,,], h = -1.1
        """
    )
    assert isinstance(ast, Module)
    assert len(ast.body) == 8
    for stmt in ast.body:
        assert isinstance(stmt, Assign)

    assert ast.body[0].targets == [Name(id="a", type=Type.INT)]
    assert ast.body[0].value == Constant(value=1)
    assert ast.body[1].targets == [Name(id="b", type=Type.INT)]
    assert isinstance(ast.body[1].value, Array1D)

    assert ast.body[2].targets == [Name(id="c", type=Type.BOOL)]
    assert ast.body[2].value == Constant(value=False)
    assert ast.body[3].targets == [Name(id="d", type=Type.BOOL)]
    assert ast.body[3].value == Constant(value=True)
    assert ast.body[4].targets == [Name(id="e", type=Type.BOOL)]
    assert ast.body[4].value == Constant(value=False)

    assert ast.body[5].targets == [Name(id="f", type=Type.FLOAT)]
    assert ast.body[5].value == Constant(value=3.14)
    assert ast.body[6].targets == [Name(id="g", type=Type.FLOAT)]
    assert isinstance(ast.body[6].value, Array3D)
    assert ast.body[7].targets == [Name(id="h", type=Type.FLOAT)]
    assert ast.body[7].value == Constant(value=-1.1)
