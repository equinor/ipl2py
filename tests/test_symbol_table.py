import pytest

from ipl2py.exceptions import CompilationError, SymbolCollision
from ipl2py.ipl import Type
from ipl2py.symbols import Symbol, SymbolTableType


@pytest.mark.parametrize("decl", ["a", "a[]", "a[,]", "a[,,]"])
def test_single_global_decl(symbol_table, decl):
    table = symbol_table(f"Int {decl}")
    assert table.name == "top"
    assert table.type == SymbolTableType.MODULE
    assert table.has_children is False
    assert table.identifiers == ("a",)
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True)


def test_single_global_decl_assign(symbol_table):
    table = symbol_table("Float a = 1.1")
    assert table.identifiers == ("a",)
    assert table.lookup("a") == Symbol(
        "a", Type.FLOAT, is_global=True, is_assigned=True
    )


def test_single_global_decl_list(symbol_table):
    table = symbol_table("Bool a, b = TRUE, c[], d[,], e[,,]")
    assert table.identifiers == ("a", "b", "c", "d", "e")
    assert table.lookup("a") == Symbol("a", Type.BOOL, is_global=True)
    assert table.lookup("b") == Symbol("b", Type.BOOL, is_global=True, is_assigned=True)
    assert table.lookup("c") == Symbol("c", Type.BOOL, is_global=True)
    assert table.lookup("d") == Symbol("d", Type.BOOL, is_global=True)
    assert table.lookup("e") == Symbol("e", Type.BOOL, is_global=True)


def test_multi_global_decls(symbol_table):
    content = """
Bool a = FALSE
Int i,j,k
Float x = 0.9, y, z = -0.2
        """
    table = symbol_table(content)
    assert table.identifiers == ("a", "i", "j", "k", "x", "y", "z")
    assert table.lookup("a") == Symbol("a", Type.BOOL, is_global=True, is_assigned=True)
    assert table.lookup("i") == Symbol("i", Type.INT, is_global=True)
    assert table.lookup("j") == Symbol("j", Type.INT, is_global=True)
    assert table.lookup("k") == Symbol("k", Type.INT, is_global=True)
    assert table.lookup("x") == Symbol(
        "x", Type.FLOAT, is_global=True, is_assigned=True
    )
    assert table.lookup("y") == Symbol("y", Type.FLOAT, is_global=True)
    assert table.lookup("z") == Symbol(
        "z", Type.FLOAT, is_global=True, is_assigned=True
    )


def test_global_symbol_collision(symbol_table):
    with pytest.raises(SymbolCollision):
        symbol_table("Int a, a[]")
    with pytest.raises(SymbolCollision):
        symbol_table(
            """
Int a
Float a, b, d
            """
        )


def test_global_assign_to_unassigned(symbol_table):
    table = symbol_table(
        """
Int a
a = 2
        """
    )
    assert table.identifiers == ("a",)
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True, is_assigned=True)


def test_global_cant_assign_to_undeclared(symbol_table):
    with pytest.raises(CompilationError):
        symbol_table("a = 2")


def test_global_assign_symbol_causes_reference(symbol_table):
    table = symbol_table(
        """
Int a = 1
Int b
b = a
        """
    )
    assert table.identifiers == ("a", "b")
    assert table.lookup("a") == Symbol(
        "a", Type.INT, is_global=True, is_assigned=True, is_referenced=True
    )
    assert table.lookup("b") == Symbol("b", Type.INT, is_global=True, is_assigned=True)


def test_global_assign_subscripted_name_marks_assigned(symbol_table):
    table = symbol_table(
        """
Int a[]
a[1] = 1
        """
    )
    assert table.identifiers == ("a",)
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True, is_assigned=True)


def test_global_assign_with_attribute_marks_assigned(symbol_table):
    table = symbol_table(
        """
Int a
a.length = 1
Int b
b.length[x[0,1,2].something].size = 1
        """
    )
    assert table.identifiers == ("a", "b")
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True, is_assigned=True)
    assert table.lookup("b") == Symbol("b", Type.INT, is_global=True, is_assigned=True)
