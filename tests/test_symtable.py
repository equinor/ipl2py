import pytest

from ipl2py.exceptions import CompilationError, SymbolCollisionError
from ipl2py.ipl import Type
from ipl2py.symtable import FunctionSymbol, ProcedureSymbol, Symbol, SymbolTableType


@pytest.mark.parametrize("decl", ["a", "a[]", "a[,]", "a[,,]"])
def test_single_global_decl(symbol_table, decl):
    table = symbol_table(f"Int {decl}")
    assert table.name == "top"
    assert table.type == SymbolTableType.MODULE
    assert table.has_children is False
    assert table.identifiers == ["a"]
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True)


def test_single_global_decl_assign(symbol_table):
    table = symbol_table("Float a = 1.1")
    assert table.identifiers == ["a"]
    assert table.lookup("a") == Symbol(
        "a", Type.FLOAT, is_global=True, is_assigned=True
    )


def test_single_global_decl_list(symbol_table):
    table = symbol_table("Bool a, b = TRUE, c[], d[,], e[,,]")
    assert table.identifiers == ["a", "b", "c", "d", "e"]
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
    assert table.identifiers == ["a", "i", "j", "k", "x", "y", "z"]
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
    with pytest.raises(SymbolCollisionError):
        symbol_table("Int a, a[]")
    with pytest.raises(SymbolCollisionError):
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
    assert table.identifiers == ["a"]
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
    assert table.identifiers == ["a", "b"]
    assert table.lookup("a") == Symbol(
        "a", Type.INT, is_global=True, is_assigned=True, is_referenced=True
    )
    assert table.lookup("b") == Symbol("b", Type.INT, is_global=True, is_assigned=True)


def test_global_cant_reference_undeclared(symbol_table):
    with pytest.raises(CompilationError):
        symbol_table(
            """
Int a
a = b
            """
        )


def test_global_assign_subscripted_name_marks_assigned(symbol_table):
    table = symbol_table(
        """
Int a[]
a[1] = 1
        """
    )
    assert table.identifiers == ["a"]
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True, is_assigned=True)


def test_global_assign_with_attribute_marks_assigned(symbol_table):
    table = symbol_table(
        """
Int a
a.length = 1
Int b
b.length[a[0,1,2].something].size = 1
        """
    )
    assert table.identifiers == ["a", "b"]
    assert table.lookup("a") == Symbol("a", Type.INT, is_global=True, is_assigned=True)
    assert table.lookup("b") == Symbol("b", Type.INT, is_global=True, is_assigned=True)


def test_global_subscripted_identifier_is_marked_referenced(symbol_table):
    table = symbol_table(
        """
Int a = 1, c = 3
Int b[]
b[a] = 2
b[b[a] + 2 * 3 - 1] = b.length[c]
        """
    )
    assert table.identifiers == ["a", "c", "b"]
    assert table.lookup("a") == Symbol(
        "a", Type.INT, is_global=True, is_assigned=True, is_referenced=True
    )
    assert table.lookup("c") == Symbol(
        "c", Type.INT, is_global=True, is_assigned=True, is_referenced=True
    )
    assert table.lookup("b") == Symbol("b", Type.INT, is_global=True, is_assigned=True)


@pytest.mark.parametrize(
    "test_type,block_start,block_end",
    [("IF", "THEN", "ENDIF"), ("WHILE", "DO", "ENDWHILE")],
)
@pytest.mark.parametrize(
    "case",
    [
        "a",
        "a = TRUE",
        "TRUE = a",
        "(a + 2) < 3 * 2",
        "TRUE AND a",
        "(TRUE OR FALSE) AND NOT (a AND FALSE)",
        "NOT a",
        "-a < 2",
        "2 + b[a] >= 3",
    ],
)
def test_global_references_in_condition_tests(
    symbol_table, test_type, block_start, block_end, case
):
    table = symbol_table(
        f"""
Bool a = TRUE
Bool b = FALSE
{test_type} {case} {block_start}
    a = b
{block_end}
        """
    )
    assert table.identifiers == ["a", "b"]
    assert table.lookup("a") == Symbol(
        "a", Type.BOOL, is_global=True, is_assigned=True, is_referenced=True
    )
    assert table.lookup("b") == Symbol(
        "b", Type.BOOL, is_global=True, is_assigned=True, is_referenced=True
    )


def test_global_references_in_for_loop(symbol_table):
    table = symbol_table(
        """
Int i = 1
Int j
FOR i FROM 1 TO 10 DO
    j = i
    i = i + j
ENDFOR
        """
    )
    assert table.identifiers == ["i", "j"]
    assert table.lookup("i") == Symbol(
        "i", Type.INT, is_global=True, is_assigned=True, is_referenced=True
    )
    assert table.lookup("j") == Symbol(
        "j", Type.INT, is_global=True, is_assigned=True, is_referenced=True
    )


def test_global_undeclared_iterator_in_for_loop(symbol_table):
    with pytest.raises(CompilationError):
        symbol_table(
            """
Int j
FOR i FROM 1 TO 10 DO
    j = i
    i = i + j
ENDFOR
            """
        )


def test_procedure_creates_symtable_child(symbol_table):
    table = symbol_table(
        """
FUNCTION a()
    HALT
ENDFUNCTION
        """
    )
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=None, is_global=True, is_callable=True, is_assigned=True
    )
    assert table.get_child("a") == ProcedureSymbol(name="a", parent=table)


def test_identifier_scoping_with_single_nested_scope(symbol_table):
    """It is valid in IPL to have variables and functions with the same
    identifier."""
    table = symbol_table(
        """
Int a
FUNCTION a()
    Int a = 1
ENDFUNCTION
        """
    )
    assert table.identifiers == ["a"]
    assert table.lookup("a") == Symbol(name="a", type=Type.INT, is_global=True)
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=None, is_global=True, is_callable=True, is_assigned=True
    )
    procedure = table.get_child("a")
    expected = ProcedureSymbol(name="a", parent=table)
    inner_a = Symbol(name="a", type=Type.INT, is_global=False, is_assigned=True)
    expected.add_symbol(inner_a)
    assert procedure == expected
    assert procedure.lookup("a") == inner_a


def test_function_creates_symtable_child(symbol_table):
    table = symbol_table(
        """
Int FUNCTION a()
    HALT
ENDFUNCTION
        """
    )
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=Type.INT, is_global=True, is_callable=True, is_assigned=True
    )
    assert table.get_child("a") == FunctionSymbol(
        name="a", parent=table, return_type=Type.INT
    )


def test_local_lookup_miss_returns_global_identifier(symbol_table):
    table = symbol_table(
        """
Int a
Int FUNCTION a()
    a = 1
ENDFUNCTION
        """
    )
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=Type.INT, is_global=True, is_callable=True, is_assigned=True
    )
    assert table.get_child("a") == FunctionSymbol(
        name="a", parent=table, return_type=Type.INT
    )
    expected = Symbol(
        name="a", type=Type.INT, is_global=True, is_assigned=True, is_free=True
    )
    assert table.get_child("a").lookup("a") == expected
    expected.is_free = False
    assert table.lookup("a") == expected


def test_assigning_to_undeclared_in_nested_scope_fails(symbol_table):
    with pytest.raises(CompilationError):
        symbol_table(
            """
Int FUNCTION a()
    a = 1
ENDFUNCTION
            """
        )


def test_no_param_set_as_local_identifier(symbol_table):
    table = symbol_table(
        """
Int a
FUNCTION a()
    a = 1
ENDFUNCTION
        """
    )
    assert table.get_child("a").params == []


def test_single_param_set_as_local_identifier(symbol_table):
    table = symbol_table(
        """
Int FUNCTION a(Int a)
    HALT
ENDFUNCTION
        """
    )
    assert table.identifiers == []
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=Type.INT, is_global=True, is_callable=True, is_assigned=True
    )
    expected = Symbol(name="a", type=Type.INT, is_global=False, is_parameter=True)
    assert table.get_child("a").lookup("a") == expected


def test_redeclaring_param_identifier_causes_collision(symbol_table):
    with pytest.raises(SymbolCollisionError):
        symbol_table(
            """
Int FUNCTION a(Int a)
    Bool a
ENDFUNCTION
            """
        )


def test_single_param_is_set_assigned(symbol_table):
    table = symbol_table(
        """
Int FUNCTION a(Int a)
    a = 1
ENDFUNCTION
        """
    )
    assert table.identifiers == []
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=Type.INT, is_global=True, is_callable=True, is_assigned=True
    )
    expected = Symbol(
        name="a", type=Type.INT, is_global=False, is_assigned=True, is_parameter=True
    )
    assert table.get_child("a").lookup("a") == expected


def test_multi_params_in_various_states(symbol_table):
    table = symbol_table(
        """
FUNCTION a(Int a, Bool b[], Float c = 3.14, String d = "something")
    Int e
    a = 1
    c = -c
ENDFUNCTION
        """
    )
    assert table.identifiers == []
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a", type=None, is_global=True, is_callable=True, is_assigned=True
    )
    expected_a = Symbol(
        name="a", type=Type.INT, is_global=False, is_assigned=True, is_parameter=True
    )
    assert table.get_child("a").lookup("a") == expected_a
    expected_b = Symbol(
        name="b", type=Type.BOOL, is_global=False, is_assigned=False, is_parameter=True
    )
    assert table.get_child("a").lookup("b") == expected_b
    expected_c = Symbol(
        name="c",
        type=Type.FLOAT,
        is_global=False,
        is_parameter=True,
        is_assigned=True,
        is_referenced=True,
    )
    assert table.get_child("a").lookup("c") == expected_c
    expected_d = Symbol(
        name="d",
        type=Type.STRING,
        is_global=False,
        is_parameter=True,
    )
    assert table.get_child("a").lookup("d") == expected_d
    expected_e = Symbol(
        name="e",
        type=Type.INT,
        is_global=False,
        is_parameter=False,
    )
    assert table.get_child("a").lookup("e") == expected_e


def test_call_procedure_sets_callable_as_referenced(symbol_table):
    table = symbol_table(
        """
FUNCTION a()
    HALT
ENDFUNCTION
a()
        """
    )
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a",
        type=None,
        is_global=True,
        is_callable=True,
        is_assigned=True,
        is_referenced=True,
    )


def test_calling_undeclared_procedure_raises(symbol_table):
    with pytest.raises(CompilationError):
        symbol_table(
            """
FUNCTION a()
    HALT
ENDFUNCTION
b()
            """
        )


def test_identifiers_in_call_arguments_set_as_referenced(symbol_table):
    table = symbol_table(
        """
Int a
Float b
Float FUNCTION a(Int a, Float b, Bool c)
    RETURN(a + b)
ENDFUNCTION
a(a + 1, b, FALSE)
        """
    )
    assert table.identifiers == ["a", "b"]
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a",
        type=Type.FLOAT,
        is_global=True,
        is_callable=True,
        is_assigned=True,
        is_referenced=True,
    )
    outer_a = Symbol(name="a", type=Type.INT, is_global=True, is_referenced=True)
    assert table.lookup("a") == outer_a
    outer_b = Symbol(name="b", type=Type.FLOAT, is_global=True, is_referenced=True)
    assert table.lookup("b") == outer_b

    inner_a = Symbol(name="a", type=Type.INT, is_parameter=True, is_referenced=True)
    assert table.get_child("a").lookup("a") == inner_a
    inner_b = Symbol(name="b", type=Type.FLOAT, is_parameter=True, is_referenced=True)
    assert table.get_child("a").lookup("b") == inner_b
    inner_c = Symbol(name="c", type=Type.BOOL, is_parameter=True)
    assert table.get_child("a").lookup("c") == inner_c


def test_single_identifier_in_return_is_set_as_referenced(symbol_table):
    table = symbol_table(
        """
Float FUNCTION a(Int a)
    RETURN(a)
ENDFUNCTION
a(1)
        """
    )
    assert table.identifiers == []
    assert table.callable_identifiers == ["a"]
    assert table.callable_lookup("a") == Symbol(
        name="a",
        type=Type.FLOAT,
        is_global=True,
        is_callable=True,
        is_assigned=True,
        is_referenced=True,
    )
    inner_a = Symbol(name="a", type=Type.INT, is_parameter=True, is_referenced=True)
    assert table.get_child("a").lookup("a") == inner_a
