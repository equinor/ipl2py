import pytest

import ipl2py.ast as ast
import ipl2py.ipl as ipl


def test_empty_file_creates_empty_module(to_ast):
    tree = to_ast("")
    assert isinstance(tree, ast.Module)
    assert tree.body == []


@pytest.mark.parametrize(
    "type,default",
    [
        (ipl.Type.INT, 0),
        (ipl.Type.FLOAT, 0.0),
        (ipl.Type.BOOL, False),
        (ipl.Type.STRING, ""),
        (ipl.Type.FACIES_MODEL, None),
    ],
)
def test_global_single_decl_statement_assigns_to_default(to_ast, type, default):
    tree = to_ast(f"{type.value} a")
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 1
    assign = tree.body[0]
    assert isinstance(assign, ast.Assign)
    assert len(assign.targets) == 1
    assert assign.targets[0] == ast.Name(id="a", type=type)
    assert assign.value == ast.Constant(value=default)


def test_global_single_assign_statement(to_ast):
    tree = to_ast("Int a = 1")
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 1
    assign = tree.body[0]
    assert isinstance(assign, ast.Assign)
    assert len(assign.targets) == 1
    assert assign.targets[0] == ast.Name(id="a", type=ipl.Type.INT)
    assert assign.value == ast.Constant(value=1)


def test_global_multiple_inline_assign_statements(to_ast):
    tree = to_ast("Int a = 1, b[], c[,], d[,,], e")
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 5
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[0].value == ast.Constant(value=1)

    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.INT)]
    assert isinstance(tree.body[1].value, ast.Array1D)

    assert tree.body[2].targets == [ast.Name(id="c", type=ipl.Type.INT)]
    assert isinstance(tree.body[2].value, ast.Array2D)

    assert tree.body[3].targets == [ast.Name(id="d", type=ipl.Type.INT)]
    assert isinstance(tree.body[3].value, ast.Array3D)

    assert tree.body[4].targets == [ast.Name(id="e", type=ipl.Type.INT)]
    assert tree.body[4].value == ast.Constant(value=0)


def test_global_multi_single_assign_statements(to_ast):
    tree = to_ast(
        """
Int a = 1
Bool b
Float c = 3.14
        """
    )
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 3
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[0].value == ast.Constant(value=1)

    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.BOOL)]
    assert tree.body[1].value == ast.Constant(value=False)

    assert tree.body[2].targets == [ast.Name(id="c", type=ipl.Type.FLOAT)]
    assert tree.body[2].value == ast.Constant(value=3.14)


def test_global_multi_multi_assign_statements(to_ast):
    tree = to_ast(
        """
Int a = 1, b[]
Bool c, d = TRUE, e = FALSE
Float f = 3.14, g[,,], h = -1.1
        """
    )
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 8
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[0].value == ast.Constant(value=1)
    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.INT)]
    assert isinstance(tree.body[1].value, ast.Array1D)

    assert tree.body[2].targets == [ast.Name(id="c", type=ipl.Type.BOOL)]
    assert tree.body[2].value == ast.Constant(value=False)
    assert tree.body[3].targets == [ast.Name(id="d", type=ipl.Type.BOOL)]
    assert tree.body[3].value == ast.Constant(value=True)
    assert tree.body[4].targets == [ast.Name(id="e", type=ipl.Type.BOOL)]
    assert tree.body[4].value == ast.Constant(value=False)

    assert tree.body[5].targets == [ast.Name(id="f", type=ipl.Type.FLOAT)]
    assert tree.body[5].value == ast.Constant(value=3.14)
    assert tree.body[6].targets == [ast.Name(id="g", type=ipl.Type.FLOAT)]
    assert isinstance(tree.body[6].value, ast.Array3D)
    assert tree.body[7].targets == [ast.Name(id="h", type=ipl.Type.FLOAT)]
    assert tree.body[7].value == ast.Constant(value=-1.1)


def test_global_assign(to_ast):
    tree = to_ast(
        """
Int a
a = 1
a = 2
        """
    )
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 3
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[0].value == ast.Constant(value=0)
    assert tree.body[1].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[1].value == ast.Constant(value=1)
    assert tree.body[2].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[2].value == ast.Constant(value=2)


@pytest.mark.parametrize("constant", [c for c in ipl.Constant])
def test_global_assign_with_ipl_constant(to_ast, constant):
    tree = to_ast(
        f"""
Int a = {constant.value}
        """
    )
    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert tree.body[0].value == ast.Constant(value=constant)


@pytest.mark.parametrize("constant", [c for c in ipl.Constant])
def test_global_redfined_constant_identifier_takes_precedence(to_ast, constant):
    tree = to_ast(
        f"""
Int {constant.value} = 123
Int b = {constant.value}
        """
    )
    assert tree.body[0].targets == [ast.Name(id=constant.value, type=ipl.Type.INT)]
    assert tree.body[0].value == ast.Constant(value=123)
    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.INT)]
    assert tree.body[1].value == ast.Name(id=constant.value, type=ipl.Type.INT)


def test_assign_attribute_to_identifier(to_ast):
    tree = to_ast(
        """
Int a[], length
length = a.length
        """
    )
    assert len(tree.body) == 3
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert isinstance(tree.body[0].value, ast.Array1D)
    assert tree.body[1].targets == [ast.Name(id="length", type=ipl.Type.INT)]
    assert tree.body[1].value == ast.Constant(value=0)

    assert tree.body[2].targets == [ast.Name(id="length", type=ipl.Type.INT)]
    attribute = tree.body[2].value
    assert isinstance(attribute, ast.Attribute)
    assert attribute.value == ast.Name(id="a", type=ipl.Type.INT)
    assert attribute.attr == "length"


def test_global_assignment_to_subscript(to_ast):
    tree = to_ast(
        """
Int a[]
a[1] = 2
a[a[1]] = 3
        """
    )
    assert len(tree.body) == 3
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert isinstance(tree.body[0].value, ast.Array1D)

    subscript = tree.body[1].targets[0]
    assert tree.body[1].value == ast.Constant(value=2)
    assert isinstance(subscript, ast.Subscript)
    assert subscript.value == ast.Name(id="a", type=ipl.Type.INT)
    index = subscript.index
    assert isinstance(index, ast.Index1D)
    assert index.i == ast.Constant(value=1)

    outer_subscript = tree.body[2].targets[0]
    assert tree.body[2].value == ast.Constant(value=3)
    assert isinstance(outer_subscript, ast.Subscript)
    assert outer_subscript.value == ast.Name(id="a", type=ipl.Type.INT)
    outer_index = outer_subscript.index
    assert isinstance(index, ast.Index1D)

    inner_subscript = outer_index.i
    assert isinstance(inner_subscript, ast.Subscript)
    assert inner_subscript.value == ast.Name(id="a", type=ipl.Type.INT)
    inner_index = inner_subscript.index
    assert isinstance(index, ast.Index1D)
    assert inner_index.i == ast.Constant(value=1)


@pytest.mark.parametrize(
    "decl,index,idx_type,type",
    [
        ("", "1", ast.Index1D, ast.Array1D),
        (",", "1,2", ast.Index2D, ast.Array2D),
        (",,", "1,2,3", ast.Index3D, ast.Array3D),
    ],
)
def test_global_assignment_to_all_subscript_types(to_ast, decl, index, idx_type, type):
    tree = to_ast(
        f"""
Int a[{decl}]
a[{index}] = 2
        """
    )
    assert len(tree.body) == 2
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert isinstance(tree.body[0].value, type)

    subscript = tree.body[1].targets[0]
    assert tree.body[1].value == ast.Constant(value=2)
    assert isinstance(subscript, ast.Subscript)
    assert subscript.value == ast.Name(id="a", type=ipl.Type.INT)
    index = subscript.index
    assert isinstance(index, idx_type)
    assert index.i == ast.Constant(value=1)
    if idx_type is not ast.Index1D:
        assert index.j == ast.Constant(value=2)
    if idx_type is ast.Index3D:
        assert index.k == ast.Constant(value=3)


def test_global_subscript_on_lhs_and_rgs(to_ast):
    tree = to_ast(
        """
Int a[]
Int b[]
a[1] = a[b[2].length]
        """
    )
    assert len(tree.body) == 3
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    lhs_subscript = tree.body[2].targets[0]
    assert isinstance(lhs_subscript, ast.Subscript)
    assert lhs_subscript.value == ast.Name(id="a", type=ipl.Type.INT)
    lhs_index = lhs_subscript.index
    assert isinstance(lhs_index, ast.Index1D)
    assert lhs_index.i == ast.Constant(value=1)

    rhs_subscript = tree.body[2].value
    assert isinstance(rhs_subscript, ast.Subscript)
    assert rhs_subscript.value == ast.Name(id="a", type=ipl.Type.INT)

    outer_index = rhs_subscript.index
    assert isinstance(outer_index, ast.Index1D)

    inner_attr = outer_index.i
    assert isinstance(inner_attr, ast.Attribute)
    assert inner_attr.attr == "length"

    inner_subscript = inner_attr.value
    assert isinstance(inner_subscript, ast.Subscript)
    assert inner_subscript.value == ast.Name(id="b", type=ipl.Type.INT)
    inner_index = inner_subscript.index
    assert isinstance(inner_index, ast.Index1D)
    assert inner_index.i == ast.Constant(value=2)


@pytest.mark.parametrize(
    "op,op_str",
    [
        (ast.Add(), "+"),
        (ast.Sub(), "-"),
        (ast.Mult(), "*"),
        (ast.Div(), "/"),
    ],
)
def test_global_simple_binops(to_ast, op, op_str):
    tree = to_ast(f"Int a = 1 {op_str} 2")
    binop = tree.body[0].value
    assert isinstance(binop, ast.BinOp)
    assert binop.lhs == ast.Constant(value=1)
    assert binop.op == op
    assert binop.rhs == ast.Constant(value=2)


def test_global_compound_binop_precedence(to_ast):
    # Should resolve to (1 - 2) + ((3 * 4) / 5)
    tree = to_ast("Int a = 1 - 2 + 3 * 4 / 5")
    root_binop = tree.body[0].value
    assert isinstance(root_binop, ast.BinOp)
    assert root_binop.op == ast.Add()

    # (1 - 2)
    lhs_binop = root_binop.lhs
    assert lhs_binop.lhs == ast.Constant(value=1)
    assert lhs_binop.op == ast.Sub()
    assert lhs_binop.rhs == ast.Constant(value=2)

    # ((3 * 4) / 5)
    rhs_binop = root_binop.rhs
    assert rhs_binop.op == ast.Div()
    assert rhs_binop.rhs == ast.Constant(value=5)

    # (3 * 4)
    inner_binop = rhs_binop.lhs
    assert inner_binop.lhs == ast.Constant(value=3)
    assert inner_binop.op == ast.Mult()
    assert inner_binop.rhs == ast.Constant(value=4)


def test_global_compound_binop_precedence_with_parantheses(to_ast):
    tree = to_ast("Int a = 1 - ((2 + 3) * 4) / 5")
    root_binop = tree.body[0].value
    assert isinstance(root_binop, ast.BinOp)
    assert root_binop.lhs == ast.Constant(value=1)
    assert root_binop.op == ast.Sub()

    # ((2 + 3) * 4) / 5
    rhs_binop = root_binop.rhs
    assert rhs_binop.rhs == ast.Constant(value=5)
    assert rhs_binop.op == ast.Div()

    # (2 + 3) * 4
    inner_lhs_binop = rhs_binop.lhs
    assert inner_lhs_binop.op == ast.Mult()
    assert inner_lhs_binop.rhs == ast.Constant(value=4)

    # 2 + 3
    inner_binop = inner_lhs_binop.lhs
    assert inner_binop.lhs == ast.Constant(value=2)
    assert inner_binop.op == ast.Add()
    assert inner_binop.rhs == ast.Constant(value=3)
