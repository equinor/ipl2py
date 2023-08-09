import pytest

import ipl2py.ast as ast
import ipl2py.ipl as ipl

from .asserts import assert_ast_constant_assign


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
    assert_ast_constant_assign(tree.body[0], "a", type, default)


def test_global_single_assign_statement(to_ast):
    tree = to_ast("Int a = 1")
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 1
    assign = tree.body[0]
    assert isinstance(assign, ast.Assign)
    assert len(assign.targets) == 1
    assert_ast_constant_assign(tree.body[0], "a", ipl.Type.INT, 1)


def test_global_multiple_inline_assign_statements(to_ast):
    tree = to_ast("Int a = 1, b[], c[,], d[,,], e")
    assert isinstance(tree, ast.Module)
    assert len(tree.body) == 5
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert_ast_constant_assign(tree.body[0], "a", ipl.Type.INT, 1)

    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.INT)]
    assert isinstance(tree.body[1].value, ast.Array1D)

    assert tree.body[2].targets == [ast.Name(id="c", type=ipl.Type.INT)]
    assert isinstance(tree.body[2].value, ast.Array2D)

    assert tree.body[3].targets == [ast.Name(id="d", type=ipl.Type.INT)]
    assert isinstance(tree.body[3].value, ast.Array3D)

    assert_ast_constant_assign(tree.body[4], "e", ipl.Type.INT, 0)


def test_global_multi_single_assign_statements(to_ast):
    tree = to_ast(
        """
Int a = 1
Bool b
Float c = 3.14
        """
    )
    assert_ast_constant_assign(tree.body[0], "a", ipl.Type.INT, 1)
    assert_ast_constant_assign(tree.body[1], "b", ipl.Type.BOOL, False)
    assert_ast_constant_assign(tree.body[2], "c", ipl.Type.FLOAT, 3.14)


def test_global_point_assignment(to_ast):
    tree = to_ast(
        """
Int a, b
Point c = (a + b, a)
        """
    )
    a = tree.body[0].targets[0]
    b = tree.body[1].targets[0]
    stmt = tree.body[2]
    assert isinstance(stmt, ast.Assign)
    assert stmt.targets[0] == ast.Name(id="c", type=ipl.Type.POINT)
    point = stmt.value
    assert isinstance(point, ast.Point)
    assert point.x.left == a
    assert point.x.right == b
    assert point.y == a
    assert point.z == ast.Constant(value=0)


def test_global_multi_multi_assign_statements(to_ast):
    tree = to_ast(
        """
Int a = 1, b[]
Bool c, d = TRUE, e = FALSE
Float f = 3.14, g[,,], h = -1.1
        """
    )
    assert len(tree.body) == 8
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert_ast_constant_assign(tree.body[0], "a", ipl.Type.INT, 1)
    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.INT)]
    assert isinstance(tree.body[1].value, ast.Array1D)

    assert_ast_constant_assign(tree.body[2], "c", ipl.Type.BOOL, False)
    assert_ast_constant_assign(tree.body[3], "d", ipl.Type.BOOL, True)
    assert_ast_constant_assign(tree.body[4], "e", ipl.Type.BOOL, False)

    assert_ast_constant_assign(tree.body[5], "f", ipl.Type.FLOAT, 3.14)
    assert tree.body[6].targets == [ast.Name(id="g", type=ipl.Type.FLOAT)]
    assert isinstance(tree.body[6].value, ast.Array3D)
    assert_ast_constant_assign(tree.body[7], "h", ipl.Type.FLOAT, -1.1)


def test_global_assign(to_ast):
    tree = to_ast(
        """
Int a
a = 1
a = 2
        """
    )
    assert len(tree.body) == 3
    for stmt in tree.body:
        assert isinstance(stmt, ast.Assign)

    assert_ast_constant_assign(tree.body[0], "a", ipl.Type.INT, 0)
    assert_ast_constant_assign(tree.body[1], "a", ipl.Type.INT, 1)
    assert_ast_constant_assign(tree.body[2], "a", ipl.Type.INT, 2)


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
    assert_ast_constant_assign(tree.body[0], constant.value, ipl.Type.INT, 123)
    assert tree.body[1].targets == [ast.Name(id="b", type=ipl.Type.INT)]
    assert tree.body[1].value == ast.Name(id=constant.value, type=ipl.Type.INT)


def test_assign_attribute_to_identifier(to_ast):
    tree = to_ast(
        """
Int a[], length
length = a.length
        """
    )
    assert tree.body[0].targets == [ast.Name(id="a", type=ipl.Type.INT)]
    assert isinstance(tree.body[0].value, ast.Array1D)
    assert_ast_constant_assign(tree.body[1], "length", ipl.Type.INT, 0)

    assert tree.body[2].targets == [ast.Name(id="length", type=ipl.Type.INT)]
    attribute = tree.body[2].value
    assert isinstance(attribute, ast.Attribute)
    assert attribute.value == ast.Name(id="a", type=None)
    assert attribute.attr == "length"


def test_global_assignment_to_subscript(to_ast):
    tree = to_ast(
        """
Int a[]
a[1] = 2
a[a[1]] = 3
        """
    )
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
    assert binop.left == ast.Constant(value=1)
    assert binop.op == op
    assert binop.right == ast.Constant(value=2)


def test_global_compound_binop_precedence(to_ast):
    # Should resolve to (1 - 2) + ((3 * 4) / 5)
    tree = to_ast("Int a = 1 - 2 + 3 * 4 / 5")
    root_binop = tree.body[0].value
    assert isinstance(root_binop, ast.BinOp)
    assert root_binop.op == ast.Add()

    # (1 - 2)
    lhs_binop = root_binop.left
    assert lhs_binop.left == ast.Constant(value=1)
    assert lhs_binop.op == ast.Sub()
    assert lhs_binop.right == ast.Constant(value=2)

    # ((3 * 4) / 5)
    rhs_binop = root_binop.right
    assert rhs_binop.op == ast.Div()
    assert rhs_binop.right == ast.Constant(value=5)

    # (3 * 4)
    inner_binop = rhs_binop.left
    assert inner_binop.left == ast.Constant(value=3)
    assert inner_binop.op == ast.Mult()
    assert inner_binop.right == ast.Constant(value=4)


def test_global_compound_binop_precedence_with_parantheses(to_ast):
    tree = to_ast("Int a = 1 - ((2 + 3) * 4) / 5")
    root_binop = tree.body[0].value
    assert isinstance(root_binop, ast.BinOp)
    assert root_binop.left == ast.Constant(value=1)
    assert root_binop.op == ast.Sub()

    # ((2 + 3) * 4) / 5
    rhs_binop = root_binop.right
    assert rhs_binop.right == ast.Constant(value=5)
    assert rhs_binop.op == ast.Div()

    # (2 + 3) * 4
    inner_lhs_binop = rhs_binop.left
    assert inner_lhs_binop.op == ast.Mult()
    assert inner_lhs_binop.right == ast.Constant(value=4)

    # 2 + 3
    inner_binop = inner_lhs_binop.left
    assert inner_binop.left == ast.Constant(value=2)
    assert inner_binop.op == ast.Add()
    assert inner_binop.right == ast.Constant(value=3)


@pytest.mark.parametrize(
    "op,op_class",
    [
        ("+", ast.UAdd()),
        ("-", ast.USub()),
        ("NOT ", ast.UNot()),
    ],
)
def test_global_single_unaryop(to_ast, op, op_class):
    tree = to_ast(
        f"""
Int a = 1
Int b = {op}a
        """
    )
    unaryop = tree.body[1].value
    assert isinstance(unaryop, ast.UnaryOp)
    assert unaryop.op == op_class
    assert unaryop.operand == ast.Name(id="a", type=ipl.Type.INT)


@pytest.mark.parametrize(
    "op,op_class",
    [
        ("+", ast.UAdd()),
        ("-", ast.USub()),
        ("NOT ", ast.UNot()),
    ],
)
def test_global_chained_unaryop(to_ast, op, op_class):
    tree = to_ast(
        f"""
Int a = 1
Int b = {op}{op}{op}a
        """
    )
    unaryop = tree.body[1].value
    assert isinstance(unaryop, ast.UnaryOp)
    assert unaryop.op == op_class

    unaryop = unaryop.operand
    assert isinstance(unaryop, ast.UnaryOp)
    assert unaryop.op == op_class

    unaryop = unaryop.operand
    assert isinstance(unaryop, ast.UnaryOp)
    assert unaryop.op == op_class
    assert unaryop.operand == ast.Name(id="a", type=ipl.Type.INT)


@pytest.mark.parametrize(
    "op,op_class",
    [
        ("<", ast.Lt()),
        ("<=", ast.LtE()),
        (">", ast.Gt()),
        (">=", ast.GtE()),
        ("=", ast.Eq()),
        ("<>", ast.NotEq()),
    ],
)
def test_global_single_comparison(to_ast, op, op_class):
    tree = to_ast(
        f"""
Bool a = 1 {op} 2
        """
    )
    compare = tree.body[0].value
    assert isinstance(compare, ast.Compare)
    assert compare.left == ast.Constant(value=1)
    assert compare.op == op_class
    assert compare.right == ast.Constant(value=2)


@pytest.mark.parametrize(
    "op,op_class",
    [
        ("<", ast.Lt()),
        ("<=", ast.LtE()),
        (">", ast.Gt()),
        (">=", ast.GtE()),
        ("=", ast.Eq()),
        ("<>", ast.NotEq()),
    ],
)
def test_global_chained_compare(to_ast, op, op_class):
    tree = to_ast(
        f"""
Int a = 1 {op} 2 {op} 3
        """
    )
    compare = tree.body[0].value
    assert isinstance(compare, ast.Compare)
    assert compare.op == op_class
    assert compare.right == ast.Constant(value=3)

    compare = compare.left
    assert isinstance(compare, ast.Compare)
    assert compare.left == ast.Constant(value=1)
    assert compare.op == op_class
    assert compare.right == ast.Constant(value=2)


def test_global_halt(to_ast):
    tree = to_ast("HALT")
    assert isinstance(tree.body[0], ast.Halt)


def test_global_simple_if(to_ast):
    tree = to_ast(
        """
IF 1 THEN
    HALT
ENDIF
        """
    )
    if_ = tree.body[0]
    assert isinstance(if_, ast.If)
    assert if_.test == ast.Constant(value=1)
    assert len(if_.body) == 1 and isinstance(if_.body[0], ast.Halt)
    assert if_.orelse is None


def test_global_simple_if_else(to_ast):
    tree = to_ast(
        """
IF 1 THEN
    HALT
ELSE
    HALT
ENDIF
        """
    )
    if_ = tree.body[0]
    assert isinstance(if_, ast.If)
    assert if_.test == ast.Constant(value=1)
    assert len(if_.body) == 1 and isinstance(if_.body[0], ast.Halt)
    assert len(if_.orelse) == 1 and isinstance(if_.orelse[0], ast.Halt)


@pytest.mark.parametrize(
    "test_str,boolop,left,op,right",
    [
        (
            "TRUE AND TRUE",
            ast.BoolOp,
            ast.Constant(value=True),
            ast.And(),
            ast.Constant(value=True),
        ),
        (
            "1 OR 2",
            ast.BoolOp,
            ast.Constant(value=1),
            ast.Or(),
            ast.Constant(value=2),
        ),
        (
            "1 <= 2",
            ast.Compare,
            ast.Constant(value=1),
            ast.LtE(),
            ast.Constant(value=2),
        ),
        (
            "TRUE = FALSE",
            ast.Compare,
            ast.Constant(value=True),
            ast.Eq(),
            ast.Constant(value=False),
        ),
        (
            '"hi" <> "bye"',
            ast.Compare,
            ast.Constant(value="hi"),
            ast.NotEq(),
            ast.Constant(value="bye"),
        ),
        (
            "1 + 2",
            ast.BinOp,
            ast.Constant(value=1),
            ast.Add(),
            ast.Constant(value=2),
        ),
    ],
)
def test_global_various_binary_if_tests(to_ast, test_str, boolop, left, op, right):
    tree = to_ast(
        f"""
IF {test_str} THEN
    HALT
ENDIF
        """
    )
    if_ = tree.body[0]
    assert isinstance(if_, ast.If)
    test = if_.test
    assert isinstance(test, boolop)
    assert test.left == left
    assert test.op == op
    assert test.right == right
    assert len(if_.body) == 1 and isinstance(if_.body[0], ast.Halt)
    assert if_.orelse is None


def test_global_compound_if_test_precendence(to_ast):
    # Should resolve to (T & F) | T
    tree = to_ast(
        """
IF TRUE AND FALSE OR TRUE THEN
    HALT
ENDIF
        """
    )
    if_ = tree.body[0]
    assert isinstance(if_, ast.If)
    test = if_.test
    assert isinstance(test, ast.BoolOp)
    left = test.left
    assert test.right == ast.Constant(value=True)
    assert test.op == ast.Or()

    assert isinstance(left, ast.BoolOp)
    assert left.left == ast.Constant(value=True)
    assert left.op == ast.And()
    assert left.right == ast.Constant(value=False)
    assert len(if_.body) == 1 and isinstance(if_.body[0], ast.Halt)
    assert if_.orelse is None


def test_global_a_semi_complex_compound_if_test(to_ast):
    tree = to_ast(
        """
Int a, b, c, d
IF (a > 2 AND a < 4) AND (b = c OR d <> 0) THEN
    HALT
ENDIF
        """
    )
    if_ = tree.body[4]
    assert isinstance(if_, ast.If)
    test = if_.test
    assert isinstance(test, ast.BoolOp)
    assert test.op == ast.And()

    left = test.left
    assert isinstance(left, ast.BoolOp)
    assert left.op == ast.And()
    assert left.left.left == ast.Name(id="a", type=ipl.Type.INT)
    assert left.left.op == ast.Gt()
    assert left.left.right == ast.Constant(value=2)
    assert left.right.left == ast.Name(id="a", type=ipl.Type.INT)
    assert left.right.op == ast.Lt()
    assert left.right.right == ast.Constant(value=4)

    right = test.right
    assert isinstance(right, ast.BoolOp)
    assert right.op == ast.Or()
    assert right.left.left == ast.Name(id="b", type=ipl.Type.INT)
    assert right.left.op == ast.Eq()
    assert right.left.right == ast.Name(id="c", type=ipl.Type.INT)
    assert right.right.left == ast.Name(id="d", type=ipl.Type.INT)
    assert right.right.op == ast.NotEq()
    assert right.right.right == ast.Constant(value=0)

    assert len(if_.body) == 1 and isinstance(if_.body[0], ast.Halt)
    assert if_.orelse is None


def test_global_while(to_ast):
    tree = to_ast(
        """
Bool b = TRUE
Int a
WHILE b DO
    HALT
    HALT
ENDWHILE
        """
    )
    while_ = tree.body[2]
    assert isinstance(while_, ast.While)
    assert while_.test == ast.Name(id="b", type=ipl.Type.BOOL)
    body = while_.body
    assert isinstance(body[0], ast.Halt)
    assert isinstance(body[1], ast.Halt)


@pytest.mark.parametrize("direction", ["TO", "DOWNTO"])
def test_global_for(to_ast, direction):
    tree = to_ast(
        f"""
Int a
FOR a FROM 1 {direction} 10 DO
    HALT
ENDFOR
        """
    )
    for_ = tree.body[1]
    assert isinstance(for_, ast.For)
    assert for_.target == tree.body[0].targets[0]
    assert for_.start == ast.Constant(value=1)
    assert for_.end == ast.Constant(value=10)
    assert isinstance(for_.body[0], ast.Halt)


def test_global_nested_for(to_ast):
    tree = to_ast(
        """
Int i, j, k
FOR i FROM 1 TO 10 DO
    FOR j FROM 10 DOWNTO 1 DO
        FOR k FROM 1 TO 2 DO
            HALT
        ENDFOR
    ENDFOR
ENDFOR
        """
    )
    for_i = tree.body[3]
    assert isinstance(for_i, ast.For)
    assert for_i.target == tree.body[0].targets[0]
    assert for_i.start == ast.Constant(value=1)
    assert for_i.end == ast.Constant(value=10)
    for_j = for_i.body[0]
    assert isinstance(for_j, ast.For)
    assert for_j.target == tree.body[1].targets[0]
    assert for_j.start == ast.Constant(value=10)
    assert for_j.end == ast.Constant(value=1)
    for_k = for_j.body[0]
    assert isinstance(for_k, ast.For)
    assert for_k.target == tree.body[2].targets[0]
    assert for_k.start == ast.Constant(value=1)
    assert for_k.end == ast.Constant(value=2)


def test_simple_procedure(to_ast):
    tree = to_ast(
        """
FUNCTION a()
    HALT
ENDFUNCTION
        """
    )
    fn = tree.body[0]
    assert isinstance(fn, ast.Function)
    assert fn.name.id == "a"
    assert fn.name.type is None
    assert fn.params == []
    assert len(fn.body) == 1
    assert isinstance(fn.body[0], ast.Halt)


def test_simple_function(to_ast):
    tree = to_ast(
        """
Int FUNCTION a()
    RETURN(1)
ENDFUNCTION
        """
    )
    fn = tree.body[0]
    assert isinstance(fn, ast.Function)
    assert fn.name.id == "a"
    assert fn.name.type is ipl.Type.INT
    assert fn.params == []
    assert len(fn.body) == 1
    assert isinstance(fn.body[0], ast.Return)
    assert fn.body[0].value == ast.Constant(value=1)


def test_simple_procedure_with_params(to_ast):
    tree = to_ast(
        """
FUNCTION a(Int a, Bool b)
    HALT
ENDFUNCTION
        """
    )
    fn = tree.body[0]
    assert isinstance(fn, ast.Function)
    assert fn.name.id == "a"
    assert fn.name.type is None
    assert fn.params == [
        ast.Param(id="a", type=ipl.Type.INT),
        ast.Param(id="b", type=ipl.Type.BOOL),
    ]
    assert len(fn.body) == 1
    assert isinstance(fn.body[0], ast.Halt)


def test_simple_function_with_params(to_ast):
    tree = to_ast(
        """
Float FUNCTION a(Int a, Int b)
    RETURN(a + b)
ENDFUNCTION
        """
    )
    fn = tree.body[0]
    assert isinstance(fn, ast.Function)
    assert fn.name.id == "a"
    assert fn.name.type is ipl.Type.FLOAT
    assert fn.params == [
        ast.Param(id="a", type=ipl.Type.INT),
        ast.Param(id="b", type=ipl.Type.INT),
    ]
    assert len(fn.body) == 1
    assert isinstance(fn.body[0], ast.Return)
    binop = fn.body[0].value
    assert isinstance(binop, ast.BinOp)
    assert binop.left == ast.Name(id="a", type=ipl.Type.INT)
    assert binop.op == ast.Add()
    assert binop.right == ast.Name(id="b", type=ipl.Type.INT)


def test_simple_function_with_params_and_locals(to_ast):
    tree = to_ast(
        """
Bool FUNCTION a(Int a, Int b)
    Int c = 1
    Int d = 2
    b = c + d
    RETURN(a < b)
ENDFUNCTION
        """
    )
    fn = tree.body[0]
    assert isinstance(fn, ast.Function)
    assert fn.name.id == "a"
    assert fn.name.type is ipl.Type.BOOL

    assert fn.params == [
        ast.Param(id="a", type=ipl.Type.INT),
        ast.Param(id="b", type=ipl.Type.INT),
    ]

    assert len(fn.body) == 4
    # Int c = 1
    assert_ast_constant_assign(fn.body[0], "c", ipl.Type.INT, 1)
    # Int d = 2
    assert_ast_constant_assign(fn.body[1], "d", ipl.Type.INT, 2)
    # b = c + d
    assert fn.body[2].targets[0].id == "b"
    assert fn.body[2].targets[0].type == ipl.Type.INT
    b_val = fn.body[2].value
    assert isinstance(b_val, ast.BinOp)
    assert b_val.left.id == "c"
    assert b_val.op == ast.Add()
    assert b_val.right.id == "d"

    # RETURN(a < b)
    assert isinstance(fn.body[3], ast.Return)
    compare = fn.body[3].value
    assert isinstance(compare, ast.Compare)
    assert compare.left == ast.Name(id="a", type=ipl.Type.INT)
    assert compare.op == ast.Lt()
    assert compare.right == ast.Name(id="b", type=ipl.Type.INT)


def test_simple_procedure_with_scoped_symbol_collisions(to_ast):
    tree = to_ast(
        """
Int a
Int FUNCTION a(Int a)
    RETURN(a)
ENDFUNCTION
        """
    )
    assert_ast_constant_assign(tree.body[0], "a", ipl.Type.INT, 0)
    fn = tree.body[1]
    assert isinstance(fn, ast.Function)
    assert fn.name.id == "a"
    assert fn.name.type is ipl.Type.INT

    assert fn.params == [ast.Param(id="a", type=ipl.Type.INT)]
    assert isinstance(fn.body[0].value, ast.Name)
    assert fn.body[0].value.id == "a"
    assert fn.body[0].value.type == ipl.Type.INT


def test_simple_global_call(to_ast):
    tree = to_ast(
        """
FUNCTION f()
    HALT
ENDFUNCTION
f()
        """
    )
    assert isinstance(tree.body[0], ast.Function)
    call = tree.body[1]
    assert isinstance(call, ast.Call)
    assert call.func == tree.body[0].name
    assert call.args == []


def test_simple_global_call_with_constant_argument(to_ast):
    tree = to_ast(
        """
FUNCTION f(Int a)
    HALT
ENDFUNCTION
f(1)
        """
    )
    assert isinstance(tree.body[0], ast.Function)
    call = tree.body[1]
    assert isinstance(call, ast.Call)
    assert call.func == tree.body[0].name
    assert call.args == [ast.Constant(value=1)]


def test_simple_global_call_with_multiple_argument_types(to_ast):
    tree = to_ast(
        """
Int a, b
Bool c
FUNCTION f(Int c, Int d, Bool e)
    HALT
ENDFUNCTION
f(a, b, c)
        """
    )
    assert isinstance(tree.body[3], ast.Function)
    call = tree.body[4]
    assert isinstance(call, ast.Call)
    assert call.func == tree.body[3].name
    assert call.args == [
        ast.Name(id="a", type=ipl.Type.INT),
        ast.Name(id="b", type=ipl.Type.INT),
        ast.Name(id="c", type=ipl.Type.BOOL),
    ]


def test_simple_global_call_with_multiple_expressions_as_arguments(to_ast):
    tree = to_ast(
        """
Int a, b
FUNCTION f(Int c, Bool d, Float e)
    HALT
ENDFUNCTION
f((a + 1) * b, b = a, 1 / 2)
        """
    )
    assert isinstance(tree.body[2], ast.Function)
    call = tree.body[3]
    assert isinstance(call, ast.Call)
    assert call.func == tree.body[2].name

    arg_1 = call.args[0]
    assert isinstance(arg_1, ast.BinOp)
    assert arg_1.op == ast.Mult()
    assert arg_1.right == ast.Name(id="b", type=ipl.Type.INT)
    assert isinstance(arg_1.left, ast.BinOp)
    assert arg_1.left.left == ast.Name(id="a", type=ipl.Type.INT)
    assert arg_1.left.right == ast.Constant(value=1)

    arg_2 = call.args[1]
    assert isinstance(arg_2, ast.Compare)
    assert arg_2.left == ast.Name(id="b", type=ipl.Type.INT)
    assert arg_2.op == ast.Eq()
    assert arg_2.right == ast.Name(id="a", type=ipl.Type.INT)

    arg_3 = call.args[2]
    assert isinstance(arg_3, ast.BinOp)
    assert arg_3.left == ast.Constant(value=1)
    assert arg_3.op == ast.Div()
    assert arg_3.right == ast.Constant(value=2)


def test_simple_recursive_and_nested_call(to_ast):
    tree = to_ast(
        """
FUNCTION f()
    f()
ENDFUNCTION
f(f())
        """
    )
    recursive_call = tree.body[0].body[0]
    assert isinstance(recursive_call, ast.Call)
    assert recursive_call.func == ast.Name(id="f", type=None)
    assert recursive_call.args == []

    call = tree.body[1]
    assert isinstance(call, ast.Call)
    assert call.func == tree.body[0].name
    inner_call = call.args[0]
    assert isinstance(inner_call, ast.Call)
    assert inner_call.func == tree.body[0].name
