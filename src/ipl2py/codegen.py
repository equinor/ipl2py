from collections import deque
from contextlib import contextmanager, nullcontext
from enum import Enum, auto
from typing import (
    Callable,
    ContextManager,
    Deque,
    Generator,
    List,
    Optional,
    Sequence,
    Union,
)

import ipl2py.ast as ast
import ipl2py.ipl as ipl
from ipl2py.visitors import Visitor


class _Context(Enum):
    BOOLOP = auto()
    BINOP = auto()
    COMPARE = auto()
    MODULE = auto()
    BLOCK = auto()
    TEST = auto()


class CodeGenVisitor(Visitor):
    """This visitor generates the Python code from the AST. The AST
    was modelled roughly (but not 1 to 1) after the Python AST. This
    code similarly copies (often directly) from Python's ``ast.unparse``
    function. The reasons these are copied and not used directly are
    that the AST may have breaking changes between different Python
    versions, and that ``unparse()`` is not available in Python 3.8.
    There are also slight differences because our source language is not
    Python itself, of course.
    """

    def __init__(self) -> None:
        self._indent = 0
        self._context: Deque[_Context] = deque()
        self._context.append(_Context.MODULE)
        self._source: List[str] = []
        self._ensure_type: Optional[ipl.Type] = None

    def delimit_if(self, start: str, end: str, condition: bool) -> ContextManager[None]:
        if condition:
            return self.delimit(start, end)
        return nullcontext()

    def interleave(
        self,
        inter: Callable,
        f: Callable[[ast.Node], None],
        seq: Sequence[ast.Node],
    ):
        """Call f on each item in seq, calling inter() in between."""
        seq_iter = iter(seq)
        try:
            f(next(seq_iter))
        except StopIteration:
            pass
        else:
            for x in seq_iter:
                inter()
                f(x)

    def newline(self) -> None:
        self.write("\n")

    def fill(self, text="") -> None:
        """Indent a piece of text and append it, according to the current
        indentation level"""
        self.newline()
        self.write("    " * self._indent + text)

    @contextmanager
    def block(self, *, extra=None) -> Generator[None, None, None]:
        """A context manager for preparing the source for blocks. It adds
        the character ':', increases the indentation on enter, and decreases
        the indentation on exit. If *extra* is given, it will be directly
        appended after the colon character.
        """
        with self.context(_Context.BLOCK):
            self.write(":")
            if extra:
                self.write(extra)
            self._indent += 1
            yield
            self._indent -= 1

    @contextmanager
    def context(self, ctx: _Context) -> Generator[None, None, None]:
        """A context manager for wrapping a context around operations.
        Useful for knowing about our semantic position within the AST."""
        self._context.append(ctx)
        yield
        self._context.pop()

    @contextmanager
    def delimit(self, start: str, end: str) -> Generator[None, None, None]:
        """A context manager for preparing the source for expressions. It adds
        *start* to the buffer and enters, after exit it adds *end*."""
        self.write(start)
        yield
        self.write(end)

    def write(self, text: str) -> None:
        """Add new source parts"""
        self._source.extend(text)

    def traverse(
        self, node: Union[ast.Node, List[ast.Node], List[ast.Statement]]
    ) -> None:
        if isinstance(node, list):
            for item in node:
                self.traverse(item)
        else:
            super().visit(node)

    def visit(self, node: ast.Node) -> str:
        self.traverse(node)
        return "".join(self._source)

    def Module(self, node: ast.Module) -> None:
        self.traverse(node.body)

    def If(self, node: ast.If) -> None:
        self.fill("if ")
        with self.context(_Context.TEST):
            self.traverse(node.test)
        with self.block():
            self.traverse(node.body)

        # Collapse nested else: ifs into equivalent elifs
        while (
            node.orelse and len(node.orelse) == 1 and isinstance(node.orelse[0], ast.If)
        ):
            node = node.orelse[0]
            self.fill("elif ")
            with self.context(_Context.TEST):
                self.traverse(node.test)
            with self.block():
                self.traverse(node.body)

        if node.orelse:
            self.fill("else")
            with self.block():
                self.traverse(node.orelse)

    def Halt(self, node: ast.Halt) -> None:
        self.fill("exit()")

    def Assign(self, node: ast.Assign) -> None:
        self.fill()
        for target in node.targets:
            self._ensure_type = getattr(target, "type", None)
            self.traverse(target)
            self.write(" = ")

        self.traverse(node.value)
        self._ensure_type = None

    def Call(self, node: ast.Call) -> None:
        if self._context[-1] in [_Context.MODULE, _Context.BLOCK]:
            self.fill()
        self.traverse(node.func)
        with self.delimit("(", ")"):
            comma = False
            for arg in node.args:
                if comma:
                    self.write(", ")
                else:
                    comma = True
                self.traverse(arg)

    def UnaryOp(self, node: ast.UnaryOp) -> None:
        unops = {
            "UAdd": "+",
            "USub": "-",
            "UNot": "not ",
        }
        operator = unops[node.op.__class__.__name__]
        self.write(operator)
        needs_parens = (
            isinstance(node.operand, ast.BinOp)
            or isinstance(node.operand, ast.Compare)
            or isinstance(node.operand, ast.BoolOp)
        )
        with self.delimit_if("(", ")", needs_parens):
            self.traverse(node.operand)

    def BinOp(self, node: ast.BinOp) -> None:
        binops = {
            "Add": "+",
            "Sub": "-",
            "Mult": "*",
            "Div": "/",
        }
        operator = binops[node.op.__class__.__name__]
        with self.delimit_if("(", ")", self._context[-1] == _Context.COMPARE):
            with self.context(_Context.COMPARE):
                self.traverse(node.left)
                self.write(f" {operator} ")
                self.traverse(node.right)

    def Compare(self, node: ast.Compare) -> None:
        cmpops = {
            "Lt": "<",
            "LtE": "<=",
            "Gt": ">",
            "GtE": ">=",
            "Eq": "==",
            "NotEq": "!=",
        }
        with self.delimit_if("(", ")", self._context[-1] == _Context.COMPARE):
            with self.context(_Context.COMPARE):
                self.traverse(node.left)
                self.write(f" {cmpops[node.op.__class__.__name__]} ")
                self.traverse(node.right)

    def BoolOp(self, node: ast.BoolOp) -> None:
        operator = node.op.__class__.__name__.lower()
        with self.delimit_if("(", ")", self._context[-1] == _Context.BOOLOP):
            with self.context(_Context.BOOLOP):
                s = f" {operator} "
                self.interleave(
                    lambda: self.write(s),
                    lambda node: self.traverse(node),
                    node.values,
                )

    def Attribute(self, node: ast.Attribute) -> None:
        self.traverse(node.value)
        self.write(".")
        self.write(node.attr)

    def Subscript(self, node: ast.Subscript) -> None:
        self.traverse(node.value)
        with self.delimit("[", "]"):
            self.traverse(node.index)

    def Name(self, node: ast.Name) -> None:
        self.write(node.id)

    def Constant(self, node: ast.Constant) -> None:
        val = node.value
        # Ensure implicit casting doesn't end up modifying the type
        if self._ensure_type == ipl.Type.FLOAT and isinstance(val, int):
            val = float(val)  # type: ignore
        elif self._ensure_type == ipl.Type.INT and isinstance(val, float):
            val = int(val)  # type: ignore

        self.write(repr(val))


def generate_code(ast: ast.Module) -> str:
    code = CodeGenVisitor().visit(ast)
    return code.strip()
