from contextlib import contextmanager
from typing import Generator, List, Union

import ipl2py.ast as ast
from ipl2py.visitors import Visitor


class CodeGenVisitor(Visitor):
    def __init__(self) -> None:
        self._indent = 0
        self._source: List[str] = []

    def newline(self) -> None:
        self.write("\n")

    def fill(self, text="") -> None:
        """Indent a piece of text and append it, according to the current
        indentation level"""
        self.newline()
        self.write("    " * self._indent + text)

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

    def traverse(self, node: Union[ast.Node, List[ast.Node]]) -> None:
        if isinstance(node, list):
            for item in node:
                self.traverse(item)
        else:
            super().visit(node)

    def visit(self, node: ast.Node) -> str:
        self.traverse(node)
        return "".join(self._source)

    def Module(self, node) -> None:
        self.traverse(node.body)

    def Assign(self, node) -> None:
        self.fill()
        for target in node.targets:
            self.traverse(target)
            self.write(" = ")
        self.traverse(node.value)

    def Call(self, node) -> None:
        self.newline()
        self.traverse(node.func)
        with self.delimit("(", ")"):
            comma = False
            for arg in node.args:
                if comma:
                    self.write(", ")
                else:
                    comma = True
                self.traverse(arg)

    def Name(self, node) -> None:
        self.write(node.id)

    def Constant(self, node) -> None:
        self.write(repr(node.value))
