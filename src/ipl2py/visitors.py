from abc import ABC
from dataclasses import fields
from typing import Any, Generator, List, Tuple, Union

import ipl2py.ast as ast


class Visitor(ABC):
    def __init__(self, visit_meta=False) -> None:
        self._visit_meta = visit_meta

    def iter_fields(self, node: ast.Node) -> Generator[Tuple[str, Any], None, None]:
        for field in fields(node):
            if field.name == "meta" and self._visit_meta is False:
                continue
            try:
                yield field.name, getattr(node, field.name)
            except AttributeError:
                pass

    def iter_child_nodes(self, node: ast.Node) -> Generator[ast.Node, None, None]:
        for name, field in self.iter_fields(node):
            if isinstance(field, ast.Node):
                yield field
            elif isinstance(field, list):
                for item in field:
                    if isinstance(item, ast.Node):
                        yield item

    def traverse(self, node: Union[ast.Node, List[ast.Node]]) -> None:
        if isinstance(node, list):
            for item in node:
                self.traverse(item)
        else:
            self.visit(node)

    def visit(self, node: ast.Node):
        node_name = node.__class__.__name__
        visitor = getattr(self, node_name, self.__default__)
        return visitor(node)

    def __default__(self, node: ast.Node):
        for node in self.iter_child_nodes(node):
            self.visit(node)


class PrettyPrintVisitor(Visitor):
    def visit(self, node: ast.Node, level=0) -> str:
        out = [f"{'  '*level}{node.__class__.__name__}"]
        for node in self.iter_child_nodes(node):
            out.append(self.visit(node, level + 1))
        return "\n".join(out)
