import logging
from typing import List, Mapping, Tuple, Union

import lark
from lark import Token, Tree
from lark.visitors import Visitor_Recursive

from .exceptions import CompilationError
from .ipl import Type
from .symtable import (
    FunctionSymbolTable,
    ProcedureSymbolTable,
    Symbol,
    SymbolTable,
    SymbolTableNode,
)

logger = logging.getLogger(__name__)


class CommentVisitor(Visitor_Recursive):
    """This visitor recursively visits each branch and looks for comments that
    should be placed within it. It stores these in the node's `meta` attribute,
    in custom attributes (directly below). The purpose of this is to preserve
    comments from the original source in nearly the same place after
    transformation.

    This leads to some redundancy. For example, a declaration statement will
    have the same comment added to several tree levels (e.g. `decl_stmt` and
    `decl`). IPL allows arbitrary new lines so this should be handled either
    in a transformation or during code generation.

    IF-ELSE statements (as distinct from IF statements) contain two suites and
    this complicates determining where a code block ends, so it is handled a
    bit separately.
    """

    HEADER_COMMENTS = "header_comments"
    INLINE_COMMENTS = "inline_comments"
    FOOTER_COMMENTS = "footer_comments"
    INVALID = -1

    def __init__(self, comments: List[Token]):
        super().__init__()
        self._mapped_comments = self._map_line_to_comment(comments)
        self._prev_end_line = 0

        self._compound_end_line = 0
        self._is_if_else = False

    def _add_comments_attrs(self, node: Tree):
        setattr(node.meta, "header_comments", [])
        setattr(node.meta, "inline_comments", [])
        setattr(node.meta, "footer_comments", [])

    def _map_line_to_comment(self, comments: List[Token]) -> Mapping[int, Token]:
        """Map each comment to a line number in the original source"""

        def _by_line_number(comment: Token) -> int:
            # The line attribute is technically optional but should
            # always be there as positions are propagated if comments
            # are preserved.
            line = getattr(comment, "line", 0)
            if line == 0:
                logger.warning(
                    "Encountered comment without a line number. This comment might "
                    "be lost: %s",
                    comment,
                )
            return line

        comments.sort(key=_by_line_number)
        # Note that 0 is not a valid line number so it won't be placed into a node.
        mapping: Mapping[int, Token] = {c.line if c.line else 0: c for c in comments}
        logger.debug("Mapped %d comments: %s", len(mapping), mapping)
        return mapping

    def _get_comments(self, from_line: int, to_line: int) -> List[Tuple[int, Token]]:
        comments = [
            (key, val)
            for key, val in self._mapped_comments.items()
            if from_line <= (val.line if val.line else 0) < to_line
        ]
        logger.debug("Comments from line %d to %d: %s", from_line, to_line, comments)
        return comments

    def _get_node_bounds(self, node: Union[Tree, lark.Tree, Token]) -> Tuple[int, int]:
        """Gets the start and end line of a tree in the originl source."""
        if isinstance(node, Tree):
            try:
                line = node.meta.line
                end_line = node.meta.end_line
            except AttributeError:
                assert not node.children
        else:
            # Tokens have optional line numbers as well.
            line = getattr(node, "line", self.INVALID)
            end_line = getattr(node, "end_line", self.INVALID)
        return line, end_line

    def _assign_comments_to(
        self, node: Tree, attr: str, line: int, end_line: int
    ) -> None:
        comments = self._get_comments(line, end_line)
        logger.debug(
            "Assign comments: node=%s, attr=%s, line=%d, end_line=%d",
            node,
            attr,
            line,
            end_line,
        )
        setattr(node.meta, attr, comments)

    def _set_prev_end_line(self, end_line: int) -> None:
        self._prev_end_line = end_line + 1

    def start(self, node: Tree) -> None:
        self._add_comments_attrs(node)

        last_comment_end_line = 1
        if len(self._mapped_comments) > 0:
            last_comment_end_line = list(self._mapped_comments)[-1]

        # Without children the meta values aren't set, even with
        # `propagate_positions`, but there could still be comments.
        # Same comment-only edge case.
        if not node.children:
            setattr(node.meta, "line", 1)
            setattr(node.meta, "end_line", last_comment_end_line)
            self._assign_comments_to(
                node,
                self.HEADER_COMMENTS,
                self._prev_end_line,
                last_comment_end_line + 1,
            )
            return

        line, end_line = self._get_node_bounds(node)
        if last_comment_end_line > end_line:
            end_line = last_comment_end_line

        self._assign_comments_to(node, self.HEADER_COMMENTS, self._prev_end_line, line)
        # The last child could be a token so we need some special handling.
        # Note that tokens do not have a meta attribute so it will be grouped
        # into the start footer.
        _, last_child_end_line = self._get_node_bounds(node.children[-1])
        if last_child_end_line == self.INVALID:
            logger.warning(
                "May have missed some comments at the end of the file. "
                "Please report this as a bug."
            )
            last_child_end_line = end_line

        self._assign_comments_to(
            node, self.FOOTER_COMMENTS, last_child_end_line + 1, end_line + 1
        )
        self._set_prev_end_line(end_line)

    def suite(self, node: Tree) -> None:
        self._add_comments_attrs(node)

        line, suite_end_line = self._get_node_bounds(node)
        end_line = self._compound_end_line
        _, last_child_end_line = self._get_node_bounds(node.children[-1])

        if self._is_if_else:
            # IF-ELSE has two suites so we bound this (the first IF suite) to
            # its natural termination rather than the if_stmt's end_line.
            end_line = suite_end_line - 1
            self._is_if_else = False

        self._assign_comments_to(node, self.HEADER_COMMENTS, self._prev_end_line, line)
        self._assign_comments_to(
            node, self.FOOTER_COMMENTS, last_child_end_line + 1, end_line + 1
        )
        self._set_prev_end_line(end_line)

    def __default__(self, node: Tree) -> None:
        self._add_comments_attrs(node)

        line, end_line = self._get_node_bounds(node)
        self._assign_comments_to(node, self.HEADER_COMMENTS, self._prev_end_line, line)

        # Node is on a single line so it's in an inline comment
        if line == end_line:
            self._assign_comments_to(node, self.INLINE_COMMENTS, line, line + 1)
            self._set_prev_end_line(line)
            return

        if node.data in (
            "if_stmt",
            "while_stmt",
            "for_stmt",
            "proc_def",
            "func_def",
        ):
            self._compound_end_line = end_line
            # Flag that we have two suites to work with
            if node.data == "if_stmt" and node.children[-1] is not None:
                self._is_if_else = True

            self._assign_comments_to(node, self.INLINE_COMMENTS, line, line + 1)
            end_line = line

        self._set_prev_end_line(end_line)


ScopeStack = List[SymbolTableNode]


class SymbolTableVisitor(Visitor_Recursive):
    """This visitor generates a symbol table from the top down. This is done
    before the first-pass AST is constructed because IPL requires top declarations
    before any statements can operate on them. Converting that directly to an AST
    would mean setting empty defaults for these variables. Generating the symbol
    table first should allow us to generate a more Pythonic AST. The intent, with
    hope, is to not need secondary passes on the AST after it is generated.

    We are fortunate in that scoping is fairly flat in IPL: we only need to worry
    about globals and procedures/functions, which cannot nest.

    This visitor overrides the default `visit_topdown` function provided in its
    derived class. This is done to also call an exit function upon leaving some
    node to help keep track of scoping.
    """

    decl_types = ("decl", "decl_1d", "decl_2d", "decl_3d", "decl_assign")

    def __init__(self, root: SymbolTable) -> None:
        super().__init__()
        self._root = root
        self._scope_stack: ScopeStack = [root]

    def __default_exit__(self, tree: Tree) -> Tree:
        return tree

    def _call_exit_userfunc(self, tree: Tree) -> Tree:
        return getattr(self, f"{tree.data}_exit", self.__default_exit__)(tree)

    def visit_topdown(self, tree: Tree) -> Tree:
        """Overrides the default visit_topdown visitor function to add an additional
        exit call after all child trees have been visited."""
        self._call_userfunc(tree)  # Inherited from derived class
        for child in tree.children:
            if isinstance(child, Tree):
                self.visit_topdown(child)
        self._call_exit_userfunc(tree)
        return tree

    def get_global(self) -> SymbolTable:
        return self._root

    def get_scope(self) -> SymbolTableNode:
        assert len(self._scope_stack) > 0
        return self._scope_stack[-1]

    def push_scope(self, node: SymbolTableNode) -> ScopeStack:
        self._scope_stack.append(node)
        return self._scope_stack

    def pop_scope(self) -> Union[None, SymbolTableNode]:
        if len(self._scope_stack) == 0:
            return None
        node = self._scope_stack[-1]
        self._scope_stack = self._scope_stack[:-1]
        return node

    def _get_lhs_name(self, node: Tree) -> Token:
        """Recursively find the name of the variable being subscripted or
        having its attribute accessed. If given a tree representing something
        like `a.length[b[0,1,2].something].size = 1` this will return
        `Token("NAME", "a")`"""
        if isinstance(node.children[0], Token):
            return node.children[0]
        return self._get_lhs_name(node.children[0])  # type: ignore

    def _create_symbol(
        self, token: Token, type_: Type, is_assigned=False, is_parameter=False
    ) -> None:
        assert token.type == "NAME"
        symbol = Symbol(
            token.value,
            type_,
            is_assigned=is_assigned,
            is_parameter=is_parameter,
        )
        self.get_scope().insert_symbol(symbol)

    def _update_referenced_identifiers(self, node: Tree) -> None:
        # At this point all variables should be declared so we can
        # raise exceptions for undeclared identifiers.
        for child in node.children:
            if not (isinstance(child, Token) and child.type == "NAME"):
                continue
            name = child.value
            symbol = self.get_scope().lookup(name)
            if not symbol:
                raise CompilationError(f"Reference to undeclared identifier {name}")
            symbol.is_referenced = True

    def func_def(self, node: Tree) -> None:
        type = node.children[0].value  # type: ignore
        identifier = node.children[1].value  # type: ignore
        root_table = self.get_global()
        function = FunctionSymbolTable(identifier, root_table, type)
        root_table.insert_child(function)
        self.push_scope(function)

    def func_def_exit(self, node: Tree) -> None:
        self.pop_scope()

    def proc_def(self, node: Tree) -> None:
        identifier = node.children[0].value  # type: ignore
        root_table = self.get_global()
        procedure = ProcedureSymbolTable(identifier, root_table)
        root_table.insert_child(procedure)
        self.push_scope(procedure)

    def proc_def_exit(self, node: Tree) -> None:
        self.pop_scope()

    def param(self, node: Tree) -> None:
        type = node.children[0].value  # type: ignore
        self._handle_decl(node.children[1], type, is_parameter=True)

    def _handle_decl(self, node: Tree, type_: Type, is_parameter=False) -> None:
        if node.data == "decl_assign":
            # IPL allows assignment syntax in function parameter declarations
            # but does not assign them. An unlikely edge being case with
            # is_assigned
            self._create_symbol(
                node.children[0],  # type: ignore
                type_,
                is_assigned=False if is_parameter else True,
                is_parameter=is_parameter,
            )
            return
        self._create_symbol(
            node.children[0], type_, is_parameter=is_parameter  # type: ignore
        )

    def _decl_list(self, nodes: List[Tree], type_) -> None:
        for node in nodes:
            if node.data in self.decl_types:
                self._handle_decl(node, type_)  # type: ignore
                continue
            raise ValueError(
                f"decl_list contained child with `data` attribute {node.data}"
            )

    def decl_stmt(self, node: Tree) -> None:
        type_ = node.children[0].value  # type: ignore

        # node.children may contain `decl_types` or decl_list,
        # but the type is child to the decl_stmt only.
        for child in node.children[1:]:
            if child.data in self.decl_types:
                self._handle_decl(child, type_)  # type: ignore
            elif child.data == "decl_list":
                self._decl_list(child.children, type_)  # type: ignore
            else:
                raise ValueError(
                    f"decl_stmt contained child with `data` attribute {child.data}"
                )

    def assign(self, node: Tree) -> None:
        lhs = node.children[0]
        rhs = node.children[1]

        if isinstance(lhs, Token) and lhs.type == "NAME":
            lhs_name = lhs.value
        elif lhs.data in ("subscript", "attribute"):
            lhs_name = self._get_lhs_name(lhs).value  # type: ignore

        lhs_symbol = self.get_scope().lookup(lhs_name)
        if not lhs_symbol:
            raise CompilationError(f"Assignment to undeclared identifier {lhs_name}")
        lhs_symbol.is_assigned = True

        # A single assignment e.g. a = b would mark a reference
        # of b. This won't be caught elsewhere.
        if isinstance(rhs, Token) and rhs.type == "NAME":
            rhs_symbol = self.get_scope().lookup(rhs.value)
            if not rhs_symbol:
                raise CompilationError(
                    f"Reference of undeclared identifier {rhs.value}"
                )
            rhs_symbol.is_referenced = True

    def call(self, node: Tree) -> None:
        name = node.children[0].value  # type: ignore
        called_symbol = self.get_global().callable_lookup(name)
        if not called_symbol:
            raise CompilationError(f"Called undeclared callable {name}")
        called_symbol.is_referenced = True

    # Prefer to allow list rather than use __default__
    def while_stmt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def if_stmt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def return_stmt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def and_test(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def or_test(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def arg_list(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def subscript_list(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def lt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def lte(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def gt(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def gte(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def eq(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def noteq(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def add(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def sub(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def mult(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def div(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def uadd(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def usub(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)

    def unot(self, node: Tree) -> None:
        self._update_referenced_identifiers(node)
