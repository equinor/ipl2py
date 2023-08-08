import logging
from typing import Callable, Dict, List, Mapping, Tuple, Union

from lark import Lark, Token, Tree
from lark.exceptions import ParseError, UnexpectedInput, UnexpectedToken
from lark.visitors import Visitor_Recursive

from ipl2py.grammar import GRAMMAR
from ipl2py.ipl import SysDef, Type

logger = logging.getLogger(__name__)

LexerCallback = Callable[[Token], Union[Token, None]]


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

    def __init__(self, comments: List[Token]) -> None:
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
            # always be there in our usage as we use the Lark
            # propgate_positions option.
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
        return mapping

    def _get_comments(self, from_line: int, to_line: int) -> List[Tuple[int, Token]]:
        comments = [
            (key, val)
            for key, val in self._mapped_comments.items()
            if from_line <= (val.line if val.line else 0) < to_line
        ]
        return comments

    def _get_node_bounds(self, node: Union[Tree, Token]) -> Tuple[int, int]:
        """Gets the start and end line of a tree in the originl source."""
        if isinstance(node, Tree):
            try:
                line = node.meta.line
                end_line = node.meta.end_line
            except AttributeError:
                assert not node.children
        else:
            line = getattr(node, "line", self.INVALID)
            end_line = getattr(node, "end_line", self.INVALID)
        return line, end_line

    def _assign_comments_to(
        self, node: Tree, attr: str, line: int, end_line: int
    ) -> None:
        comments = self._get_comments(line, end_line)
        setattr(node.meta, attr, comments)

    def _set_prev_end_line(self, end_line: int) -> None:
        self._prev_end_line = end_line + 1

    def start(self, node: Tree) -> None:
        self._add_comments_attrs(node)

        last_comment_end_line = 1
        if len(self._mapped_comments) > 0:
            last_comment_end_line = list(self._mapped_comments)[-1]

        # This only occurs when a source file is empty.
        # Without children the meta values aren't set, even with
        # `propagate_positions`, but there could still be comments.
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
        # into the module footer.
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

        # Node is on a single line so it's an inline comment
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
            # Flag that we have two suites to work with, as distinct from
            # just an IF statement.
            if node.data == "if_stmt" and node.children[-1] is not None:
                self._is_if_else = True

            self._assign_comments_to(node, self.INLINE_COMMENTS, line, line + 1)
            end_line = line

        self._set_prev_end_line(end_line)


def parse(content: str, include_comments=True, cache=False) -> Tree:
    """Parse IPL code into a parse tree.

    :param content: The IPL source file as a string. True by default.
    :param include_comments: Include comments in the tree.
    :param cache: Cache the parser generated by Lark. False by default.
    """
    lexer_callbacks: Dict[str, LexerCallback] = {
        "BOOL": lambda token: token.update(
            value=False if token.value == "FALSE" else True
        ),
        "FLOAT": lambda token: token.update(value=float(token.value)),
        "INT": lambda token: token.update(value=int(token.value)),
        "STRING": lambda token: token.update(value=token.value[1:-1]),
        "SYSDEF": lambda token: token.update(value=SysDef(token.value)),
        "TYPE": lambda token: token.update(value=Type(token.value)),
    }

    comments: List[Token] = []
    if include_comments:

        def _comments_callback(token: Token) -> None:
            # Strip leading //
            token = token.update(value=token.value[2:].strip())
            comments.append(token)

        lexer_callbacks["COMMENT"] = _comments_callback

    parser = Lark(
        GRAMMAR,
        parser="lalr",
        propagate_positions=True,
        lexer_callbacks=lexer_callbacks,
        cache=cache,
    )
    try:
        tree = parser.parse(content + "\n")
    except (ParseError, UnexpectedInput, UnexpectedToken):
        logger.exception("Failed to parse IPL")
        raise

    # Always call this even if there aren't comments so that
    # the comment attributes are added.
    CommentVisitor(comments).visit_topdown(tree)
    return tree
