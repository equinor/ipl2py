import logging
from typing import Callable, Dict, List, Union

from lark import Lark, Token
from lark.exceptions import ParseError, UnexpectedInput, UnexpectedToken

from .grammar import GRAMMAR
from .transformer import TokenTransformer
from .tree import Tree
from .visitor import CommentVisitor

logger = logging.getLogger(__name__)

LexerCallback = Callable[[Token], Union[Token, None]]


def _parse(content: str, include_comments: bool) -> Tree:
    comments: List[Token] = []

    def _comments_callback(token: Token) -> None:
        # Strip leading //
        token = token.update(value=token.value[2:].strip())
        comments.append(token)

    callbacks: Dict[str, LexerCallback] = {}
    if include_comments:
        callbacks["COMMENT"] = _comments_callback

    parser = Lark(
        GRAMMAR,
        parser="lalr",
        tree_class=Tree,
        propagate_positions=True,
        lexer_callbacks=callbacks,
    )

    logger.debug("Parsing content=\n%s", content)
    try:
        tree = parser.parse(content + "\n")
    except (ParseError, UnexpectedInput, UnexpectedToken):
        logger.exception("Failed to parse IPL")
        raise

    if comments:
        CommentVisitor(comments).visit_topdown(tree)
    # mypy doesn't track that we've provided our own tree class
    return tree  # type: ignore


def parse(content: str, include_comments=True) -> Tree:
    """Parse IPL code into a parse tree.

    :param content: The IPL source file as a string.
    :param include_comments: Include comments in the tree.
    """
    tree = _parse(content, include_comments)
    tree = TokenTransformer().transform(tree)
    return tree
