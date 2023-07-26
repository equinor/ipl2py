import logging
from typing import Callable, Dict, List

from lark import Lark, Token, Tree
from lark.exceptions import ParseError, UnexpectedInput, UnexpectedToken

from .grammar import GRAMMAR
from .transformer import TokenTransformer
from .visitor import CommentVisitor

logger = logging.getLogger(__name__)


def _create_parser(
    callbacks: Dict[str, Callable],
    include_comments=False,
) -> Lark:
    optional_args = {}
    if include_comments:
        optional_args.update(dict(propagate_positions=True, lexer_callbacks=callbacks))
    return Lark(GRAMMAR, parser="lalr", **optional_args)


def parse(content: str, include_comments=True) -> Tree:
    """Parse IPL code into a parse tree."""
    comments: List[Token] = []

    def _comments_callback(token: Token) -> None:
        token = token.update(value=token.value.lstrip("//").strip())
        comments.append(token)

    callbacks = {
        "COMMENT": _comments_callback,
    }
    lark_parser = _create_parser(callbacks, include_comments)

    logger.debug("Parsing content=\n%s", content)
    try:
        tree = lark_parser.parse(content + "\n")
    except (ParseError, UnexpectedInput, UnexpectedToken):
        logger.exception("Failed to parse IPL")
        raise

    if include_comments:
        CommentVisitor(comments).visit_topdown(tree)

    tree = TokenTransformer().transform(tree)
    return tree
