import logging
from typing import Callable, Dict, List, Union

from lark import Lark, Token, Tree
from lark.exceptions import ParseError, UnexpectedInput, UnexpectedToken

from .grammar import GRAMMAR
from .ipl import SysDef, Type
from .visitors import CommentVisitor

logger = logging.getLogger(__name__)

LexerCallback = Callable[[Token], Union[Token, None]]


def _parse(content: str, include_comments: bool) -> Tree:
    callbacks: Dict[str, LexerCallback] = {
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

        callbacks["COMMENT"] = _comments_callback

    parser = Lark(
        GRAMMAR,
        parser="lalr",
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
    return tree


def parse(content: str, include_comments=True) -> Tree:
    """Parse IPL code into a parse tree.

    :param content: The IPL source file as a string.
    :param include_comments: Include comments in the tree.
    """
    tree = _parse(content, include_comments)
    return tree
