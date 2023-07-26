import logging
from argparse import ArgumentParser, Namespace

from ipl2py import __version__, parse

from .utils import get_test_tree

logger = logging.getLogger(__name__)


def read_file(infile: str) -> str:
    logger.info("Opening %s", infile)
    try:
        with open(infile, encoding="utf-8") as fin:
            content = fin.read()
    except UnicodeDecodeError:
        logger.exception("Expected %s to be utf-8", infile)
        raise
    return content


def parse_args() -> Namespace:
    parser = ArgumentParser(
        prog="ipl2py",
        description="Source-to-source compiler, from  RMS IPL scripts to Python",
    )
    parser.add_argument(
        "--version",
        help="Print the version number",
        action="version",
        version=__version__,
    )
    parser.add_argument(
        "infile",
        nargs="?",  # Make it optional
        help=("The IPL file to be converted to Python"),
    )
    parser.add_argument(
        "-c",
        "--code",
        type=str,
        dest="code",
        help=(
            "Supply IPL code and print its parse tree. Can "
            "be used with `-p` to pretty print the structure. "
        ),
    )
    parser.add_argument(
        "-d",
        "--debug",
        help="Print debugging information",
        action="store_const",
        dest="loglevel",
        const=logging.DEBUG,
        default=logging.WARNING,
    )
    parser.add_argument(
        "--no-comments",
        action="store_true",
        dest="no_comments",
        help="Discard comments from the IPL script.",
        default=False,
    )
    parser.add_argument(
        "-o",
        "--outfile",
        type=str,
        dest="outfile",
        help="Output Python filepath. Defaults to the same name as the IPL script.",
    )
    parser.add_argument(
        "-p",
        "--pretty",
        action="store_true",
        dest="pretty",
        help="Pretty print the parse tree",
        default=False,
    )
    parser.add_argument(
        "-v",
        "--verbose",
        help="Be verbose",
        action="store_const",
        dest="loglevel",
        const=logging.INFO,
    )
    args = parser.parse_args()
    return args


def main():
    args = parse_args()

    logging.basicConfig(level=args.loglevel)
    if args.code:
        content = args.code
    elif args.infile:
        content = read_file(args.infile)
    else:
        content = get_test_tree()

    tree = parse(content, include_comments=False if args.no_comments else True)

    if args.pretty:
        print(tree.pretty())
    else:
        print(tree)


if __name__ == "__main__":
    main()
