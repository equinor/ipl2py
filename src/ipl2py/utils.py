from lark import Tree


def get_test_tree() -> str:
    """Put IPL strings in here for testing during development."""
    return """
// 2
IF TRUE THEN // 3
    // 4
    IF TRUE THEN // 5
        HALT // 6
    ELSE // 7
        HALT // 8
    ENDIF // 9
    // 10
ENDIF // 11
// 12
    """


def print_assigned_comments(tree: Tree) -> None:
    """Useful for debugging"""
    tree_iter = tree.iter_subtrees_topdown()
    for node in tree_iter:
        print("#" * 15)
        print(f"{node.data} line={node.meta.line} end_line={node.meta.end_line}")
        for position in ("header", "inline", "footer"):
            try:
                comments = getattr(node.meta, f"{position}_comments")
                print(f" - {position}_comments", comments)
            except AttributeError:
                pass
