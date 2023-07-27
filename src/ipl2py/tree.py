import lark


class Tree(lark.Tree):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        setattr(self.meta, "header_comments", [])
        setattr(self.meta, "inline_comments", [])
        setattr(self.meta, "footer_comments", [])
