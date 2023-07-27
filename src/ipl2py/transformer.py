from lark import Token, Transformer

from .ipl import SysDef, Type


class TokenTransformer(Transformer):
    def BOOL(self, token: Token) -> Token:
        return token.update(value=False if token.value == "FALSE" else True)

    def INT(self, token: Token) -> Token:
        return token.update(value=int(token.value))

    def FLOAT(self, token: Token) -> Token:
        return token.update(value=float(token.value))

    def STRING(self, token: Token) -> Token:
        return token.update(value=token.value[1:-1])

    def SYSDEF(self, token: Token) -> Token:
        return token.update(value=SysDef(token.value))

    def TYPE(self, token: Token) -> Token:
        return token.update(value=Type(token.value))
