from lark import Token, Transformer


class TokenTransformer(Transformer):
    def INT(self, token: Token) -> Token:
        return token.update(value=int(token.value))

    def FLOAT(self, token: Token) -> Token:
        return token.update(value=float(token.value))

    def BOOL(self, token: Token) -> Token:
        return token.update(value=False if token.value == "FALSE" else True)

    def STRING(self, token: Token) -> Token:
        return token.update(value=token.value[1:-1])
