class CompilationError(Exception):
    def __init__(self, message, line=0, col=0):
        message = f"{message}, line {line or '?'} column {col or '?'}"
        super().__init__(message)


class SymbolCollisionError(Exception):
    pass
