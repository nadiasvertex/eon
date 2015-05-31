__author__ = 'Christopher Nelson'

class DataDefinitionLanguageError(RuntimeError):
    def __init__(self, code, message):
        self.code = code
        self.message = message
