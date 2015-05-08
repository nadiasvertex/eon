__author__ = 'csnelson'


class Column:
    def __init__(self, name=None, data_type=None, nullable=True, default=None, max_length=None):
        self.name = name
        self.data_type = data_type
        self.nullable = nullable
        self.default = default
        self.max_length = max_length

    def store(self):
        return {
            "name": self.name,
            "data-type": self.data_type,
            "nullable": self.nullable,
            "default": self.default,
            "max-length": self.max_length
        }

    def load(self, data):
        self.name = data["name"]
        self.data_type = data["data-type"]
        self.nullable = data["nullable"]
        self.default = data["default"]
        self.max_length = data["max-length"]
        return self
