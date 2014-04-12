__author__ = 'Christopher Nelson'


class Field:
    pass


class IntegerField(Field):
    """
    Represents an integer in a data model.
    """
    pass


class TextField(Field):
    """
    Represents text in a data model.
    """
    def __init__(self, limit=None):
        self.limit = limit


def model(cls):
    """
    This decorator turns a normal class into a model. It performs some post-processing to identify fields in the
    class that are intended to be model fields.
    :param cls: The class to process.
    """
    members = dir(cls)
    fields = {
        name: getattr(cls, name)
        for name in members
        if isinstance(getattr(cls, name), Field)
    }
    setattr(cls, "_fields", fields)
    return cls
