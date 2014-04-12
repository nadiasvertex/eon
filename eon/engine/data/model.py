__author__ = 'Christopher Nelson'


class Field:
    pass


class KeyField(Field):
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
    id_field = cls.__name__ + "Id"
    id_field = "".join([c if c.islower() else "_" + c.lower() for c in id_field])
    if id_field.startswith("_"):
        id_field = id_field[1:]
    if id_field not in fields:
        kf = KeyField()
        fields[id_field] = kf
        setattr(cls, id_field, kf)

    setattr(cls, "_fields", fields)
    return cls
