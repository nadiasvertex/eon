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


def to_underscore(name):
    """
    Converts PascalCase to underscore_style.
    :param name: The name to convert.
    """
    v = "".join([c if c.islower() else "_" + c.lower() for c in name])
    return v if not v.startswith("_") else v[1:]


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
    id_field = to_underscore(cls.__name__ + "Id")
    if id_field not in fields:
        kf = KeyField()
        fields[id_field] = kf
        setattr(cls, id_field, kf)

    setattr(cls, "fields", fields)
    setattr(cls, "table_name", to_underscore(cls.__name__))
    setattr(cls, "key_name", id_field)
    return cls
