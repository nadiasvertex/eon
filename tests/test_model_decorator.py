import unittest

from eon.engine.data.model import model, IntegerField, TextField


class TestModelDecorator(unittest.TestCase):
    def test_find_fields(self):

        @model
        class TestModel:
            field1 = IntegerField()
            field2 = TextField()

        fields = getattr(TestModel, "_fields")
        self.assertIn("field1", fields)
        self.assertIn("field2", fields)
