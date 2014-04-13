import unittest

from eon.engine.data.model import model, IntegerField, TextField


class TestModelDecorator(unittest.TestCase):
    def test_find_fields(self):

        @model
        class TestModel:
            field1 = IntegerField()
            field2 = TextField()

        fields = TestModel.fields
        self.assertIn("field1", fields)
        self.assertIn("field2", fields)

    def test_has_key_field(self):

        @model
        class TestModel:
            field1 = IntegerField()
            field2 = TextField()

        fields = TestModel.fields
        self.assertIn("test_model_id", fields)

