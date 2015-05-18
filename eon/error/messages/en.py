from eon.error import code

__author__ = 'Christopher Nelson'

messages = {
    code.UNKNOWN_SCHEMA_OBJECT: "Reference made to an unknown schema object named '{name}'.",
    code.DUPLICATE_SCHEMA_OBJECT: "Schema object '{name}' already exists.",
    code.MALFORMED_SCHEMA_SPEC: "Schema specification is missing a required field: '{required_field_name}'",
    code.INDEX_ERROR: "Row identifier '{rid}' does not exist in object '{object_name}'.",
}
