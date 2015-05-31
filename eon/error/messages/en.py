from eon.error import code

__author__ = 'Christopher Nelson'

messages = {
    code.UNKNOWN_SCHEMA_OBJECT: "Reference made to an unknown schema object named '{name}'.",
    code.DUPLICATE_SCHEMA_OBJECT: "Schema object '{name}' already exists.",
    code.MALFORMED_SCHEMA_SPEC: "Schema specification is missing a required field: '{required_field_name}'",
    code.UNKNOWN_DATA_TYPE: "Data type '{value}' is not a known type. Allowed values are: {allowed_values}",
    code.ILLEGAL_DATA_LENGTH: "Data type '{name}' is not allowed to have a max-length constraint of '{limit}'",
    code.MISSING_REQUEST_BODY: "The command requires a message body, but none was provided.",
    code.INDEX_ERROR: "Row identifier '{rid}' does not exist in object '{object_name}'.",
}
