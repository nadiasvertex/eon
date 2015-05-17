import os

__author__ = 'Christopher Nelson'

import importlib

message_cache = {}


def get_language_code(language):
    language_codes = [language, language.split("_")[0]]
    for lang in language_codes:
        if lang not in message_cache:
            continue
        return message_cache[lang]

    for lang in language_codes:
        module_file_path = os.path.join(os.path.dirname(__file__), "messages", lang + ".py")
        if not os.path.exists(module_file_path):
            continue

        module_path = "eon.error.messages." + lang
        m = importlib.import_module(module_path)
        message_cache[language] = m

        return m


def get_error_message(language, code):
    messages = message_cache.get(language, {})
    return messages.get(code)
