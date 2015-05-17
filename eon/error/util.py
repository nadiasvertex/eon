import logging
import os

__author__ = 'Christopher Nelson'

import importlib

message_cache = {}


def get_language_code(language):
    log = logging.getLogger(__name__)
    language_codes = [language, language.split("_")[0]]
    for lang in language_codes:
        if lang not in message_cache:
            continue
        return message_cache[lang]

    for lang in language_codes:
        module_file_path = os.path.join(os.path.dirname(__file__), "messages", lang + ".py")
        if not os.path.exists(module_file_path):
            log.debug("no message module for language '%s' at '%s'", lang, module_file_path)
            continue

        log.debug("loading message module '%s' for language '%s'", module_file_path, lang)
        module_path = "eon.error.messages." + lang
        m = importlib.import_module(module_path)
        message_cache[language] = m

        return m.messages


def get_error_message(language, code):
    messages = get_language_code(language)
    return messages.get(code)
