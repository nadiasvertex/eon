import json
from aiohttp import web
from eon.error.util import get_error_message

__author__ = 'Christopher Nelson'


def response(body):
    return web.Response(
        content_type="application/json",
        body=json.dumps(body).encode('utf-8'),
    )


def error(language, error_code, args):
    return web.Response(
        content_type="application/json", body=json.dumps({
            "success": False,
            "error_code": error_code,
            "message": get_error_message(language, error_code).format(**args)
        }).encode("utf-8"))
