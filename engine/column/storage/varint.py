# Protocol Buffers - Google's data interchange format
# Copyright 2008 Google Inc.  All rights reserved.
# http://code.google.com/p/protobuf/
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following disclaimer
# in the documentation and/or other materials provided with the
# distribution.
#     * Neither the name of Google Inc. nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


class NotEnoughDataError(Exception):
    pass


def _varint_decoder(mask):
    """Return an encoder for a basic varint value (does not include tag).

    Decoded values will be bitwise-anded with the given mask before being
    returned, e.g. to limit them to 32 bits.  The returned decoder does not
    take the usual "end" parameter -- the caller is expected to do bounds checking
    after the fact (often the caller can defer such checking until later).  The
    decoder returns a (value, new_pos) pair.
    """

    def decode_varint(buffer, pos):
        result = 0
        shift = 0
        while 1:
            if pos > len(buffer) - 1:
                raise NotEnoughDataError("Not enough data to decode varint")
            b = buffer[pos]
            result |= ((b & 0x7f) << shift)
            pos += 1
            if not (b & 0x80):
                result &= mask
                return (result, pos)
            shift += 7
            if shift >= 64:
                raise RuntimeError('Too many bytes when decoding varint.')

    return decode_varint


def _signed_varint_decoder(mask):
    """Like _varint_decoder() but decodes signed values."""

    def decode_varint(buffer, pos):
        result = 0
        shift = 0
        while 1:
            if pos > len(buffer) - 1:
                raise NotEnoughDataError("Not enough data to decode varint")
            b = buffer[pos]
            result |= ((b & 0x7f) << shift)
            pos += 1
            if not (b & 0x80):
                if result > 0x7fffffffffffffff:
                    result -= (1 << 64)
                    result |= ~mask
                else:
                    result &= mask
                return (result, pos)
            shift += 7
            if shift >= 64:
                raise RuntimeError('Too many bytes when decoding varint.')

    return decode_varint


decode = _varint_decoder((1 << 64) - 1)
decode_signed = _signed_varint_decoder((1 << 64) - 1)


# Use these versions for values which must be limited to 32 bits.
decode_int32 = _varint_decoder((1 << 32) - 1)
decode_signed_int32 = _signed_varint_decoder((1 << 32) - 1)


def varint_size(value):
    """Compute the size of a varint value."""
    if value <= 0x7f: return 1
    if value <= 0x3fff: return 2
    if value <= 0x1fffff: return 3
    if value <= 0xfffffff: return 4
    if value <= 0x7ffffffff: return 5
    if value <= 0x3ffffffffff: return 6
    if value <= 0x1ffffffffffff: return 7
    if value <= 0xffffffffffffff: return 8
    if value <= 0x7fffffffffffffff: return 9
    return 10


def signed_varint_size(value):
    """Compute the size of a signed varint value."""
    if value < 0: return 10
    if value <= 0x7f: return 1
    if value <= 0x3fff: return 2
    if value <= 0x1fffff: return 3
    if value <= 0xfffffff: return 4
    if value <= 0x7ffffffff: return 5
    if value <= 0x3ffffffffff: return 6
    if value <= 0x1ffffffffffff: return 7
    if value <= 0xffffffffffffff: return 8
    if value <= 0x7fffffffffffffff: return 9
    return 10


def _varint_encoder():
    """Return an encoder for a basic varint value."""

    def encode_varint(write, value):
        bits = value & 0x7f
        value >>= 7
        while value:
            write(0x80 | bits)
            bits = value & 0x7f
            value >>= 7
        return write(bits)

    return encode_varint


def _signed_varint_encoder():
    """Return an encoder for a basic signed varint value."""

    def encode_signed_varint(write, value):
        if value < 0:
            value += (1 << 64)
        bits = value & 0x7f
        value >>= 7
        while value:
            write(0x80 | bits)
            bits = value & 0x7f
            value >>= 7
        return write(bits)

    return encode_signed_varint


encode = _varint_encoder()
encode_signed = _signed_varint_encoder()
