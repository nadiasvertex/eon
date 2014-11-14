__author__ = 'Christopher Nelson'


class Reader:
    def __init__(self, data, decompressor):
        self.decompressor = decompressor
        self.chunk_size = 4096
        self.data = data
        self.data_offset = 0
        self.buffer = bytearray()

    def read(self, count=1):
        if self.buffer is None:
            return None

        while len(self.buffer) < count and self.data_offset < len(self.data):
            start = self.data_offset
            end = self.data_offset + self.chunk_size
            chunk = self.decompressor.decompress(self.data[start:end])
            self.data_offset += self.chunk_size
            self.buffer += chunk

        if self.data_offset >= len(self.data) and len(self.buffer) < count:
            data = self.buffer
            self.buffer = None
            return data

        data = self.buffer[:count]
        self.buffer = self.buffer[count:]
        return data


class Writer:
    def __init__(self, compressor):
        self.data = bytearray()
        self.compressor = compressor

    def write(self, data):
        self.data += self.compressor.compress(data)

    def get_bytes(self):
        self.data += self.compressor.flush()
        return memoryview(self.data)


