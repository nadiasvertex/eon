import io

__author__ = 'Christopher Nelson'


class IndentedWriter:
    def __init__(self):
        self.s = io.StringIO()
        self.indent_count = 0
        self.pending_newline = False

    def _write(self, data):
        if self.pending_newline:
            self.s.write("\n")
            self.s.write(" " * self.indent_count)
            self.pending_newline = False

        self.s.write(data)

    def getvalue(self):
        return self.s.getvalue()

    def indent(self):
        self.indent_count += 1

    def dedent(self):
        self.indent_count -= 1

    def write(self, data):
        if "\n" in data:
            for c in data:
                if c == "\n":
                    self.newline()
                else:
                    self._write(c)

        else:
            self._write(data)

    def newline(self):
        if self.pending_newline:
            self.s.write("\n")

        self.pending_newline = True
