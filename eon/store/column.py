import logging
import os

from eon.schema.data import DataType
from eon.util.indent_writer import IndentedWriter

__author__ = 'Christopher Nelson'

data_type_map = {
    DataType.small_int: "i16",
    DataType.standard_int: "i32",
    DataType.big_int: "i64",
    DataType.bool: "u8",
    DataType.decimal: "u64"
}


class Column:
    def __init__(self, data_dir, data_type):
        self.log = logging.getLogger(__name__)
        self.data_type = data_type
        self.data_dir = data_dir
        self.encoder_path = os.path.join(self.data_dir, "encode")

        if not os.path.exists(self.data_dir):
            os.makedirs(self.data_dir)

    def _gen_write(self, p):
        if self.data_type == DataType.small_int:
            limit = 16
        elif self.data_type == DataType.standard_int:
            limit = 32
        elif self.data_type == DataType.big_int:
            limit = 64
        elif self.data_type == DataType.decimal:
            limit = 64
        elif self.data_type == DataType.bool:
            limit = 8

        p.write("let bytes: [u8; %d] = [" % (limit / 8))
        values = ["(value>>%d) as u8" % i for i in range(0, limit, 8)]
        p.write(",".join(values))
        p.write("];\n")
        p.write("f.write(&bytes).unwrap();\n");

    def _make_encoder(self):
        """
        An encoder takes a batch count and a list of values and converts it into binary
        values of the correct type. Those are written into a column data file.
        """
        p = IndentedWriter()

        p.write("use std::io;\n")
        p.write("use std::io::prelude::*;\n")
        p.write("use std::fs::OpenOptions;\n\n")

        p.write("fn main() {\n")
        p.indent()
        #p.write("let mut record_count_s = String::new();\n")
        #p.write("io::stdin().read_line(&mut record_count_s).unwrap();\n")
        #p.write("let record_count = record_count_s.parse::<u32>().unwrap();\n\n")

        p.write('let mut f = OpenOptions::new().write(true).append(true).create(true).open("test.array").unwrap();\n\n')

        p.write("let mut data = String::new();\n")
        p.write("io::stdin().read_line(&mut data).unwrap();\n")
        p.write('for v in data.split_whitespace() {\n')
        p.indent()
        p.write("let value = v.parse::<%s>().unwrap();\n" % data_type_map[self.data_type])
        self._gen_write(p)
        p.dedent()
        p.write("}\n")

        p.dedent()
        p.write("}\n")

        encoder_src = os.path.join(self.data_dir, "encode.rs")
        with open(encoder_src, "w") as o:
            o.write(p.getvalue())

        return os.system("rustc -O -o %s %s" % (self.encoder_path, encoder_src)) == 0

    def get_encoder(self):
        if not os.path.exists(self.encoder_path):
            if not self._make_encoder():
                return None
