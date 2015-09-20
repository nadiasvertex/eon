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
        self.decoder_path = os.path.join(self.data_dir, "decode")

        if not os.path.exists(self.data_dir):
            os.makedirs(self.data_dir)

    def _gen_write_value(self, p):
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
        p.write("f.write(&bytes).unwrap();\n")

    def _gen_read_value(self, p):
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

        p.write("let mut bytes = [0; %d];\n" % (limit / 8))
        p.write("if f.read(&mut bytes).unwrap() == 0 {\n")
        p.indent()
        p.write('println!("");\nbreak;\n')
        p.dedent()
        p.write("}\n")

        type_name = data_type_map[self.data_type]
        values = ["((bytes[%d] as %s)<<%d)" % (i / 8, type_name, i) for i in range(0, limit, 8)]
        p.write("let value = ")
        p.write("|".join(values))
        p.write(";\n")
        p.write('print!("{} ", value);\n')

    def _gen_get_filename(self, p):
        p.write('let filename = env::args().nth(1).expect("Filename not provided as first argument.");\n')

    def _make_encoder(self):
        """
        An encoder takes a list of values and converts it into binary
        values of the correct type. Those are written into a column data file.
        """
        p = IndentedWriter()

        p.write("use std::io;\n")
        p.write("use std::io::prelude::*;\n")
        p.write("use std::env;\n")
        p.write("use std::fs::OpenOptions;\n\n")

        p.write("fn main() {\n")
        p.indent()

        self._gen_get_filename(p)
        p.write('let mut f = OpenOptions::new().write(true).append(true).create(true).open(filename).unwrap();\n\n')

        p.write("let mut data = String::new();\n")
        p.write("io::stdin().read_line(&mut data).unwrap();\n")
        p.write('for v in data.split_whitespace() {\n')
        p.indent()
        p.write("let value = v.parse::<%s>().unwrap();\n" % data_type_map[self.data_type])
        self._gen_write_value(p)
        p.dedent()
        p.write("}\n")

        p.dedent()
        p.write("}\n")

        encoder_src = os.path.join(self.data_dir, "encode.rs")
        with open(encoder_src, "w") as o:
            o.write(p.getvalue())

        return os.system("rustc -O -o %s %s" % (self.encoder_path, encoder_src)) == 0

    def _make_decoder(self):
        """
        An decoder takes a binary column data file and writes its values to stdout.
        """
        p = IndentedWriter()

        p.write("use std::io::prelude::*;\n")
        p.write("use std::env;\n")
        p.write("use std::fs::OpenOptions;\n\n")

        p.write("fn main() {\n")
        p.indent()

        self._gen_get_filename(p)
        p.write('let mut f = OpenOptions::new().read(true).open(filename).unwrap();\n\n')

        p.write("loop {\n")
        p.indent()

        self._gen_read_value(p)
        p.dedent()
        p.write("}\n")

        p.dedent()
        p.write("}\n")

        decoder_src = os.path.join(self.data_dir, "decode.rs")
        with open(decoder_src, "w") as o:
            o.write(p.getvalue())

        return os.system("rustc -O -o %s %s" % (self.decoder_path, decoder_src)) == 0

    def get_encoder(self):
        if not os.path.exists(self.encoder_path):
            if not self._make_encoder():
                return None

        return self.encoder_path

    def get_decoder(self):
        if not os.path.exists(self.decoder_path):
            if not self._make_decoder():
                return None

        return self.decoder_path
