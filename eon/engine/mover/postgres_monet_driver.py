import subprocess


class Driver:
    def __init__(self, database_name):
        self.database_name = database_name

    def move_to_cold(self, table_name, predicate):
        extract_query = "SELECT * FROM %s WHERE %s" % (table_name, predicate)
        options = ["DELIMITER '|'"]
        format_str = """psql -d {database} -c "COPY ({query}) TO STDOUT WITH {options}" | mclient -d {database} -s "COPY INTO {table} FROM STDIN" -"""
        cmd = format_str.format(database=self.database_name,
                                table=table_name,
                                query=extract_query,
                                options=" ".join(options))

        subprocess.check_call(cmd, shell=True)

