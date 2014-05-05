func = """
package compute

import (
	"time"
)

func exec_binop(instruction uint64, p *Predicate, m *Machine) {{
	lvalue_type := get_op_type(instruction)
	dst_reg_idx := get_binop_dst_register(instruction)
	src1_reg_idx := get_binop_src1_register(instruction)
	src2_reg_idx := get_binop_src2_register(instruction)

	dst_reg := m.Registers[dst_reg_idx]
	src1_reg := m.Registers[src1_reg_idx]
	src2_reg := m.Registers[src2_reg_idx]

	switch get_op_code(instruction) {{
	{operations}
	}}
}}
"""

bin_ops = [("Eq", "=="), ("Ne","!="), ("Lt", "<"), ("Gt", ">"),
		   ("Le", "<="), ("Ge",">="), ("And", "&"), ("Or", "|"),
		   ("Add", "+"), ("Sub", "-"), ("Mul", "*"), ("Div", "/"),
		   ("Mod", "%")]

types = [("Null", "nil"), ("Bool", "bool"), ("SmallInteger", "int16"),
		  ("Integer", "int32"), ("BigInteger", "int64"),
	      ("Decimal", "decimal"), ("String", "string"), ("DateTime", "time.Time")]

op_case = """
	case {op}:
		switch(lvalue_type) {{
		{type_ops}
		}}
"""

op_type = """
		case {type}:
			v1 := src1_reg.Value.({go_type})
			v2 := src2_reg.Value.({go_type})
			dst_reg.Type = {type}
			dst_reg.Value = v1 {go_op} v2
"""

null_type = """
		case {type}:
			dst_reg.Type = Null
			dst_reg.Value = nil		
"""

cases = []
for bo, go in bin_ops:
	processors = []
	for btype, gtype in types:
		if btype == "Null":
			processors.append(null_type.format(type=btype))
		else:
			processors.append(op_type.format(go_op=go, type=btype, go_type=gtype))
	cases.append(op_case.format(op=bo, type_ops="\n".join(processors)))

print(func.format(operations="\n".join(cases)))

