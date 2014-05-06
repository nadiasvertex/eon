from mako.template import Template

t = Template("""
package compute

import (
	"time"
	inf "speter.net/go/exp/math/dec/inf"
)

func exec_binop(instruction uint64, p *Predicate, m *Machine) {
	lvalue_type := get_op_type(instruction)
	dst_reg_idx := get_binop_dst_register(instruction)
	src1_reg_idx := get_binop_src1_register(instruction)
	src2_reg_idx := get_binop_src2_register(instruction)

	dst_reg := m.Registers[dst_reg_idx]
	src1_reg := m.Registers[src1_reg_idx]
	src2_reg := m.Registers[src2_reg_idx]

	switch get_op_code(instruction) {
		% for op, go_op in binops: 
		case ${op}:
			switch(lvalue_type) {
				% for type, go_type in types:
					% if type == "Null":
				${make_null_case(type, go_type, go_op)}
					% elif type=="Decimal":
				${make_decimal_case(type, go_type, go_op)}
					% elif type=="DateTime":
				${make_datetime_case(type, go_type, go_op)}
					% else:
				${make_standard_case(type, go_type, go_op)}
					% endif
				% endfor
			}
		% endfor		
	}
}

<%def name="make_result_type(type, go_op)">
				% if go_op in ("==", "!=", "<=", ">=", "<", ">"):
				dst_reg.Type = Bool
				% else:
				dst_reg.Type = ${type}
				% endif
</%def>

<%def name="make_fetch(type, go_type, go_op)">
				v1 := src1_reg.Value.(${go_type})
				v2 := src2_reg.Value.(${go_type})
				${make_result_type(type, go_op)}
</%def>

<%def name="make_standard_case(type, go_type, go_op)">
			case ${type}:
				% if type not in ("Bool", "String") or \
				     type == "Bool" and go_op in ("==", "!=") or \
				     type == "String" and go_op not in ("&", "|", "^", "%", "-", "*", "/"):
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1 ${go_op} v2
				% else:
				panic("Unsupported operation '${go_op}' on type '${type}'.")
				% endif
</%def>

<%def name="make_decimal_case(type, go_type, go_op)">
			case ${type}:
				% if go_op in ("==", "!=", "<=", ">=", "<", ">"):
				${make_fetch(type, go_type, go_op)}
				result := v1.Cmp(v2);
				dst_reg.Value = result ${go_op} 0 
				% elif go_op == '+':
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value.(${go_type}).Add(v1, v2)
				% elif go_op == '-':
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value.(${go_type}).Sub(v1, v2)
				% elif go_op == '*':
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value.(${go_type}).Mul(v1, v2)
				% else:
				panic("Unsupported operation '${go_op}' on type '${type}'.")
				% endif
</%def>

<%def name="make_datetime_case(type, go_type, go_op)">
			case ${type}:
				% if go_op == "==":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1.Equal(v2) 
				% elif go_op == "!=":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = !v1.Equal(v2) 
				% elif go_op == "<":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1.Before(v2) 
				% elif go_op == ">":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1.After(v2)
				% elif go_op == "<=":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1.Before(v2) || v1.Equal(v2)
				% elif go_op == ">=":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1.After(v2)  || v1.Equal(v2)
				% elif go_op == "-":
				${make_fetch(type, go_type, go_op)}
				dst_reg.Value = v1.Sub(v2)
				% else:
				panic("Unsupported operation '${go_op}' on type '${type}'.")
				% endif
</%def>

<%def name="make_null_case(type, go_type, go_op)">
			case ${type}:
				dst_reg.Type = Null
				dst_reg.Value = nil
</%def>
""")

bin_ops = [("Eq", "=="), ("Ne","!="), ("Lt", "<"), ("Gt", ">"),
		   ("Le", "<="), ("Ge",">="), ("And", "&"), ("Or", "|"),
		   ("Add", "+"), ("Sub", "-"), ("Mul", "*"), ("Div", "/"),
		   ("Mod", "%")]

types = [("Null", "nil"), ("Bool", "bool"), ("TinyInteger", "int8"), ("SmallInteger", "int16"),
		  ("Integer", "int32"), ("BigInteger", "int64"),
	      ("Decimal", "*inf.Dec"), ("String", "string"), ("DateTime", "time.Time")]

print(t.render(binops=bin_ops, types=types))

