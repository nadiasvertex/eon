from mako.template import Template

t = Template("""
package compute

import (
	"time"
	inf "speter.net/go/exp/math/dec/inf"
)

func exec_binop(instruction uint64, m *Machine) {
	lvalue_type := get_op_type(instruction)
	dst_reg_idx := get_binop_dst_register(instruction)
	src1_reg_idx := get_binop_src1_register(instruction)
	src2_reg_idx := get_binop_src2_register(instruction)

	dst_reg := &m.Registers[dst_reg_idx]
	src1_reg := &m.Registers[src1_reg_idx]
	src2_reg := &m.Registers[src2_reg_idx]

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
				result := inf.NewDec(0, 0)
				result.Add(v1, v2) 
				dst_reg.Value = result
				% elif go_op == '-':
				${make_fetch(type, go_type, go_op)}
				result := inf.NewDec(0, 0)
				result.Sub(v1, v2)
				dst_reg.Value = result
				% elif go_op == '*':
				${make_fetch(type, go_type, go_op)}
				result := inf.NewDec(0, 0)
				result.Mul(v1, v2)
				dst_reg.Value = result
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

tests = Template("""
package compute

import (
	"testing"
	"time"
	inf "speter.net/go/exp/math/dec/inf"
)

% for op, go_op in binops:
%   for type, go_type in types:
%     if type=="Bool" and go_op in ("<", ">", "<=", ">=", "&", "|", "+", "-", "*", "/", "%") or \
         type=="Decimal" and go_op in ("&", "|", "/", "%") or \
         type=="String" and go_op in ("&", "|", "/", "-", "*", "%") or \
         type=="DateTime" and go_op in ("&", "|", "/", "+", "*", "%"):
//
%     else:
func Test${op}${type}(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	% if type=="Null":
	${make_null_assign(type, go_type)}
	% elif type=="Bool":
	${make_bool_assign(type, go_type)}
	% elif type=="Decimal":
	${make_decimal_assign(type, go_type)}
	% elif type=="DateTime":
	${make_datetime_assign(type, go_type)}
	% else:
	${make_standard_assign(type, go_type)}
	% endif

	m.Registers[0].Type = ${type}
	m.Registers[1].Type = ${type}

	instruction := uint64(${op}) |
	               uint64(${type})<<8 |
	               uint64(2)<<16 |
	               uint64(0)<<32 |
	               uint64(1)<<48

	if get_op_code(instruction) != ${op} {
		t.Error("Expected op code to be '${op}'")
	}

	exec_binop(instruction, m)
}
%     endif
%   endfor
% endfor

<%def name="make_standard_assign(type, go_type)">
	m.Registers[0].Value = ${go_type}(0);
	m.Registers[1].Value = ${go_type}(1);
</%def>

<%def name="make_decimal_assign(type, go_type)">
	m.Registers[0].Value = inf.NewDec(0,0)
	m.Registers[1].Value = inf.NewDec(1,0)
</%def>

<%def name="make_datetime_assign(type, go_type)">
	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)
</%def>

<%def name="make_bool_assign(type, go_type)">
	m.Registers[0].Value = true
	m.Registers[1].Value = false
</%def>

<%def name="make_null_assign(type, go_type)">
	m.Registers[0].Value = nil
	m.Registers[1].Value = nil
</%def>
""")

bin_ops = [("Eq", "=="), ("Ne","!="), ("Lt", "<"), ("Gt", ">"),
		   ("Le", "<="), ("Ge",">="), ("And", "&"), ("Or", "|"),
		   ("Add", "+"), ("Sub", "-"), ("Mul", "*"), ("Div", "/"),
		   ("Mod", "%")]

types = [("Null", "nil"), ("Bool", "bool"), ("TinyInteger", "int8"), ("SmallInteger", "int16"),
		  ("Integer", "int32"), ("BigInteger", "int64"),
	      ("Decimal", "*inf.Dec"), ("String", "string"), ("DateTime", "time.Time")]

with open("bin_ops.go", "w") as out:
	out.write(t.render(binops=bin_ops, types=types))

with open("bin_ops_test.go", "w") as out:
	out.write(tests.render(binops=bin_ops, types=types))

