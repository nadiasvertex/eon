package compute

import "testing"

func TestSimpleEq(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 8)

	m.Registers[0].Value = int8(0);
	m.Registers[1].Value = int8(1);

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(0, Eq);
	instruction = set_op_type(instruction, TinyInteger)
	instruction = set_binop_dst_register(instruction, 2)
	instruction = set_binop_src1_register(instruction, 0)
	instruction = set_binop_src2_register(instruction, 1)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	if get_op_type(instruction) != TinyInteger {
		t.Error("Expected operation type to be TinyInteger")
	}

	if get_binop_dst_register(instruction) != 2 {
		t.Error("Expected dest register == 2")
	}

	if get_binop_src1_register(instruction) != 0 {
		t.Error("Expected src1 register == 0")
	}
	
	if get_binop_src2_register(instruction) != 1 {
		t.Error("Expected src2 register == 1")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Bool {
		t.Error("Expected result register type to be bool.")
	}

	if m.Registers[2].Value.(bool) == true {
		t.Error("Expected equality test to yield false.")
	}
}
