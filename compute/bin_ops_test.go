package compute

import (
	inf "speter.net/go/exp/math/dec/inf"
	"testing"
	"time"
)

func TestEqNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}
func TestEqBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Bool),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestEqDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Eq)

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 == 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}
func TestNeBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Bool),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestNeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Ne)

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 != 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestLtTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLtDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Lt)

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 < 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestGtTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGtDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Gt)

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 > 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestLeTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestLeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Le)

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 <= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestGeTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestGeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Ge)

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(bool) != (0 >= 1) {
		t.Error("Expected result to be false.")
	}
}
func TestAndNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		And)

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestAndTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		And)

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 & 1) {
		t.Error("Expected result to be 0 & 1.")
	}
}
func TestAndSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		And)

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 & 1) {
		t.Error("Expected result to be 0 & 1.")
	}
}
func TestAndInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		And)

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 & 1) {
		t.Error("Expected result to be 0 & 1.")
	}
}
func TestAndBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		And)

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 & 1) {
		t.Error("Expected result to be 0 & 1.")
	}
}

//
//
//
func TestOrNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Or)

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestOrTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Or)

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 | 1) {
		t.Error("Expected result to be 0 | 1.")
	}
}
func TestOrSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Or)

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 | 1) {
		t.Error("Expected result to be 0 | 1.")
	}
}
func TestOrInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Or)

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 | 1) {
		t.Error("Expected result to be 0 | 1.")
	}
}
func TestOrBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Or)

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 | 1) {
		t.Error("Expected result to be 0 | 1.")
	}
}

//
//
//
func TestAddNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestAddTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 + 1) {
		t.Error("Expected result to be 0 + 1.")
	}
}
func TestAddSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 + 1) {
		t.Error("Expected result to be 0 + 1.")
	}
}
func TestAddInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 + 1) {
		t.Error("Expected result to be 0 + 1.")
	}
}
func TestAddBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 + 1) {
		t.Error("Expected result to be 0 + 1.")
	}
}
func TestAddDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

}
func TestAddString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			String),
		Add)

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)

}

//
func TestSubNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestSubTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 - 1) {
		t.Error("Expected result to be 0 - 1.")
	}
}
func TestSubSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 - 1) {
		t.Error("Expected result to be 0 - 1.")
	}
}
func TestSubInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 - 1) {
		t.Error("Expected result to be 0 - 1.")
	}
}
func TestSubBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 - 1) {
		t.Error("Expected result to be 0 - 1.")
	}
}
func TestSubDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

}

//
func TestSubDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			DateTime),
		Sub)

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)

}
func TestMulNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Mul)

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestMulTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Mul)

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 * 1) {
		t.Error("Expected result to be 0 * 1.")
	}
}
func TestMulSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Mul)

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 * 1) {
		t.Error("Expected result to be 0 * 1.")
	}
}
func TestMulInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Mul)

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 * 1) {
		t.Error("Expected result to be 0 * 1.")
	}
}
func TestMulBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Mul)

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 * 1) {
		t.Error("Expected result to be 0 * 1.")
	}
}
func TestMulDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Decimal),
		Mul)

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)

}

//
//
func TestDivNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Div)

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestDivTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Div)

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 / 1) {
		t.Error("Expected result to be 0 / 1.")
	}
}
func TestDivSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Div)

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 / 1) {
		t.Error("Expected result to be 0 / 1.")
	}
}
func TestDivInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Div)

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 / 1) {
		t.Error("Expected result to be 0 / 1.")
	}
}
func TestDivBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Div)

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 / 1) {
		t.Error("Expected result to be 0 / 1.")
	}
}

//
//
//
func TestModNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Null),
		Mod)

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Type != Null {
		t.Error("Expected result to be null.")
	}
}

//
func TestModTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			TinyInteger),
		Mod)

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int8) != (0 % 1) {
		t.Error("Expected result to be 0 % 1.")
	}
}
func TestModSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			SmallInteger),
		Mod)

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int16) != (0 % 1) {
		t.Error("Expected result to be 0 % 1.")
	}
}
func TestModInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			Integer),
		Mod)

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int32) != (0 % 1) {
		t.Error("Expected result to be 0 % 1.")
	}
}
func TestModBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := set_op_code(
		set_op_type(
			set_binop_dst_register(
				set_binop_src1_register(
					set_binop_src2_register(0, 1),
					0),
				2),
			BigInteger),
		Mod)

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)

	if m.Registers[2].Value.(int64) != (0 % 1) {
		t.Error("Expected result to be 0 % 1.")
	}
}

//
//
//
