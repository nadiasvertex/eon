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

	instruction := uint64(Eq) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Eq) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Eq) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Eq) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Eq) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Eq) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Eq) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Eq) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestEqDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Eq) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Eq {
		t.Error("Expected op code to be 'Eq'")
	}

	exec_binop(instruction, m)
}
func TestNeNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := uint64(Ne) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Ne) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Ne) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Ne) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Ne) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Ne) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Ne) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Ne) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestNeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Ne) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ne {
		t.Error("Expected op code to be 'Ne'")
	}

	exec_binop(instruction, m)
}
func TestLtNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := uint64(Lt) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}

//
func TestLtTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Lt) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestLtSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Lt) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestLtInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Lt) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestLtBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Lt) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestLtDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Lt) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestLtString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Lt) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestLtDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Lt) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}

	exec_binop(instruction, m)
}
func TestGtNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := uint64(Gt) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}

//
func TestGtTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Gt) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestGtSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Gt) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestGtInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Gt) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestGtBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Gt) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestGtDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Gt) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestGtString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Gt) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestGtDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Gt) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}

	exec_binop(instruction, m)
}
func TestLeNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := uint64(Le) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}

//
func TestLeTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Le) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestLeSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Le) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestLeInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Le) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestLeBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Le) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestLeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Le) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestLeString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Le) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestLeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Le) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}

	exec_binop(instruction, m)
}
func TestGeNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := uint64(Ge) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}

//
func TestGeTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Ge) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestGeSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Ge) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestGeInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Ge) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestGeBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Ge) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestGeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Ge) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestGeString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Ge) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestGeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Date(2004, time.April, 17, 14, 0, 0, 0, time.UTC)
	m.Registers[1].Value = time.Date(2014, time.May, 7, 19, 0, 0, 0, time.UTC)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Ge) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}

	exec_binop(instruction, m)
}
func TestAndNull(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = nil
	m.Registers[1].Value = nil

	m.Registers[0].Type = Null
	m.Registers[1].Type = Null

	instruction := uint64(And) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)
}

//
func TestAndTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(And) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)
}
func TestAndSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(And) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)
}
func TestAndInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(And) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)
}
func TestAndBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(And) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}

	exec_binop(instruction, m)
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

	instruction := uint64(Or) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)
}

//
func TestOrTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Or) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)
}
func TestOrSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Or) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)
}
func TestOrInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Or) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)
}
func TestOrBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Or) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}

	exec_binop(instruction, m)
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

	instruction := uint64(Add) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)
}

//
func TestAddTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Add) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)
}
func TestAddSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Add) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)
}
func TestAddInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Add) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)
}
func TestAddBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Add) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}

	exec_binop(instruction, m)
}
func TestAddDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Add) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

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

	instruction := uint64(Add) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

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

	instruction := uint64(Sub) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)
}

//
func TestSubTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Sub) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)
}
func TestSubSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Sub) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)
}
func TestSubInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Sub) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)
}
func TestSubBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Sub) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}

	exec_binop(instruction, m)
}
func TestSubDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Sub) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

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

	instruction := uint64(Sub) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

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

	instruction := uint64(Mul) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)
}

//
func TestMulTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Mul) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)
}
func TestMulSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Mul) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)
}
func TestMulInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Mul) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)
}
func TestMulBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Mul) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}

	exec_binop(instruction, m)
}
func TestMulDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = inf.NewDec(0, 0)
	m.Registers[1].Value = inf.NewDec(1, 0)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Mul) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

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

	instruction := uint64(Div) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)
}

//
func TestDivTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Div) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)
}
func TestDivSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Div) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)
}
func TestDivInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Div) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)
}
func TestDivBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Div) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}

	exec_binop(instruction, m)
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

	instruction := uint64(Mod) |
		uint64(Null)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)
}

//
func TestModTinyInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int8(0)
	m.Registers[1].Value = int8(1)

	m.Registers[0].Type = TinyInteger
	m.Registers[1].Type = TinyInteger

	instruction := uint64(Mod) |
		uint64(TinyInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)
}
func TestModSmallInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int16(0)
	m.Registers[1].Value = int16(1)

	m.Registers[0].Type = SmallInteger
	m.Registers[1].Type = SmallInteger

	instruction := uint64(Mod) |
		uint64(SmallInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)
}
func TestModInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int32(0)
	m.Registers[1].Value = int32(1)

	m.Registers[0].Type = Integer
	m.Registers[1].Type = Integer

	instruction := uint64(Mod) |
		uint64(Integer)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)
}
func TestModBigInteger(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = int64(0)
	m.Registers[1].Value = int64(1)

	m.Registers[0].Type = BigInteger
	m.Registers[1].Type = BigInteger

	instruction := uint64(Mod) |
		uint64(BigInteger)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}

	exec_binop(instruction, m)
}

//
//
//
