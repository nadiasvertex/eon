package compute

import "testing"

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
}
func TestEqDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestEqDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestNeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestNeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestLtBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Lt) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Lt {
		t.Error("Expected op code to be 'Lt'")
	}
}
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
}
func TestLtDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestLtDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestGtBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Gt) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Gt {
		t.Error("Expected op code to be 'Gt'")
	}
}
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
}
func TestGtDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestGtDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestLeBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Le) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Le {
		t.Error("Expected op code to be 'Le'")
	}
}
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
}
func TestLeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestLeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestGeBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Ge) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Ge {
		t.Error("Expected op code to be 'Ge'")
	}
}
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
}
func TestGeDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestGeDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestAndBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(And) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}
}
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
}
func TestAndDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(And) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}
}
func TestAndString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(And) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}
}
func TestAndDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(And) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != And {
		t.Error("Expected op code to be 'And'")
	}
}
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
}
func TestOrBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Or) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}
}
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
}
func TestOrDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Or) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}
}
func TestOrString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Or) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}
}
func TestOrDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Or) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Or {
		t.Error("Expected op code to be 'Or'")
	}
}
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
}
func TestAddBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Add) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}
}
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
}
func TestAddDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestAddDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Add) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Add {
		t.Error("Expected op code to be 'Add'")
	}
}
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
}
func TestSubBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Sub) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}
}
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
}
func TestSubDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestSubString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Sub) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Sub {
		t.Error("Expected op code to be 'Sub'")
	}
}
func TestSubDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

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
}
func TestMulBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Mul) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}
}
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
}
func TestMulDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

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
}
func TestMulString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Mul) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}
}
func TestMulDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Mul) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mul {
		t.Error("Expected op code to be 'Mul'")
	}
}
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
}
func TestDivBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Div) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}
}
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
}
func TestDivDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Div) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}
}
func TestDivString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Div) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}
}
func TestDivDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Div) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Div {
		t.Error("Expected op code to be 'Div'")
	}
}
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
}
func TestModBool(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = true
	m.Registers[1].Value = false

	m.Registers[0].Type = Bool
	m.Registers[1].Type = Bool

	instruction := uint64(Mod) |
		uint64(Bool)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}
}
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
}
func TestModDecimal(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = *inf.Dec(0)
	m.Registers[1].Value = *inf.Dec(1)

	m.Registers[0].Type = Decimal
	m.Registers[1].Type = Decimal

	instruction := uint64(Mod) |
		uint64(Decimal)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}
}
func TestModString(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = string(0)
	m.Registers[1].Value = string(1)

	m.Registers[0].Type = String
	m.Registers[1].Type = String

	instruction := uint64(Mod) |
		uint64(String)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}
}
func TestModDateTime(t *testing.T) {
	m := new(Machine)
	m.Registers = make([]Register, 3)

	m.Registers[0].Value = time.Time(0)
	m.Registers[1].Value = time.Time(1)

	m.Registers[0].Type = DateTime
	m.Registers[1].Type = DateTime

	instruction := uint64(Mod) |
		uint64(DateTime)<<8 |
		uint64(2)<<16 |
		uint64(0)<<32 |
		uint64(1)<<48

	if get_op_code(instruction) != Mod {
		t.Error("Expected op code to be 'Mod'")
	}
}
