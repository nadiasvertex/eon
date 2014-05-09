package compute

import (
	inf "speter.net/go/exp/math/dec/inf"
	"testing"
	"time"
)

func TestEncodeDateTime(t *testing.T) {
	p := new(Predicate)
	p.Instructions = make([]Instruction, 8)
	p.Data = make([]byte, 64)

	n := time.Now()
	p.EncodeLiteralLoadDateTime(0, &n)

	if p.InstructionPointer!=1 {
		t.Error("There should be one and only one instruction in the array.")
	}

	if p.DataLength==0 {
		t.Error("There should be data written to the buffer.")
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	exec_literalop(p.Instructions[0], p, m)

	if !m.Registers[0].Value.(*time.Time).Equal(n) {
		t.Error("Time loaded into virtual machine register does not match time encoded into instruction.")
	}
}

func TestEncodeDecimal(t *testing.T) {
	p := new(Predicate)
	p.Instructions = make([]Instruction, 8)
	p.Data = make([]byte, 64)

	d := inf.NewDec(37, 0)
	p.EncodeLiteralLoadDecimal(0, d)

	if p.InstructionPointer!=1 {
		t.Error("There should be one and only one instruction in the array.")
	}

	if p.DataLength==0 {
		t.Error("There should be data written to the buffer.")
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	exec_literalop(p.Instructions[0], p, m)

	if m.Registers[0].Value.(*inf.Dec).Cmp(d) != 0 {
		t.Error("Value loaded into virtual machine register does not match decimal value encoded into instruction.")
	}
}

func TestEncodeString(t *testing.T) {
	p := new(Predicate)
	p.Instructions = make([]Instruction, 8)
	p.Data = make([]byte, 64)

	s := "a test string"
	p.EncodeLiteralLoadString(0, s)

	if p.InstructionPointer!=1 {
		t.Error("There should be one and only one instruction in the array.")
	}

	if p.DataLength==0 {
		t.Error("There should be data written to the buffer.")
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	exec_literalop(p.Instructions[0], p, m)

	if m.Registers[0].Value.(string) != s {
		t.Error("Value loaded into virtual machine register does not match string value encoded into instruction.")
	}
}

