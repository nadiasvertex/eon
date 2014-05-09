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
}

func TestEncodeDecimal(t *testing.T) {
	p := new(Predicate)
	p.Instructions = make([]Instruction, 8)
	p.Data = make([]byte, 64)

	p.EncodeLiteralLoadDecimal(0, inf.NewDec(37, 0))

	if p.InstructionPointer!=1 {
		t.Error("There should be one and only one instruction in the array.")
	}

	if p.DataLength==0 {
		t.Error("There should be data written to the buffer.")
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	exec_literalop(p.Instructions[0], p, m)
}
