package compute

import (
	//inf "speter.net/go/exp/math/dec/inf"
	"testing"
	"time"
)

func TestLoadLiteralIndirectDateTime(t *testing.T) {
	tm := time.Now()
	data, err := tm.MarshalBinary();
	if err!=nil {
		t.Error("Unable to serialize time value.")
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	p := new(Predicate)
	instruction := set_op_code(
		set_op_type(
			set_litop_register(0, 0),
			DateTime),
		LoadLiteralIndirect)

	buffer := make([]byte, len(data)+64)
	buffer[0] = 0
	buffer[1] = 1
	copy(buffer[2:], data)

	p.Data = buffer

	exec_literalop(instruction, p, m)

	if m.Registers[0].Value.(*time.Time).Equal(tm) == false {
		t.Error("The loaded time value is not the same as the one stored.")
	}
}
