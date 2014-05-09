package compute

import (
	inf "speter.net/go/exp/math/dec/inf"
	"fmt"
	"testing"
	"time"
)

func TestLoadLiteralIndirectDateTime(t *testing.T) {
	tm := time.Now()
	data, err := tm.GobEncode();
	if err!=nil {
		t.Error(fmt.Sprintf("Unable to serialize time value: %v", err))
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	p := new(Predicate)
	instruction := set_op_code(
		set_op_type(
			set_litop_register(
				set_litop_data_offset(0, 2), 
				0),
			DateTime),
		LoadLiteralIndirect)

	if (get_op_code(instruction) != LoadLiteralIndirect) {
		t.Error(fmt.Sprintf("Op code '%v' is not LoadLiteralIndirect.", get_op_code(instruction)))
	}

	if (get_op_type(instruction) != DateTime) {
		t.Error(fmt.Sprintf("Op type '%v' is not DateTime", get_op_type(instruction)))
	}

	if (get_litop_register(instruction) != 0) {
		t.Error("Target register is not 0.")
	}

	if (get_litop_data_offset(instruction)!=2) {
		t.Error("Data offset is not 2.")
	}

	buffer := make([]byte, len(data)+64)
	buffer[0] = 0
	buffer[1] = 1
	buffer[2] = uint8(len(data))
	copy(buffer[3:], data)

	p.Data = buffer

	exec_literalop(instruction, p, m)

	loaded_datetime := m.Registers[0].Value.(*time.Time)
	if loaded_datetime.Equal(tm) == false {
		t.Error(fmt.Sprintf("The loaded time value (%v) is not the same as the one stored (%v).", loaded_datetime, tm))
	}
}

func TestLoadLiteralIndirectDecimal(t *testing.T) {
	v := inf.NewDec(37,0)
	data, err := v.GobEncode();
	if err!=nil {
		t.Error(fmt.Sprintf("Unable to serialize decimal value: %v", err))
	}

	m := new(Machine)
	m.Registers = make([]Register, 3)

	p := new(Predicate)
	instruction := set_op_code(
		set_op_type(
			set_litop_register(
				set_litop_data_offset(0, 2), 
				0),
			Decimal),
		LoadLiteralIndirect)

	if (get_op_code(instruction) != LoadLiteralIndirect) {
		t.Error(fmt.Sprintf("Op code '%v' is not LoadLiteralIndirect.", get_op_code(instruction)))
	}

	if (get_op_type(instruction) != Decimal) {
		t.Error(fmt.Sprintf("Op type '%v' is not Decimal", get_op_type(instruction)))
	}

	if (get_litop_register(instruction) != 0) {
		t.Error("Target register is not 0.")
	}

	if (get_litop_data_offset(instruction)!=2) {
		t.Error("Data offset is not 2.")
	}

	buffer := make([]byte, len(data)+64)
	buffer[0] = 0
	buffer[1] = 1
	buffer[2] = uint8(len(data))
	copy(buffer[3:], data)

	p.Data = buffer

	exec_literalop(instruction, p, m)

	loaded_decimal := m.Registers[0].Value.(*inf.Dec)
	if loaded_decimal.Cmp(v) != 0 {
		t.Error(fmt.Sprintf("The loaded time value (%v) is not the same as the one stored (%v).", loaded_decimal, v))
	}
}
