package compute

import (
	//inf "speter.net/go/exp/math/dec/inf"
	"fmt"
	"time"
)

func exec_literalop(instruction Instruction, p* Predicate, m *Machine) {
	op_type := get_op_type(instruction)
	dst_index := get_litop_register(instruction)
	dst := &m.Registers[dst_index]

	switch(get_op_code(instruction)) {
	case LoadLiteral:
		switch(op_type) {
		case TinyInteger:
			dst.Type = TinyInteger
			dst.Value = get_litop_data_as_int8(instruction)
		case SmallInteger:
			dst.Type = SmallInteger
			dst.Value = get_litop_data_as_int16(instruction)
		case Integer:
			dst.Type = Integer
			dst.Value = get_litop_data_as_int32(instruction)
		case BigInteger:
			dst.Type = BigInteger
			dst.Value = get_litop_data_as_int64(instruction)
		default:
			panic("Unable to load this type of data as direct literal.")
		}
	case LoadLiteralIndirect:
		offset := get_litop_data_offset(instruction)
		switch(op_type) {
		case BigInteger:
			dst.Type = BigInteger
			dst.Value = uint64(p.Data[offset]) | uint64(p.Data[offset+1])<<8 | uint64(p.Data[offset+2])<<16 |
						uint64(p.Data[offset+3])<<24 | uint64(p.Data[offset+4])<<32 | uint64(p.Data[offset+5])<<40 |
						uint64(p.Data[offset+6])<<48 | uint64(p.Data[offset+7])<<56
		case DateTime:
			dst.Type = DateTime
			t := new(time.Time) 
			length := uint32(p.Data[offset])
			err := t.UnmarshalBinary(p.Data[offset+1:offset+1+length])
			dst.Value = t;

			if err!=nil {
				panic(fmt.Sprintf("Unable to load DateTime: %v", err))
			}

		default:
			panic("Unable to load this type of data as indirect literal.")
		}
	case LoadParameter:
	}
}