
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
		case Eq:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				
				v1 := src1_reg.Value.(bool)
				v2 := src2_reg.Value.(bool)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 == v2

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 == v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 == v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 == v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 == v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Bool


				result := v1.Cmp(v2);
				dst_reg.Value = result == 0

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 == v2

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1.Equal(v2)

			}
		case Ne:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				
				v1 := src1_reg.Value.(bool)
				v2 := src2_reg.Value.(bool)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 != v2

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 != v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 != v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 != v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 != v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Bool


				result := v1.Cmp(v2);
				dst_reg.Value = result != 0

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 != v2

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = Bool


				dst_reg.Value = !v1.Equal(v2)

			}
		case Lt:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '<' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 < v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 < v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 < v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 < v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Bool


				result := v1.Cmp(v2);
				dst_reg.Value = result < 0

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 < v2

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1.Before(v2)

			}
		case Gt:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '>' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 > v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 > v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 > v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 > v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Bool


				result := v1.Cmp(v2);
				dst_reg.Value = result > 0

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 > v2

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1.After(v2)

			}
		case Le:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '<=' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 <= v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 <= v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 <= v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 <= v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Bool


				result := v1.Cmp(v2);
				dst_reg.Value = result <= 0

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 <= v2

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1.Before(v2) || v1.Equal(v2)

			}
		case Ge:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '>=' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 >= v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 >= v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 >= v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 >= v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Bool


				result := v1.Cmp(v2);
				dst_reg.Value = result >= 0

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1 >= v2

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = Bool


				dst_reg.Value = v1.After(v2)  || v1.Equal(v2)

			}
		case And:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '&' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 & v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 & v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 & v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 & v2

				
			case Decimal:
				panic("Unsupported operation '&' on type 'Decimal'.")

				
			case String:
				panic("Unsupported operation '&' on type 'String'.")

				
			case DateTime:
				panic("Unsupported operation '&' on type 'DateTime'.")

			}
		case Or:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '|' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 | v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 | v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 | v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 | v2

				
			case Decimal:
				panic("Unsupported operation '|' on type 'Decimal'.")

				
			case String:
				panic("Unsupported operation '|' on type 'String'.")

				
			case DateTime:
				panic("Unsupported operation '|' on type 'DateTime'.")

			}
		case Add:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '+' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 + v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 + v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 + v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 + v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Decimal


				dst_reg.Value.(*inf.Dec).Add(v1, v2)

				
			case String:
				
				v1 := src1_reg.Value.(string)
				v2 := src2_reg.Value.(string)
				
				dst_reg.Type = String


				dst_reg.Value = v1 + v2

				
			case DateTime:
				panic("Unsupported operation '+' on type 'DateTime'.")

			}
		case Sub:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '-' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 - v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 - v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 - v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 - v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Decimal


				dst_reg.Value.(*inf.Dec).Sub(v1, v2)

				
			case String:
				panic("Unsupported operation '-' on type 'String'.")

				
			case DateTime:
				
				v1 := src1_reg.Value.(time.Time)
				v2 := src2_reg.Value.(time.Time)
				
				dst_reg.Type = DateTime


				dst_reg.Value = v1.Sub(v2)

			}
		case Mul:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '*' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 * v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 * v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 * v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 * v2

				
			case Decimal:
				
				v1 := src1_reg.Value.(*inf.Dec)
				v2 := src2_reg.Value.(*inf.Dec)
				
				dst_reg.Type = Decimal


				dst_reg.Value.(*inf.Dec).Mul(v1, v2)

				
			case String:
				panic("Unsupported operation '*' on type 'String'.")

				
			case DateTime:
				panic("Unsupported operation '*' on type 'DateTime'.")

			}
		case Div:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '/' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 / v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 / v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 / v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 / v2

				
			case Decimal:
				panic("Unsupported operation '/' on type 'Decimal'.")

				
			case String:
				panic("Unsupported operation '/' on type 'String'.")

				
			case DateTime:
				panic("Unsupported operation '/' on type 'DateTime'.")

			}
		case Mod:
			switch(lvalue_type) {
				
			case Null:
				dst_reg.Type = Null
				dst_reg.Value = nil

				
			case Bool:
				panic("Unsupported operation '%' on type 'Bool'.")

				
			case TinyInteger:
				
				v1 := src1_reg.Value.(int8)
				v2 := src2_reg.Value.(int8)
				
				dst_reg.Type = TinyInteger


				dst_reg.Value = v1 % v2

				
			case SmallInteger:
				
				v1 := src1_reg.Value.(int16)
				v2 := src2_reg.Value.(int16)
				
				dst_reg.Type = SmallInteger


				dst_reg.Value = v1 % v2

				
			case Integer:
				
				v1 := src1_reg.Value.(int32)
				v2 := src2_reg.Value.(int32)
				
				dst_reg.Type = Integer


				dst_reg.Value = v1 % v2

				
			case BigInteger:
				
				v1 := src1_reg.Value.(int64)
				v2 := src2_reg.Value.(int64)
				
				dst_reg.Type = BigInteger


				dst_reg.Value = v1 % v2

				
			case Decimal:
				panic("Unsupported operation '%' on type 'Decimal'.")

				
			case String:
				panic("Unsupported operation '%' on type 'String'.")

				
			case DateTime:
				panic("Unsupported operation '%' on type 'DateTime'.")

			}
	}
}












