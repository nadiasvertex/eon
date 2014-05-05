package compute

import (
	"time"
)

func exec_binop(instruction uint64, p *Predicate, m *Machine) {
	lvalue_type := get_op_type(instruction)
	dst_reg_idx := get_binop_dst_register(instruction)
	src1_reg_idx := get_binop_src1_register(instruction)
	src2_reg_idx := get_binop_src2_register(instruction)

	dst_reg := m.Registers[dst_reg_idx]
	src1_reg := m.Registers[src1_reg_idx]
	src2_reg := m.Registers[src2_reg_idx]

	switch get_op_code(instruction) {

	case Eq:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
			dst_reg.Value = v1 == v2

		case SmallInteger:
			v1 := src1_reg.Value.(int16)
			v2 := src2_reg.Value.(int16)
			dst_reg.Type = SmallInteger
			dst_reg.Value = v1 == v2

		case Integer:
			v1 := src1_reg.Value.(int32)
			v2 := src2_reg.Value.(int32)
			dst_reg.Type = Integer
			dst_reg.Value = v1 == v2

		case BigInteger:
			v1 := src1_reg.Value.(int64)
			v2 := src2_reg.Value.(int64)
			dst_reg.Type = BigInteger
			dst_reg.Value = v1 == v2

		case Decimal:
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 == v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 == v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 == v2

		}

	case Ne:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
			dst_reg.Value = v1 != v2

		case SmallInteger:
			v1 := src1_reg.Value.(int16)
			v2 := src2_reg.Value.(int16)
			dst_reg.Type = SmallInteger
			dst_reg.Value = v1 != v2

		case Integer:
			v1 := src1_reg.Value.(int32)
			v2 := src2_reg.Value.(int32)
			dst_reg.Type = Integer
			dst_reg.Value = v1 != v2

		case BigInteger:
			v1 := src1_reg.Value.(int64)
			v2 := src2_reg.Value.(int64)
			dst_reg.Type = BigInteger
			dst_reg.Value = v1 != v2

		case Decimal:
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 != v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 != v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 != v2

		}

	case Lt:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
			dst_reg.Value = v1 < v2

		case SmallInteger:
			v1 := src1_reg.Value.(int16)
			v2 := src2_reg.Value.(int16)
			dst_reg.Type = SmallInteger
			dst_reg.Value = v1 < v2

		case Integer:
			v1 := src1_reg.Value.(int32)
			v2 := src2_reg.Value.(int32)
			dst_reg.Type = Integer
			dst_reg.Value = v1 < v2

		case BigInteger:
			v1 := src1_reg.Value.(int64)
			v2 := src2_reg.Value.(int64)
			dst_reg.Type = BigInteger
			dst_reg.Value = v1 < v2

		case Decimal:
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 < v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 < v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 < v2

		}

	case Gt:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
			dst_reg.Value = v1 > v2

		case SmallInteger:
			v1 := src1_reg.Value.(int16)
			v2 := src2_reg.Value.(int16)
			dst_reg.Type = SmallInteger
			dst_reg.Value = v1 > v2

		case Integer:
			v1 := src1_reg.Value.(int32)
			v2 := src2_reg.Value.(int32)
			dst_reg.Type = Integer
			dst_reg.Value = v1 > v2

		case BigInteger:
			v1 := src1_reg.Value.(int64)
			v2 := src2_reg.Value.(int64)
			dst_reg.Type = BigInteger
			dst_reg.Value = v1 > v2

		case Decimal:
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 > v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 > v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 > v2

		}

	case Le:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
			dst_reg.Value = v1 <= v2

		case SmallInteger:
			v1 := src1_reg.Value.(int16)
			v2 := src2_reg.Value.(int16)
			dst_reg.Type = SmallInteger
			dst_reg.Value = v1 <= v2

		case Integer:
			v1 := src1_reg.Value.(int32)
			v2 := src2_reg.Value.(int32)
			dst_reg.Type = Integer
			dst_reg.Value = v1 <= v2

		case BigInteger:
			v1 := src1_reg.Value.(int64)
			v2 := src2_reg.Value.(int64)
			dst_reg.Type = BigInteger
			dst_reg.Value = v1 <= v2

		case Decimal:
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 <= v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 <= v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 <= v2

		}

	case Ge:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
			dst_reg.Value = v1 >= v2

		case SmallInteger:
			v1 := src1_reg.Value.(int16)
			v2 := src2_reg.Value.(int16)
			dst_reg.Type = SmallInteger
			dst_reg.Value = v1 >= v2

		case Integer:
			v1 := src1_reg.Value.(int32)
			v2 := src2_reg.Value.(int32)
			dst_reg.Type = Integer
			dst_reg.Value = v1 >= v2

		case BigInteger:
			v1 := src1_reg.Value.(int64)
			v2 := src2_reg.Value.(int64)
			dst_reg.Type = BigInteger
			dst_reg.Value = v1 >= v2

		case Decimal:
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 >= v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 >= v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 >= v2

		}

	case And:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 & v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 & v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 & v2

		}

	case Or:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 | v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 | v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 | v2

		}

	case Add:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 + v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 + v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 + v2

		}

	case Sub:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 - v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 - v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 - v2

		}

	case Mul:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 * v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 * v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 * v2

		}

	case Div:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 / v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 / v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 / v2

		}

	case Mod:
		switch lvalue_type {

		case Null:
			dst_reg.Type = Null
			dst_reg.Value = nil

		case Bool:
			v1 := src1_reg.Value.(bool)
			v2 := src2_reg.Value.(bool)
			dst_reg.Type = Bool
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
			v1 := src1_reg.Value.(decimal)
			v2 := src2_reg.Value.(decimal)
			dst_reg.Type = Decimal
			dst_reg.Value = v1 % v2

		case String:
			v1 := src1_reg.Value.(string)
			v2 := src2_reg.Value.(string)
			dst_reg.Type = String
			dst_reg.Value = v1 % v2

		case DateTime:
			v1 := src1_reg.Value.(time.Time)
			v2 := src2_reg.Value.(time.Time)
			dst_reg.Type = DateTime
			dst_reg.Value = v1 % v2

		}

	}
}
