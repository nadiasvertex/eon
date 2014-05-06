package compute

/*
  Predicate matching works like this:

  SELECT * FROM test WHERE ((c1 > 5 AND c2='train') OR (c1 < 2 AND c2='train')) AND
                          ((c1!=0 AND c1!=10) OR (c1!=0 AND c1!=20));

 We need to provide for a general expression solver, and we need to provide byte code that can encode
 general expressions. Thus we have the eon virtual machine.

 The machine has an infinite number of registers. The registers hold typed values. An instruction generally writes it's
 result back to a register.

 Binary operations:
 byte:operation byte:type ushort:dest_register ushort:source_register_1 ushort:source_register_2

 Load/Store operations:
 byte:operation byte:type ushort:dest_or_source_register uint:column

 Context operations:
 byte:operation ushort:register uint:offset

 Literal operations:
 byte:operation byte:type ushort:dest_register uint:offset_or_data

 Branch operations:
 byte:operation ushort:register uint:offset

 For context operations there are two possible data components: the register where the value comes from, and an offset into
 the data page associated with the code. For operations like ChooseTable, the register contains an object identifier that should
 be loaded as the active table. ChooseRow is similar, except that it uses the internal row_id, whatever that is.

 For literal operations, if the literal value is small enough to fit in the 32-bit offset_or_data value, then the value is
 stored there. The operation is encoded as a 'load_literal'. If the 'type' is a 64-bit integer, then if the value can be encoded in
 32-bits it will also be stored directly. In all other cases the offset_or_data parameter references an offset into the binary
 data page associated with the code. The operation is encoded as 'load_literal_indirect' The operation 'load_parameter' is used to
 load a parameterized value for pre-compiled queries. That way if we store the compiled query we can simply refer to a parameter
 and avoid having to re-write any part of the

 The predicate above could be encoded like this:

 term1:
 r0 = load int c1
 r1 = load_literal int 5
 r2 = r0 gt r1
 branch_if_false r5 => term2
(add-hook 'before-save-hook 'gofmt-before-save) r3 = load str c2
 r4 = load_literal_indirect str 0 ; the only literal in the data space is 'train' which starts at offset 0.
 r5 = r3 eq r4
 branch_if_false r5 => term2
 branch => term4

 term2:
 r7 = load_literal int 2
 r8 = r0 lt r7
 branch_if_false r8 => failed
 r9 = r8 and r5
 branch_if_false r9 => failed
 branch => term4

 term4:
 r10 = load_literal int 0
 r11 = r0 ne r10
 branch_if_false r11 => failed
 r12 = load_literal int 10
 r13 = r0 ne r12
 branch_if_false r13 => term5
 branch => passed

 term5:
 r14 = load_literal int 20
 r15 = r0 ne r14
 branch_if_false r15 => failed
 branch => passed

 failed:
 fail

 passed:
 pass

*/

type Opcode byte

const (
	Nop Opcode = iota

	// Binary operations
	Eq
	Ne
	Ge
	Le
	Gt
	Lt
	And
	Or
	Add
	Sub
	Mul
	Div
	Mod

	// Load/Store operations
	Load
	Store

	// Context operations
	ChooseTable
	ChooseRow

	// Literal operations
	LoadLiteral
	LoadLiteralIndirect
	LoadParameter

	// Flow control operations
	BranchIfTrue
	BranchIfFalse
	Branch
	Pass
	Fail
)

type ValueType byte

const (
	Null ValueType = iota
	Bool
	SmallInteger
	Integer
	BigInteger
	Decimal
	String
	DateTime
)

type Op struct {
	Cmd Opcode
	O1  uint16
	O2  uint16
	O3  uint
}

type BinOp struct {
	Cmd  Opcode
	Type byte
	Dst  uint16
	Src1 uint16
	Src2 uint16
}

type LoadStoreOp struct {
	Cmd      Opcode
	Type     byte
	Register uint16
	Column   uint
}

type ContextOp struct {
	Cmd      Opcode
	Register uint16
	Offset   uint32
}

type LiteralOp struct {
	Cmd        Opcode
	Type       byte
	Dst        uint16
	OffsetData uint32
}

type BranchOp struct {
	Cmd      Opcode
	Register uint16
	Offset   uint32
}

type Predicate struct {
	Instructions       []uint64
	InstructionPointer int
	RegisterFileSize   int
	Data               []byte
}

type Register struct {
	Type    ValueType
	Written bool
	Value   interface{}
}

type Machine struct {
	Registers []Register
}

func get_op_code(instruction uint64) Opcode {
	return Opcode(instruction)
}

func get_op_type(instruction uint64) ValueType {
	return ValueType(instruction >> 8)
}

func get_binop_dst_register(instruction uint64) uint16 {
	return uint16(instruction >> 16)
}

func get_binop_src1_register(instruction uint64) uint16 {
	return uint16(instruction >> 32)
}

func get_binop_src2_register(instruction uint64) uint16 {
	return uint16(instruction >> 48)
}

func Execute(p *Predicate) {
	m := new(Machine)
	m.Registers = make([]Register, p.RegisterFileSize)

	for index := p.InstructionPointer; index < len(p.Instructions); index++ {
		instruction := p.Instructions[index]
		switch get_op_code(instruction) {
		case Eq, Ne, Ge, Le, Gt, Lt,
			And, Or, Add, Sub, Mul, Div, Mod:
			exec_binop(instruction, p, m)
		}
	}
}
