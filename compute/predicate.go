package compute

import (
	inf "speter.net/go/exp/math/dec/inf"
	"fmt"
	"time"
)

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
	TinyInteger
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
	Register   uint16
	OffsetData uint32
}

type BranchOp struct {
	Cmd      Opcode
	Register uint16
	Offset   uint32
}

type InstructionEncoder interface {
	EncodeBinOp(code Opcode, opr_type ValueType, src1 uint16, src2 uint16, dst uint16)
	EncodeLiteralLoadInt8(opr_type ValueType, dst uint16, value int8)
	EncodeLiteralLoadInt16(opr_type ValueType, dst uint16, value int16)
	EncodeLiteralLoadInt32(opr_type ValueType, dst uint16, value int32)
	EncodeLiteralLoadInt64(opr_type ValueType, dst uint16, value int64)
	EncodeLiteralLoadDateTime(opr_type ValueType, dst uint16, value *time.Time)
	EncodeLiteralLoadDecimal(opr_type ValueType, dst uint16, value *inf.Dec)
}

type Instruction uint64

type Predicate struct {
	Instructions       []Instruction
	InstructionPointer int
	RegisterFileSize   int
	Data               []byte
	DataLength         int
}

type Register struct {
	Type    ValueType
	Written bool
	Value   interface{}
}

type Machine struct {
	Registers []Register
}

func get_op_code(instruction Instruction) Opcode {
	return Opcode(instruction)
}

func get_op_type(instruction Instruction) ValueType {
	return ValueType(instruction >> 8)
}

func get_binop_dst_register(instruction Instruction) uint16 {
	return uint16(instruction >> 16)
}

func get_binop_src1_register(instruction Instruction) uint16 {
	return uint16(instruction >> 32)
}

func get_binop_src2_register(instruction Instruction) uint16 {
	return uint16(instruction >> 48)
}

func get_litop_register(instruction Instruction) uint16 {
	return uint16(instruction >> 16)
}

func get_litop_data_as_int8(instruction Instruction) int8 {
	return int8(instruction >> 32)
}

func get_litop_data_as_int16(instruction Instruction) int16 {
	return int16(instruction >> 32)
}

func get_litop_data_as_int32(instruction Instruction) int32 {
	return int32(instruction >> 32)
}

func get_litop_data_as_int64(instruction Instruction) int64 {
	return int64(instruction >> 32)
}

func get_litop_data_offset(instruction Instruction) uint32 {
	return uint32(instruction >> 32)
}

func set_op_code(instruction Instruction, opcode Opcode) Instruction {
	return instruction | Instruction(opcode)
}

func set_op_type(instruction Instruction, op_type ValueType) Instruction {
	return instruction | Instruction(op_type)<<8
}

func set_binop_dst_register(instruction Instruction, index uint16) Instruction {
	return instruction | Instruction(index)<<16
}

func set_binop_src1_register(instruction Instruction, index uint16) Instruction {
	return instruction | Instruction(index)<<32
}

func set_binop_src2_register(instruction Instruction, index uint16) Instruction {
	return instruction | Instruction(index)<<48
}

func set_litop_register(instruction Instruction, register uint16) Instruction {
	return instruction | Instruction(register)<<16
}

func set_litop_data_offset(instruction Instruction, offset uint32) Instruction {
	return instruction | Instruction(offset)<<32
}

func set_litop_data_as_int8(instruction Instruction, value int8) Instruction {
	return instruction | Instruction(value)<<32
}

func set_litop_data_as_int16(instruction Instruction, value int16) Instruction {
	return instruction | Instruction(value)<<32
}

func set_litop_data_as_int32(instruction Instruction, value int32) Instruction {
	return instruction | Instruction(value)<<32
}

func set_litop_data_as_int64(instruction Instruction, value int64) Instruction {
	return instruction | Instruction(value)<<32
}

func grow_instruction_array(p *Predicate) int {
	index := p.InstructionPointer
	// Grow instruction array if needed.
	if index >= cap(p.Instructions) {
		new_instructions := make([]Instruction, cap(p.Instructions)*2)
		copy(new_instructions, p.Instructions)
		p.Instructions = new_instructions
	}

	return index
}

func grow_data_buffer(p *Predicate) int {
	offset := p.DataLength
	// Grow data buffer if needed.
	if offset >= cap(p.Data) {
		new_data := make([]byte, cap(p.Data)*2)
		copy(new_data, p.Data)
		p.Data = new_data
	}

	return offset
}

func prepare_literal_indirect_instruction(p *Predicate, opr_type ValueType, dst uint16) (int, int, Instruction) {
	index := grow_instruction_array(p)
	offset := grow_data_buffer(p)
	instruction := Instruction(LoadLiteralIndirect) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
		Instruction(offset)<<32

	return index, offset, instruction
}

func write_literal_indirect_instruction(p *Predicate, instruction Instruction, index int, data []byte, offset int) {
	p.Instructions[index] = instruction
	p.InstructionPointer += 1
	copy(p.Data[offset:], data)
	p.DataLength += len(data)
}

func (p *Predicate) EncodeBinOp(code Opcode, opr_type ValueType, src1 uint16, src2 uint16, dst uint16) {
	index := grow_instruction_array(p)
	instruction := Instruction(code) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
		Instruction(src1)<<32 | Instruction(src2)<<48

	p.Instructions[index] = instruction
	p.InstructionPointer += 1
}

func (p *Predicate) EncodeLiteralLoadInt8(opr_type ValueType, dst uint16, value int8) {
	index := grow_instruction_array(p)
	instruction := Instruction(LoadLiteral) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
		Instruction(value)<<32
	p.Instructions[index] = instruction
	p.InstructionPointer += 1
}

func (p *Predicate) EncodeLiteralLoadInt16(opr_type ValueType, dst uint16, value int16) {
	index := grow_instruction_array(p)
	instruction := Instruction(LoadLiteral) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
		Instruction(value)<<32
	p.Instructions[index] = instruction
	p.InstructionPointer += 1
}

func (p *Predicate) EncodeLiteralLoadInt32(opr_type ValueType, dst uint16, value int32) {
	index := grow_instruction_array(p)
	instruction := Instruction(LoadLiteral) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
		Instruction(value)<<32
	p.Instructions[index] = instruction
	p.InstructionPointer += 1
}

func (p *Predicate) EncodeLiteralLoadInt64(opr_type ValueType, dst uint16, value int64) {
	index := grow_instruction_array(p)
	if value >= -2147483648 && value <= 2147483647 {
		instruction := Instruction(LoadLiteral) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
			Instruction(value)<<32
		p.Instructions[index] = instruction
	} else {
		offset := grow_data_buffer(p)
		instruction := Instruction(LoadLiteralIndirect) | Instruction(opr_type)<<8 | Instruction(dst)<<16 |
			Instruction(offset)<<32
		p.Instructions[index] = instruction
		p.Data[offset] = uint8(value)
		p.Data[offset+1] = uint8(value >> 8)
		p.Data[offset+2] = uint8(value >> 16)
		p.Data[offset+3] = uint8(value >> 24)
		p.Data[offset+4] = uint8(value >> 32)
		p.Data[offset+5] = uint8(value >> 40)
		p.Data[offset+6] = uint8(value >> 48)
		p.Data[offset+7] = uint8(value >> 56)
		p.DataLength += 8
	}

	p.InstructionPointer += 1
}

func (p *Predicate) EncodeLiteralLoadDateTime(opr_type ValueType, dst uint16, value *time.Time) {
	index, offset, instruction := prepare_literal_indirect_instruction(p, opr_type, dst)
	data, err := value.GobEncode()
	if err != nil {
		panic(fmt.Sprintf("Unable to encode DateTime: %v", err))
	}
	write_literal_indirect_instruction(p, instruction, index, data, offset)
}

func (p *Predicate) EncodeLiteralLoadDecimal(opr_type ValueType, dst uint16, value *inf.Dec) {
	index, offset, instruction := prepare_literal_indirect_instruction(p, opr_type, dst)
	data, err := value.GobEncode()
	if err != nil {
		panic(fmt.Sprintf("Unable to encode Decimal: %v", err))
	}
	write_literal_indirect_instruction(p, instruction, index, data, offset)
}

func Execute(p *Predicate) {
	m := new(Machine)
	m.Registers = make([]Register, p.RegisterFileSize)

	for index := p.InstructionPointer; index < len(p.Instructions); index++ {
		instruction := p.Instructions[index]
		switch get_op_code(instruction) {
		case Eq, Ne, Ge, Le, Gt, Lt,
			And, Or, Add, Sub, Mul, Div, Mod:
			exec_binop(instruction, m)
		}
	}
}
