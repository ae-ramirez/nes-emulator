package cpu

import (
	"fmt"
)

type AddressingMode int

const (
	Immediate AddressingMode = iota
	ZeroPage
	ZeroPage_X
	ZeroPage_Y
	Absolute
	Absolute_X
	Absolute_Y
	Indirect_X
	Indirect_Y
	NoneAddressing
)

type OpCode struct {
	code     uint8
	mnemonic string
	len      uint8
	cycles   uint8
	mode     AddressingMode
}

func newOpCode(code uint8, mnemonic string, len uint8, cycles uint8, mode AddressingMode) *OpCode {
	return &OpCode{
		code:     code,
		mnemonic: mnemonic,
		len:      len,
		cycles:   cycles,
		mode:     mode,
	}
}

func OpCodesMapFunc() func() map[uint8]*OpCode {
	var m map[uint8]*OpCode

	OpCodes := []*OpCode{
		newOpCode(0x69, "ADC", 2, 2, Immediate),
		newOpCode(0x65, "ADC", 2, 3, ZeroPage),
		newOpCode(0x75, "ADC", 2, 4, ZeroPage_X),
		newOpCode(0x6d, "ADC", 3, 4, Absolute),
		newOpCode(0x7d, "ADC", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x79, "ADC", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0x61, "ADC", 2, 6, Indirect_X),
		newOpCode(0x71, "ADC", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x29, "AND", 2, 2, Immediate),
		newOpCode(0x25, "AND", 2, 3, ZeroPage),
		newOpCode(0x35, "AND", 2, 4, ZeroPage_X),
		newOpCode(0x2d, "AND", 3, 4, Absolute),
		newOpCode(0x3d, "AND", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x39, "AND", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0x21, "AND", 2, 6, Indirect_X),
		newOpCode(0x31, "AND", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x0a, "ASL", 1, 2, NoneAddressing),
		newOpCode(0x06, "ASL", 2, 5, ZeroPage),
		newOpCode(0x16, "ASL", 2, 6, ZeroPage_X),
		newOpCode(0x0e, "ASL", 3, 6, Absolute),
		newOpCode(0x1e, "ASL", 3, 7, Absolute_X),

		newOpCode(0x90, "BCC", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0xb0, "BCS", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0xf0, "BEQ", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0x30, "BMI", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0xd0, "BNE", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0x10, "BPL", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0x50, "BVC", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),
		newOpCode(0x70, "BVS", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, Immediate),

		newOpCode(0x2c, "BIT", 3, 4, Absolute),
		newOpCode(0x24, "BIT", 2, 3, ZeroPage),

		newOpCode(0x00, "BRK", 1, 7, NoneAddressing),

		newOpCode(0x18, "CLC", 1, 2, NoneAddressing),
		newOpCode(0xd8, "CLD", 1, 2, NoneAddressing),
		newOpCode(0x58, "CLI", 1, 2, NoneAddressing),
		newOpCode(0xb8, "CLV", 1, 2, NoneAddressing),

		newOpCode(0xc9, "CMP", 2, 2, Immediate),
		newOpCode(0xc5, "CMP", 2, 3, ZeroPage),
		newOpCode(0xd5, "CMP", 2, 4, ZeroPage_X),
		newOpCode(0xcd, "CMP", 3, 4, Absolute),
		newOpCode(0xdd, "CMP", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0xd9, "CMP", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0xc1, "CMP", 2, 6, Indirect_X),
		newOpCode(0xd1, "CMP", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0xe0, "CPX", 2, 2, Immediate),
		newOpCode(0xe4, "CPX", 2, 3, ZeroPage),
		newOpCode(0xec, "CPX", 3, 4, Absolute),

		newOpCode(0xc0, "CPY", 2, 2, Immediate),
		newOpCode(0xc4, "CPY", 2, 3, ZeroPage),
		newOpCode(0xcc, "CPY", 3, 4, Absolute),

		newOpCode(0xc6, "DEC", 2, 5, ZeroPage),
		newOpCode(0xd6, "DEC", 2, 6, ZeroPage_X),
		newOpCode(0xce, "DEC", 3, 6, Absolute),
		newOpCode(0xde, "DEC", 3, 7, Absolute_X),

		newOpCode(0xca, "DEX", 1, 2, NoneAddressing),

		newOpCode(0x88, "DEY", 1, 2, NoneAddressing),

		newOpCode(0x49, "EOR", 2, 2, Immediate),
		newOpCode(0x45, "EOR", 2, 3, ZeroPage),
		newOpCode(0x55, "EOR", 2, 4, ZeroPage_X),
		newOpCode(0x4d, "EOR", 3, 4, Absolute),
		newOpCode(0x5d, "EOR", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x59, "EOR", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0x41, "EOR", 2, 6, Indirect_X),
		newOpCode(0x51, "EOR", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0xe6, "INC", 2, 5, ZeroPage),
		newOpCode(0xf6, "INC", 2, 6, ZeroPage_X),
		newOpCode(0xee, "INC", 3, 6, Absolute),
		newOpCode(0xfe, "INC", 3, 7, Absolute_X),

		newOpCode(0xe8, "INX", 1, 2, NoneAddressing),

		newOpCode(0xc8, "INY", 1, 2, NoneAddressing),

		newOpCode(0x4c, "JMP", 3, 3, Absolute),
		newOpCode(0x6c, "JMP", 3, 5, Absolute),

		newOpCode(0xa9, "LDA", 2, 2, Immediate),
		newOpCode(0xa5, "LDA", 2, 3, ZeroPage),
		newOpCode(0xb5, "LDA", 2, 4, ZeroPage_X),
		newOpCode(0xad, "LDA", 3, 4, Absolute),
		newOpCode(0xbd, "LDA", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0xb9, "LDA", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0xa1, "LDA", 2, 6, Indirect_X),
		newOpCode(0xb1, "LDA", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0xa2, "LDX", 2, 2, Immediate),
		newOpCode(0xa6, "LDX", 2, 3, ZeroPage),
		newOpCode(0xb6, "LDX", 2, 4, ZeroPage_Y),
		newOpCode(0xae, "LDX", 3, 4, Absolute),
		newOpCode(0xbe, "LDX", 3, 4 /* +1 if page crossed*/, Absolute_Y),

		newOpCode(0xa0, "LDY", 2, 2, Immediate),
		newOpCode(0xa4, "LDY", 2, 3, ZeroPage),
		newOpCode(0xb4, "LDY", 2, 4, ZeroPage_X),
		newOpCode(0xac, "LDY", 3, 4, Absolute),
		newOpCode(0xbc, "LDY", 3, 4 /* +1 if page crossed */, Absolute_X),

		newOpCode(0x4a, "LSR", 1, 2, NoneAddressing),
		newOpCode(0x46, "LSR", 2, 5, ZeroPage),
		newOpCode(0x56, "LSR", 2, 6, ZeroPage_X),
		newOpCode(0x4e, "LSR", 3, 6, Absolute),
		newOpCode(0x5e, "LSR", 3, 7, Absolute_X),

		newOpCode(0x09, "ORA", 2, 2, Immediate),
		newOpCode(0x05, "ORA", 2, 3, ZeroPage),
		newOpCode(0x15, "ORA", 2, 4, ZeroPage_X),
		newOpCode(0x0d, "ORA", 3, 4, Absolute),
		newOpCode(0x1d, "ORA", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x19, "ORA", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0x01, "ORA", 2, 6, Indirect_X),
		newOpCode(0x11, "ORA", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x2a, "ROL", 1, 2, NoneAddressing),
		newOpCode(0x26, "ROL", 2, 5, ZeroPage),
		newOpCode(0x36, "ROL", 2, 6, ZeroPage_X),
		newOpCode(0x2e, "ROL", 3, 6, Absolute),
		newOpCode(0x3e, "ROL", 3, 7, Absolute_X),

		newOpCode(0x6a, "ROR", 1, 2, NoneAddressing),
		newOpCode(0x66, "ROR", 2, 5, ZeroPage),
		newOpCode(0x76, "ROR", 2, 6, ZeroPage_X),
		newOpCode(0x6e, "ROR", 3, 6, Absolute),
		newOpCode(0x7e, "ROR", 3, 7, Absolute_X),

		newOpCode(0xe9, "SBC", 2, 2, Immediate),
		newOpCode(0xe5, "SBC", 2, 3, ZeroPage),
		newOpCode(0xf5, "SBC", 2, 4, ZeroPage_X),
		newOpCode(0xed, "SBC", 3, 4, Absolute),
		newOpCode(0xfd, "SBC", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0xf9, "SBC", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0xe1, "SBC", 2, 6, Indirect_X),
		newOpCode(0xf1, "SBC", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x38, "SEC", 1, 2, NoneAddressing),
		newOpCode(0xf8, "SED", 1, 2, NoneAddressing),
		newOpCode(0x78, "SEI", 1, 2, NoneAddressing),

		newOpCode(0x85, "STA", 2, 3, ZeroPage),
		newOpCode(0x95, "STA", 2, 4, ZeroPage_X),
		newOpCode(0x8d, "STA", 3, 4, Absolute),
		newOpCode(0x9d, "STA", 3, 5, Absolute_X),
		newOpCode(0x99, "STA", 3, 5, Absolute_Y),
		newOpCode(0x81, "STA", 2, 6, Indirect_X),
		newOpCode(0x91, "STA", 2, 6, Indirect_Y),

		newOpCode(0x86, "STX", 2, 3, ZeroPage),
		newOpCode(0x96, "STX", 2, 4, ZeroPage_Y),
		newOpCode(0x8e, "STX", 3, 4, Absolute),

		newOpCode(0x84, "STY", 2, 3, ZeroPage),
		newOpCode(0x94, "STY", 2, 4, ZeroPage_X),
		newOpCode(0x8c, "STY", 3, 4, Absolute),

		newOpCode(0xaa, "TAX", 1, 7, NoneAddressing),

		newOpCode(0xa8, "TAY", 1, 2, NoneAddressing),

		newOpCode(0x8a, "TXA", 1, 2, NoneAddressing),

		newOpCode(0x98, "TYA", 1, 2, NoneAddressing),
	}

	return func() map[uint8]*OpCode {
		if m != nil {
			return m
		}

		m := make(map[uint8]*OpCode)
		for _, opcode := range OpCodes {
			m[opcode.code] = opcode
		}
		return m
	}
}

var OpCodes = OpCodesMapFunc()

func (cpu *CPU) getOperandAddress(mode AddressingMode) uint16 {
	switch mode {
	case Immediate:
		return cpu.program_counter
	case ZeroPage:
		return uint16(cpu.mem_read(cpu.program_counter))
	case Absolute:
		return cpu.mem_read_u16(cpu.program_counter)
	case ZeroPage_X:
		pos := cpu.mem_read(cpu.program_counter)
		addr := uint16(pos) + uint16(cpu.register_x)
		return addr
	case ZeroPage_Y:
		pos := cpu.mem_read(cpu.program_counter)
		addr := uint16(pos) + uint16(cpu.register_y)
		return addr
	case Absolute_X:
		base := cpu.mem_read_u16(cpu.program_counter)
		addr := base + uint16(cpu.register_x)
		return addr
	case Absolute_Y:
		base := cpu.mem_read_u16(cpu.program_counter)
		addr := base + uint16(cpu.register_y)
		return addr
	case Indirect_X:
		base := cpu.mem_read(cpu.program_counter)
		ptr := base + cpu.register_x
		return cpu.mem_read_u16(uint16(ptr))
	case Indirect_Y:
		base := cpu.mem_read(cpu.program_counter)
		deref_base := cpu.mem_read_u16(uint16(base))
		return deref_base + uint16(cpu.register_y)
	default:
		panic(fmt.Errorf("mode %#v is not supported", mode))
	}
}

func (cpu *CPU) updateZeroAndNegativeFlags(result uint8) {
	cpu.setStatusFlag(ZeroFlag, result == 0)
	cpu.setStatusFlag(NegativeFlag, result>>7 == 1)
}

func (cpu *CPU) setStatusFlag(mask uint8, set bool) {
	if set {
		cpu.status |= mask
	} else {
		cpu.status &= ^mask
	}
}

func (cpu *CPU) isStatusFlagSet(mask uint8) bool {
	return cpu.status&mask != 0
}

func (cpu *CPU) isStatusFlagClear(mask uint8) bool {
	return cpu.status&mask == 0
}

func (cpu *CPU) adc(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)

	sum := uint16(cpu.register_a) + uint16(val)
	if cpu.status&CarryFlag != 0 {
		sum += 1
	}
	result := uint8(sum)

	setOverflowFlag := (cpu.register_a^result)&(val^result)&0x80 != 0

	cpu.setStatusFlag(CarryFlag, sum > 0xff)
	cpu.setStatusFlag(OverflowFlag, setOverflowFlag)

	cpu.register_a = result
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) and(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)

	cpu.register_a = cpu.register_a & val
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) asl(mode AddressingMode) {
	var val uint8

	if mode == NoneAddressing {
		val = cpu.register_a
		cpu.setStatusFlag(CarryFlag, val>>7 == 1)

		val = val << 1
		cpu.register_a = val
	} else {
		addr := cpu.getOperandAddress(mode)
		val = cpu.mem_read(addr)
		cpu.setStatusFlag(CarryFlag, val>>7 == 1)

		val = val << 1
		cpu.mem_write(addr, val)
	}

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) bit(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)

	cpu.setStatusFlag(NegativeFlag, val&0b1000_0000 != 0)
	cpu.setStatusFlag(OverflowFlag, val&0b0100_0000 != 0)
	cpu.setStatusFlag(ZeroFlag, val&cpu.register_a == 0)
}

func (cpu *CPU) branch(shouldBranch bool) {
	if shouldBranch {
		displacement := int8(cpu.mem_read(cpu.program_counter))
		cpu.program_counter += uint16(displacement)
	}
}

func (cpu *CPU) brk() {
	// TODO
}
func (cpu *CPU) clc() {
	cpu.setStatusFlag(CarryFlag, false)
}

func (cpu *CPU) cld() {
	cpu.setStatusFlag(DecimalModeFlag, false)
}

func (cpu *CPU) cli() {
	cpu.setStatusFlag(InterruptDisableFlag, false)
}

func (cpu *CPU) clv() {
	cpu.setStatusFlag(OverflowFlag, false)
}

func (cpu *CPU) cmp(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)

	cpu.setStatusFlag(CarryFlag, cpu.register_a >= val)
	cpu.setStatusFlag(ZeroFlag, cpu.register_a == val)
	cpu.setStatusFlag(NegativeFlag, (cpu.register_a-val)>>7 == 1)
}

func (cpu *CPU) cpx(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)

	cpu.setStatusFlag(CarryFlag, cpu.register_x >= val)
	cpu.setStatusFlag(ZeroFlag, cpu.register_x == val)
	cpu.setStatusFlag(NegativeFlag, (cpu.register_x-val)>>7 == 1)
}

func (cpu *CPU) cpy(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)

	cpu.setStatusFlag(CarryFlag, cpu.register_y >= val)
	cpu.setStatusFlag(ZeroFlag, cpu.register_y == val)
	cpu.setStatusFlag(NegativeFlag, (cpu.register_y-val)>>7 == 1)
}

func (cpu *CPU) dec(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr) - 1
	cpu.mem_write(addr, val)

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) dex() {
	cpu.register_x -= 1
	cpu.updateZeroAndNegativeFlags(cpu.register_x)
}

func (cpu *CPU) dey() {
	cpu.register_y -= 1
	cpu.updateZeroAndNegativeFlags(cpu.register_y)
}

func (cpu *CPU) eor(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)
	cpu.register_a ^= val

	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) inc(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr) + 1
	cpu.mem_write(addr, val)

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) inx() {
	cpu.register_x += 1
	cpu.updateZeroAndNegativeFlags(cpu.register_x)
}

func (cpu *CPU) iny() {
	cpu.register_y += 1
	cpu.updateZeroAndNegativeFlags(cpu.register_y)
}

func (cpu *CPU) jmp_absolute() {
	addr := cpu.mem_read_u16(cpu.program_counter)
	cpu.program_counter = addr
}

func (cpu *CPU) jmp_indirect() {
	var jump_addr uint16
	addr := cpu.mem_read_u16(cpu.program_counter)

	if addr&0xff == 0xff {
		// accounting for 6502 bug
		lo := uint16(cpu.mem_read(addr))
		hi := uint16(cpu.mem_read(addr & 0xff00))
		jump_addr = (hi << 8) | lo
	} else {
		jump_addr = cpu.mem_read_u16(addr)
	}

	cpu.program_counter = jump_addr
}

func (cpu *CPU) lda(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.register_a = cpu.mem_read(addr)
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) ldx(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.register_x = cpu.mem_read(addr)
	cpu.updateZeroAndNegativeFlags(cpu.register_x)
}

func (cpu *CPU) ldy(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.register_y = cpu.mem_read(addr)
	cpu.updateZeroAndNegativeFlags(cpu.register_y)
}

func (cpu *CPU) lsr(mode AddressingMode) {
	var val uint8

	if mode == NoneAddressing {
		val = cpu.register_a
		cpu.setStatusFlag(CarryFlag, val&0b0000_0001 != 0)
		val >>= 1
		cpu.register_a = val
	} else {
		addr := cpu.getOperandAddress(mode)
		val = cpu.mem_read(addr)
		cpu.setStatusFlag(CarryFlag, val&0b0000_0001 != 0)
		val >>= 1
		cpu.mem_write(addr, val)
	}

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) ora(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)
	cpu.register_a |= val

	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) rol(mode AddressingMode) {
	var val uint8
	var setCarryFlag bool

	if mode == NoneAddressing {
		setCarryFlag = cpu.register_a&0b1000_0000 != 0
		cpu.register_a <<= 1
		cpu.register_a |= cpu.status & CarryFlag
		val = cpu.register_a
	} else {
		addr := cpu.getOperandAddress(mode)
		val := cpu.mem_read(addr)
		setCarryFlag = val&0b1000_0000 != 0
		val <<= 1
		val |= cpu.status & CarryFlag
		cpu.mem_write(addr, val)
	}

	cpu.setStatusFlag(CarryFlag, setCarryFlag)
	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) ror(mode AddressingMode) {
	var val uint8
	var setCarryFlag bool

	if mode == NoneAddressing {
		setCarryFlag = cpu.register_a&0b0000_0001 != 0
		cpu.register_a >>= 1
		cpu.register_a |= (cpu.status & CarryFlag) << 7
		val = cpu.register_a
	} else {
		addr := cpu.getOperandAddress(mode)
		val = cpu.mem_read(addr)
		setCarryFlag = val&0b0000_0001 != 0
		val >>= 1
		val |= (cpu.status & CarryFlag) << 7
		cpu.mem_write(addr, val)
	}

	cpu.setStatusFlag(CarryFlag, setCarryFlag)
	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) sbc(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	val := cpu.mem_read(addr)
	val = ^val + 1

	sub := uint16(cpu.register_a) + uint16(val) - 1
	if cpu.status&CarryFlag != 0 {
		sub += 1
	}
	result := uint8(sub)

	setOverflowFlag := (cpu.register_a^result)&(val^result)&0x80 != 0

	cpu.setStatusFlag(CarryFlag, sub > 0xff)
	cpu.setStatusFlag(OverflowFlag, setOverflowFlag)

	cpu.register_a = result
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) sec() {
	cpu.setStatusFlag(CarryFlag, true)
}

func (cpu *CPU) sed() {
	cpu.setStatusFlag(DecimalModeFlag, true)
}

func (cpu *CPU) sei() {
	cpu.setStatusFlag(InterruptDisableFlag, true)
}

func (cpu *CPU) sta(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.mem_write(addr, cpu.register_a)
}

func (cpu *CPU) stx(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.mem_write(addr, cpu.register_x)
}

func (cpu *CPU) sty(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.mem_write(addr, cpu.register_y)
}

func (cpu *CPU) tax() {
	cpu.register_x = cpu.register_a
	cpu.updateZeroAndNegativeFlags(cpu.register_x)
}

func (cpu *CPU) tay() {
	cpu.register_y = cpu.register_a
	cpu.updateZeroAndNegativeFlags(cpu.register_y)
}

func (cpu *CPU) txa() {
	cpu.register_a = cpu.register_x
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) tya() {
	cpu.register_a = cpu.register_y
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}
