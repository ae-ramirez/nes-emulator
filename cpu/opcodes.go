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

		newOpCode(0x24, "BIT", 2, 3, ZeroPage),
		newOpCode(0x2c, "BIT", 3, 4, Absolute),

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

		newOpCode(0x20, "JSR", 3, 6, Absolute),

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

		newOpCode(0xea, "NOP", 1, 2, NoneAddressing),

		newOpCode(0x09, "ORA", 2, 2, Immediate),
		newOpCode(0x05, "ORA", 2, 3, ZeroPage),
		newOpCode(0x15, "ORA", 2, 4, ZeroPage_X),
		newOpCode(0x0d, "ORA", 3, 4, Absolute),
		newOpCode(0x1d, "ORA", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x19, "ORA", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0x01, "ORA", 2, 6, Indirect_X),
		newOpCode(0x11, "ORA", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x48, "PHA", 1, 3, NoneAddressing),
		newOpCode(0x08, "PHP", 1, 3, NoneAddressing),
		newOpCode(0x68, "PLA", 1, 4, NoneAddressing),
		newOpCode(0x28, "PLP", 1, 4, NoneAddressing),

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

		newOpCode(0x40, "RTI", 1, 6, NoneAddressing),

		newOpCode(0x60, "RTS", 1, 6, NoneAddressing),

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

		newOpCode(0xaa, "TAX", 1, 2, NoneAddressing),
		newOpCode(0xa8, "TAY", 1, 2, NoneAddressing),
		newOpCode(0xba, "TSX", 1, 2, NoneAddressing),
		newOpCode(0x8a, "TXA", 1, 2, NoneAddressing),
		newOpCode(0x9a, "TXS", 1, 2, NoneAddressing),
		newOpCode(0x98, "TYA", 1, 2, NoneAddressing),

		// unofficial opcodes
		newOpCode(0x1a, "*NOP", 1, 2, NoneAddressing),
		newOpCode(0x3a, "*NOP", 1, 2, NoneAddressing),
		newOpCode(0x5a, "*NOP", 1, 2, NoneAddressing),
		newOpCode(0x7a, "*NOP", 1, 2, NoneAddressing),
		newOpCode(0xda, "*NOP", 1, 2, NoneAddressing),
		newOpCode(0xfa, "*NOP", 1, 2, NoneAddressing),
		newOpCode(0x04, "*NOP", 2, 3, ZeroPage),
		newOpCode(0x14, "*NOP", 2, 4, ZeroPage_X),
		newOpCode(0x34, "*NOP", 2, 4, ZeroPage_X),
		newOpCode(0x44, "*NOP", 2, 3, ZeroPage),
		newOpCode(0x54, "*NOP", 2, 4, ZeroPage_X),
		newOpCode(0x64, "*NOP", 2, 3, ZeroPage),
		newOpCode(0x74, "*NOP", 2, 4, ZeroPage_X),
		newOpCode(0x80, "*NOP", 2, 2, Immediate),
		newOpCode(0x82, "*NOP", 2, 2, Immediate),
		newOpCode(0x89, "*NOP", 2, 2, Immediate),
		newOpCode(0xc2, "*NOP", 2, 2, Immediate),
		newOpCode(0xd4, "*NOP", 2, 4, ZeroPage_X),
		newOpCode(0xe2, "*NOP", 2, 2, Immediate),
		newOpCode(0xf4, "*NOP", 2, 4, ZeroPage_X),
		newOpCode(0x0c, "*NOP", 3, 4, Absolute),
		newOpCode(0x1c, "*NOP", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x3c, "*NOP", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x5c, "*NOP", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0x7c, "*NOP", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0xdc, "*NOP", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0xfc, "*NOP", 3, 4 /* +1 if page crossed */, Absolute_X),

		newOpCode(0xa7, "*LAX", 2, 3, ZeroPage),
		newOpCode(0xb7, "*LAX", 2, 4, ZeroPage_Y),
		newOpCode(0xaf, "*LAX", 3, 4, Absolute),
		newOpCode(0xbf, "*LAX", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0xa3, "*LAX", 2, 6, Indirect_X),
		newOpCode(0xb3, "*LAX", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x87, "*SAX", 2, 3, ZeroPage),
		newOpCode(0x97, "*SAX", 2, 4, ZeroPage_Y),
		newOpCode(0x83, "*SAX", 2, 6, Indirect_X),
		newOpCode(0x8F, "*SAX", 3, 4, Absolute),

		newOpCode(0xeb, "*SBC", 2, 2, Immediate),

		newOpCode(0xc7, "*DCP", 2, 5, ZeroPage),
		newOpCode(0xd7, "*DCP", 2, 6, ZeroPage_X),
		newOpCode(0xcf, "*DCP", 3, 6, Absolute),
		newOpCode(0xdf, "*DCP", 3, 7, Absolute_X),
		newOpCode(0xdb, "*DCP", 3, 7, Absolute_Y),
		newOpCode(0xc3, "*DCP", 2, 8, Indirect_X),
		newOpCode(0xd3, "*DCP", 2, 8, Indirect_Y),

		newOpCode(0xe7, "*ISB", 2, 5, ZeroPage),
		newOpCode(0xf7, "*ISB", 2, 6, ZeroPage_X),
		newOpCode(0xef, "*ISB", 3, 6, Absolute),
		newOpCode(0xff, "*ISB", 3, 7, Absolute_X),
		newOpCode(0xfb, "*ISB", 3, 7, Absolute_Y),
		newOpCode(0xe3, "*ISB", 2, 8, Indirect_X),
		newOpCode(0xf3, "*ISB", 2, 8, Indirect_Y),

		newOpCode(0x07, "*SLO", 2, 5, ZeroPage),
		newOpCode(0x17, "*SLO", 2, 6, ZeroPage_X),
		newOpCode(0x0f, "*SLO", 3, 6, Absolute),
		newOpCode(0x1f, "*SLO", 3, 7, Absolute_X),
		newOpCode(0x1b, "*SLO", 3, 7, Absolute_Y),
		newOpCode(0x03, "*SLO", 2, 8, Indirect_X),
		newOpCode(0x13, "*SLO", 2, 8, Indirect_Y),

		newOpCode(0x27, "*RLA", 2, 5, ZeroPage),
		newOpCode(0x37, "*RLA", 2, 6, ZeroPage_X),
		newOpCode(0x2f, "*RLA", 3, 6, Absolute),
		newOpCode(0x3f, "*RLA", 3, 7, Absolute_X),
		newOpCode(0x3b, "*RLA", 3, 7, Absolute_Y),
		newOpCode(0x23, "*RLA", 2, 8, Indirect_X),
		newOpCode(0x33, "*RLA", 2, 8, Indirect_Y),

		newOpCode(0x47, "*SRE", 2, 5, ZeroPage),
		newOpCode(0x57, "*SRE", 2, 6, ZeroPage_X),
		newOpCode(0x4f, "*SRE", 3, 6, Absolute),
		newOpCode(0x5f, "*SRE", 3, 7, Absolute_X),
		newOpCode(0x5b, "*SRE", 3, 7, Absolute_Y),
		newOpCode(0x43, "*SRE", 2, 8, Indirect_X),
		newOpCode(0x53, "*SRE", 2, 8, Indirect_Y),

		newOpCode(0x67, "*RRA", 2, 5, ZeroPage),
		newOpCode(0x77, "*RRA", 2, 6, ZeroPage_X),
		newOpCode(0x6f, "*RRA", 3, 6, Absolute),
		newOpCode(0x7f, "*RRA", 3, 7, Absolute_X),
		newOpCode(0x7b, "*RRA", 3, 7, Absolute_Y),
		newOpCode(0x63, "*RRA", 2, 8, Indirect_X),
		newOpCode(0x73, "*RRA", 2, 8, Indirect_Y),
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

// getOperandAddress returns an adress (uint16) calculated according to the
// addressing mode specified. The bool value returns whether two different
// memory pages would have been accessed when calculating the address.
func (cpu *CPU) getOperandAddress(mode AddressingMode) (uint16, bool) {
	switch mode {
	case Immediate:
		return cpu.programCounter, false
	case ZeroPage:
		return uint16(cpu.MemRead(cpu.programCounter)), false
	case Absolute:
		return cpu.MemRead_u16(cpu.programCounter), false
	case ZeroPage_X:
		pos := cpu.MemRead(cpu.programCounter)
		addr := uint16(pos + cpu.registerX)
		return addr, false
	case ZeroPage_Y:
		pos := cpu.MemRead(cpu.programCounter)
		addr := uint16(pos + cpu.registerY)
		return addr, false
	case Absolute_X:
		base := cpu.MemRead_u16(cpu.programCounter)
		addr := base + uint16(cpu.registerX)
		return addr, base>>8 != addr>>8
	case Absolute_Y:
		base := cpu.MemRead_u16(cpu.programCounter)
		addr := base + uint16(cpu.registerY)
		if cpu.programCounter == 0xdf60 {
			fmt.Printf("base: %d\naddr: %d\n", base, addr)
		}
		return addr, base>>8 != addr>>8
	case Indirect_X:
		base := cpu.MemRead(cpu.programCounter)
		ptr := base + cpu.registerX
		lo := cpu.MemRead(uint16(ptr))
		ptr += 1
		hi := cpu.MemRead(uint16(ptr))
		return (uint16(hi))<<8 | uint16(lo), false
	case Indirect_Y:
		base := cpu.MemRead(cpu.programCounter)
		lo := uint16(cpu.MemRead(uint16(base)))
		hi := uint16(cpu.MemRead(uint16(base + 1)))
		deref_base := (hi << 8) | lo
		addr := deref_base + uint16(cpu.registerY)
		return deref_base + uint16(cpu.registerY), deref_base>>8 != addr>>8
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
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)

	sum := uint16(cpu.registerA) + uint16(val)
	if cpu.status&CarryFlag != 0 {
		sum += 1
	}
	result := uint8(sum)

	setOverflowFlag := (cpu.registerA^result)&(val^result)&0x80 != 0

	cpu.setStatusFlag(CarryFlag, sum > 0xff)
	cpu.setStatusFlag(OverflowFlag, setOverflowFlag)

	cpu.registerA = result
	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

func (cpu *CPU) and(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)

	cpu.registerA = cpu.registerA & val
	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

func (cpu *CPU) asl(mode AddressingMode) {
	var val uint8

	if mode == NoneAddressing {
		val = cpu.registerA
		cpu.setStatusFlag(CarryFlag, val>>7 == 1)

		val = val << 1
		cpu.registerA = val
	} else {
		addr, _ := cpu.getOperandAddress(mode)
		val = cpu.MemRead(addr)
		cpu.setStatusFlag(CarryFlag, val>>7 == 1)

		val = val << 1
		cpu.MemWrite(addr, val)
	}

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) bit(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)

	cpu.setStatusFlag(NegativeFlag, val&0b1000_0000 != 0)
	cpu.setStatusFlag(OverflowFlag, val&0b0100_0000 != 0)
	cpu.setStatusFlag(ZeroFlag, val&cpu.registerA == 0)
}

func (cpu *CPU) branch(shouldBranch bool) uint8 {
	if !shouldBranch {
		return 0
	}

	oldProgramCounter := cpu.programCounter + 2
	displacement := int8(cpu.MemRead(cpu.programCounter))
	cpu.programCounter += uint16(displacement) + 1

	if oldProgramCounter>>8 == cpu.programCounter>>8 {
		return 1
	} else {
		return 2
	}
}

func (cpu *CPU) brk() {
	cpu.stackPush_u16(cpu.programCounter)
	cpu.stackPush(cpu.status | 0b0011_0000)

	cpu.programCounter = cpu.MemRead_u16(0xfffe)
	cpu.setStatusFlag(BreakCommandFlag, true)
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
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)

	cpu.setStatusFlag(CarryFlag, cpu.registerA >= val)
	cpu.setStatusFlag(ZeroFlag, cpu.registerA == val)
	cpu.setStatusFlag(NegativeFlag, (cpu.registerA-val)>>7 == 1)
}

func (cpu *CPU) cpx(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)

	cpu.setStatusFlag(CarryFlag, cpu.registerX >= val)
	cpu.setStatusFlag(ZeroFlag, cpu.registerX == val)
	cpu.setStatusFlag(NegativeFlag, (cpu.registerX-val)>>7 == 1)
}

func (cpu *CPU) cpy(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)

	cpu.setStatusFlag(CarryFlag, cpu.registerY >= val)
	cpu.setStatusFlag(ZeroFlag, cpu.registerY == val)
	cpu.setStatusFlag(NegativeFlag, (cpu.registerY-val)>>7 == 1)
}

func (cpu *CPU) dec(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr) - 1
	cpu.MemWrite(addr, val)

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) dex() {
	cpu.registerX -= 1
	cpu.updateZeroAndNegativeFlags(cpu.registerX)
}

func (cpu *CPU) dey() {
	cpu.registerY -= 1
	cpu.updateZeroAndNegativeFlags(cpu.registerY)
}

func (cpu *CPU) eor(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)
	cpu.registerA ^= val

	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

func (cpu *CPU) inc(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr) + 1
	cpu.MemWrite(addr, val)

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) inx() {
	cpu.registerX += 1
	cpu.updateZeroAndNegativeFlags(cpu.registerX)
}

func (cpu *CPU) iny() {
	cpu.registerY += 1
	cpu.updateZeroAndNegativeFlags(cpu.registerY)
}

func (cpu *CPU) jmp_absolute() {
	addr := cpu.MemRead_u16(cpu.programCounter)
	cpu.programCounter = addr
}

func (cpu *CPU) jmp_indirect() {
	addr := cpu.MemRead_u16(cpu.programCounter)
	jumpAddr := cpu.getJmpIndirectAdress(addr)
	cpu.programCounter = jumpAddr
}

func (cpu *CPU) getJmpIndirectAdress(addr uint16) uint16 {
	if addr&0xff == 0xff {
		// accounting for 6502 bug
		lo := uint16(cpu.MemRead(addr))
		hi := uint16(cpu.MemRead(addr & 0xff00))
		return (hi << 8) | lo
	} else {
		return cpu.MemRead_u16(addr)
	}
}

func (cpu *CPU) jsr(mode AddressingMode) {
	addr := cpu.MemRead_u16(cpu.programCounter)
	cpu.stackPush_u16(cpu.programCounter + 2 - 1)
	cpu.programCounter = addr
}

func (cpu *CPU) lda(mode AddressingMode) uint8 {
	addr, pageCrossed := cpu.getOperandAddress(mode)
	cpu.registerA = cpu.MemRead(addr)
	cpu.updateZeroAndNegativeFlags(cpu.registerA)

	if (mode == Absolute_X || mode == Absolute_Y || mode == Indirect_Y) &&
		pageCrossed {
		return 1
	} else {
		return 0
	}
}

func (cpu *CPU) ldx(mode AddressingMode) uint8 {
	addr, pageCrossed := cpu.getOperandAddress(mode)
	cpu.registerX = cpu.MemRead(addr)
	cpu.updateZeroAndNegativeFlags(cpu.registerX)

	if mode == Absolute_Y && pageCrossed {
		return 1
	} else {
		return 0
	}
}

func (cpu *CPU) ldy(mode AddressingMode) uint8 {
	addr, pageCrossed := cpu.getOperandAddress(mode)
	cpu.registerY = cpu.MemRead(addr)
	cpu.updateZeroAndNegativeFlags(cpu.registerY)

	if mode == Absolute_X && pageCrossed {
		return 1
	} else {
		return 0
	}
}

func (cpu *CPU) lsr(mode AddressingMode) {
	var val uint8

	if mode == NoneAddressing {
		val = cpu.registerA
		cpu.setStatusFlag(CarryFlag, val&0b0000_0001 != 0)
		val >>= 1
		cpu.registerA = val
	} else {
		addr, _ := cpu.getOperandAddress(mode)
		val = cpu.MemRead(addr)
		cpu.setStatusFlag(CarryFlag, val&0b0000_0001 != 0)
		val >>= 1
		cpu.MemWrite(addr, val)
	}

	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) ora(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)
	cpu.registerA |= val

	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

func (cpu *CPU) pha() {
	cpu.stackPush(cpu.registerA)
}

func (cpu *CPU) php() {
	cpu.stackPush(cpu.status | 0b0011_0000)
}

func (cpu *CPU) pla() {
	cpu.registerA = cpu.stackPop()
	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

func (cpu *CPU) plp() {
	cpu.status = (cpu.stackPop() | 0b0010_0000) & 0b1110_1111
}

func (cpu *CPU) rol(mode AddressingMode) {
	var val uint8
	var setCarryFlag bool

	if mode == NoneAddressing {
		setCarryFlag = cpu.registerA&0b1000_0000 != 0
		cpu.registerA <<= 1
		cpu.registerA |= cpu.status & CarryFlag
		val = cpu.registerA
	} else {
		addr, _ := cpu.getOperandAddress(mode)
		val = cpu.MemRead(addr)
		setCarryFlag = val&0b1000_0000 != 0
		val <<= 1
		val |= cpu.status & CarryFlag
		cpu.MemWrite(addr, val)
	}

	cpu.setStatusFlag(CarryFlag, setCarryFlag)
	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) ror(mode AddressingMode) {
	var val uint8
	var setCarryFlag bool

	if mode == NoneAddressing {
		setCarryFlag = cpu.registerA&0b0000_0001 != 0
		cpu.registerA >>= 1
		cpu.registerA |= (cpu.status & CarryFlag) << 7
		val = cpu.registerA
	} else {
		addr, _ := cpu.getOperandAddress(mode)
		val = cpu.MemRead(addr)
		setCarryFlag = val&0b0000_0001 != 0
		val >>= 1
		val |= (cpu.status & CarryFlag) << 7
		cpu.MemWrite(addr, val)
	}

	cpu.setStatusFlag(CarryFlag, setCarryFlag)
	cpu.updateZeroAndNegativeFlags(val)
}

func (cpu *CPU) rti() {
	cpu.status = cpu.stackPop() | 0b0010_0000
	cpu.programCounter = cpu.stackPop_u16()
}

func (cpu *CPU) rts() {
	cpu.programCounter = cpu.stackPop_u16() + 1
}

func (cpu *CPU) sbc(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.MemRead(addr)
	val = ^val

	sub := uint16(cpu.registerA) + uint16(val)
	if cpu.status&CarryFlag != 0 {
		sub += 1
	}
	result := uint8(sub)

	setOverflowFlag := (cpu.registerA^result)&(val^result)&0x80 != 0

	cpu.setStatusFlag(CarryFlag, sub > 0xff)
	cpu.setStatusFlag(OverflowFlag, setOverflowFlag)

	cpu.registerA = result
	cpu.updateZeroAndNegativeFlags(cpu.registerA)
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
	addr, _ := cpu.getOperandAddress(mode)
	cpu.MemWrite(addr, cpu.registerA)
}

func (cpu *CPU) stx(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	cpu.MemWrite(addr, cpu.registerX)
}

func (cpu *CPU) sty(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	cpu.MemWrite(addr, cpu.registerY)
}

func (cpu *CPU) tax() {
	cpu.registerX = cpu.registerA
	cpu.updateZeroAndNegativeFlags(cpu.registerX)
}

func (cpu *CPU) tay() {
	cpu.registerY = cpu.registerA
	cpu.updateZeroAndNegativeFlags(cpu.registerY)
}

func (cpu *CPU) tsx() {
	cpu.registerX = cpu.stackPointer
	cpu.updateZeroAndNegativeFlags(cpu.registerX)
}

func (cpu *CPU) txa() {
	cpu.registerA = cpu.registerX
	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

func (cpu *CPU) txs() {
	cpu.stackPointer = cpu.registerX
}

func (cpu *CPU) tya() {
	cpu.registerA = cpu.registerY
	cpu.updateZeroAndNegativeFlags(cpu.registerA)
}

// unofficial opcodes

func (cpu *CPU) sax(mode AddressingMode) {
	addr, _ := cpu.getOperandAddress(mode)
	val := cpu.registerA & cpu.registerX

	cpu.MemWrite(addr, val)
}

func (cpu *CPU) dcp(mode AddressingMode) {
	cpu.dec(mode)
	cpu.cmp(mode)
}
