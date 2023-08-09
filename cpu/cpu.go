package cpu

import (
	"al/nes-emulator/bus"
	"al/nes-emulator/rom"
	"fmt"
)

const (
	CarryFlag            uint8 = 0b0000_0001
	ZeroFlag             uint8 = 0b0000_0010
	InterruptDisableFlag uint8 = 0b0000_0100
	DecimalModeFlag      uint8 = 0b0000_1000
	BreakCommandFlag     uint8 = 0b0001_0000
	OverflowFlag         uint8 = 0b0100_0000
	NegativeFlag         uint8 = 0b1000_0000
)

type CPU struct {
	registerA      uint8
	registerX      uint8
	registerY      uint8
	status         uint8
	programCounter uint16
	stackPointer   uint8
	opcodes        func() map[uint8]*OpCode
	Bus            bus.Bus
}

const stackBasePosition uint16 = 0x0100
const stackReset = 0xff

func (cpu *CPU) MemRead(addr uint16) uint8 {
	return cpu.Bus.MemRead(addr)
}

func (cpu *CPU) MemRead_u16(pos uint16) uint16 {
	return cpu.Bus.MemRead_u16(pos)
}

func (cpu *CPU) MemWrite(addr uint16, data uint8) {
	cpu.Bus.MemWrite(addr, data)
}

func (cpu *CPU) MemWrite_u16(pos uint16, data uint16) {
	cpu.Bus.MemWrite_u16(pos, data)
}

func (cpu *CPU) stackPush(data uint8) {
	addr := stackBasePosition + uint16(cpu.stackPointer)
	cpu.MemWrite(addr, data)
	cpu.stackPointer -= 1
}

func (cpu *CPU) stackPush_u16(data uint16) {
	hi := data >> 8
	lo := data & 0xff
	cpu.stackPush(uint8(hi))
	cpu.stackPush(uint8(lo))
}

func (cpu *CPU) stackPop() uint8 {
	cpu.stackPointer += 1
	addr := stackBasePosition + uint16(cpu.stackPointer)
	data := cpu.MemRead(addr)
	return data
}

func (cpu *CPU) stackPop_u16() uint16 {
	lo := uint16(cpu.stackPop())
	hi := uint16(cpu.stackPop())
	return (hi << 8) | lo
}

func (cpu *CPU) Reset() {
	cpu.registerA = 0
	cpu.registerX = 0
	cpu.registerY = 0
	cpu.status = 0b100100
	cpu.stackPointer = stackReset
	cpu.opcodes = OpCodes

	cpu.programCounter = cpu.MemRead_u16(0xFFFC)
}

func (cpu *CPU) LoadAndRun(program []uint8) {
	cpu.Load(program)
	cpu.Reset()
	cpu.Run()
}

func (cpu *CPU) LoadIntoLocation(program []uint8, location uint16) {
	cpu.MemWrite_u16(0xFFFC, location)
	for _, data := range program {
		cpu.Bus.MemWrite(location, data)
		location += 1
	}
}

func (cpu *CPU) Load(program []uint8) {
	rom := &rom.Rom{}
	rom.InitMemory()
	cpu.Bus.SetRom(rom)

	location := uint16(0xfff)
	cpu.MemWrite_u16(0xFFFC, location)
	for _, data := range program {
		cpu.Bus.MemWrite(location, data)
		location += 1
	}
}

func (cpu *CPU) Run() {
	cpu.RunWithCallback(func(*CPU) {})
}

func (cpu *CPU) RunWithCallback(callback func(*CPU)) {
	opcodes := cpu.opcodes()
	var curr_program_counter uint16
	for {
		callback(cpu)

		code := cpu.MemRead(cpu.programCounter)
		cpu.programCounter += 1
		curr_program_counter = cpu.programCounter

		opcode := opcodes[code]

		switch code {
		case 0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71:
			cpu.adc(opcode.mode)
		case 0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31:
			cpu.and(opcode.mode)
		case 0x0a, 0x06, 0x16, 0x0e, 0x1e:
			cpu.asl(opcode.mode)
		case 0x90:
			// bcc
			cpu.branch(cpu.isStatusFlagClear(CarryFlag))
		case 0xb0:
			// bcs
			cpu.branch(cpu.isStatusFlagSet(CarryFlag))
		case 0xf0:
			// beq
			cpu.branch(cpu.isStatusFlagSet(ZeroFlag))
		case 0x24, 0x2c:
			cpu.bit(opcode.mode)
		case 0x30:
			// bmi
			cpu.branch(cpu.isStatusFlagSet(NegativeFlag))
		case 0xd0:
			// bne
			cpu.branch(cpu.isStatusFlagClear(ZeroFlag))
		case 0x10:
			// bpl
			cpu.branch(cpu.isStatusFlagClear(NegativeFlag))
		case 0x50:
			// bvc
			cpu.branch(cpu.isStatusFlagClear(OverflowFlag))
		case 0x70:
			// bvs
			cpu.branch(cpu.isStatusFlagSet(OverflowFlag))
		case 0x18:
			cpu.clc()
		case 0xd8:
			cpu.cld()
		case 0x58:
			cpu.cli()
		case 0xb8:
			cpu.clv()
		case 0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1:
			cpu.cmp(opcode.mode)
		case 0xe0, 0xe4, 0xec:
			cpu.cpx(opcode.mode)
		case 0xc0, 0xc4, 0xcc:
			cpu.cpy(opcode.mode)
		case 0xc6, 0xd6, 0xce, 0xde:
			cpu.dec(opcode.mode)
		case 0xca:
			cpu.dex()
		case 0x88:
			cpu.dey()
		case 0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51:
			cpu.eor(opcode.mode)
		case 0xe6, 0xf6, 0xee, 0xfe:
			cpu.inc(opcode.mode)
		case 0xe8:
			cpu.inx()
		case 0xc8:
			cpu.iny()
		case 0x4c:
			cpu.jmp_absolute()
		case 0x6c:
			cpu.jmp_indirect()
		case 0x20:
			cpu.jsr(opcode.mode)
		case 0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1:
			cpu.lda(opcode.mode)
		case 0xa2, 0xa6, 0xb6, 0xae, 0xbe:
			cpu.ldx(opcode.mode)
		case 0xa0, 0xa4, 0xb4, 0xac, 0xbc:
			cpu.ldy(opcode.mode)
		case 0x4a, 0x46, 0x56, 0x4e, 0x5e:
			cpu.lsr(opcode.mode)
		case 0xea:
			// nop
			break
		case 0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11:
			cpu.ora(opcode.mode)
		case 0x48:
			cpu.pha()
		case 0x08:
			cpu.php()
		case 0x68:
			cpu.pla()
		case 0x28:
			cpu.plp()
		case 0x2a, 0x26, 0x36, 0x2e, 0x3e:
			cpu.rol(opcode.mode)
		case 0x6a, 0x66, 0x76, 0x6e, 0x7e:
			cpu.ror(opcode.mode)
		case 0x40:
			cpu.rti()
		case 0x60:
			cpu.rts()
		case 0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1:
			cpu.sbc(opcode.mode)
		case 0x38:
			cpu.sec()
		case 0xf8:
			cpu.sed()
		case 0x78:
			cpu.sei()
		case 0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91:
			cpu.sta(opcode.mode)
		case 0x86, 0x96, 0x8e:
			cpu.stx(opcode.mode)
		case 0x84, 0x94, 0x8c:
			cpu.sty(opcode.mode)
		case 0xaa:
			cpu.tax()
		case 0xa8:
			cpu.tay()
		case 0xba:
			cpu.tsx()
		case 0x8a:
			cpu.txa()
		case 0x9a:
			cpu.txs()
		case 0x98:
			cpu.tya()
		case 0x00:
			return
		default:
			panic(fmt.Errorf("Unsupported opcode: %#02x", code))
		}

		// A branch didn't occur
		if curr_program_counter == cpu.programCounter {
			cpu.programCounter += uint16(opcode.len) - 1
		}
	}
}
