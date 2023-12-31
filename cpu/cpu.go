package cpu

import (
	"al/nes-emulator/bus"
	"al/nes-emulator/rom"
	"fmt"
	"strings"
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
const stackReset = 0xfd

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

func (cpu *CPU) SetProgramCounter(programCounter uint16) {
	cpu.programCounter = programCounter
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
	var currProgramCounter uint16
	var extraCycles uint8

	for {
		if cpu.Bus.PollNMIStatus() {
			cpu.interruptNMI()
		}

		callback(cpu)

		code := cpu.MemRead(cpu.programCounter)
		cpu.programCounter += 1
		currProgramCounter = cpu.programCounter
		opcode := opcodes[code]

		extraCycles = cpu.execOp(opcode, code)

		cpu.Bus.Tick(opcode.cycles)
		cpu.Bus.Tick(extraCycles)

		if currProgramCounter != cpu.programCounter {
			// A branch occured
			continue
		}

		cpu.programCounter += uint16(opcode.len) - 1
	}
}

func (cpu *CPU) interruptNMI() {
	cpu.stackPush_u16(cpu.programCounter)
	cpu.stackPush(cpu.status | 0b0010_0000)

	cpu.setStatusFlag(InterruptDisableFlag, true)
	cpu.Bus.Tick(2)
	cpu.programCounter = cpu.MemRead_u16(0xfffa)
}

func (cpu *CPU) execOp(opcode *OpCode, code uint8) uint8 {
	switch code {
	case 0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71:
		cpu.adc(opcode.mode)
	case 0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31:
		cpu.and(opcode.mode)
	case 0x0a, 0x06, 0x16, 0x0e, 0x1e:
		cpu.asl(opcode.mode)
	case 0x90:
		// bcc
		return cpu.branch(cpu.isStatusFlagClear(CarryFlag))
	case 0xb0:
		// bcs
		return cpu.branch(cpu.isStatusFlagSet(CarryFlag))
	case 0xf0:
		// beq
		return cpu.branch(cpu.isStatusFlagSet(ZeroFlag))
	case 0x24, 0x2c:
		cpu.bit(opcode.mode)
	case 0x30:
		// bmi
		return cpu.branch(cpu.isStatusFlagSet(NegativeFlag))
	case 0xd0:
		// bne
		return cpu.branch(cpu.isStatusFlagClear(ZeroFlag))
	case 0x10:
		// bpl
		return cpu.branch(cpu.isStatusFlagClear(NegativeFlag))
	case 0x50:
		// bvc
		return cpu.branch(cpu.isStatusFlagClear(OverflowFlag))
	case 0x70:
		// bvs
		return cpu.branch(cpu.isStatusFlagSet(OverflowFlag))
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
		return cpu.lda(opcode.mode)
	case 0xa2, 0xa6, 0xb6, 0xae, 0xbe:
		return cpu.ldx(opcode.mode)
	case 0xa0, 0xa4, 0xb4, 0xac, 0xbc:
		return cpu.ldy(opcode.mode)
	case 0x4a, 0x46, 0x56, 0x4e, 0x5e:
		cpu.lsr(opcode.mode)
	case 0xea, 0x04, 0x14, 0x34, 0x44, 0x54, 0x64, 0x74, 0x80, 0x82,
		0x89, 0xc2, 0xd4, 0xe2, 0xf4, 0x0c, 0x1c, 0x3c, 0x5c, 0x7c,
		0xdc, 0xfc, 0x1a, 0x3a, 0x5a, 0x7a, 0xda, 0xfa:
		if opcode.mode == Absolute_X {
			_, pageCrossed := cpu.getOperandAddress(opcode.mode)
			if pageCrossed {
				return 1
			}
		}
		// nop
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
	case 0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1, 0xeb:
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
		cpu.brk()

	// unofficial opcodes
	case 0xa7, 0xb7, 0xaf, 0xbf, 0xa3, 0xb3:
		cpu.ldx(opcode.mode)
		cpu.registerA = cpu.registerX
		if (opcode.mode == Absolute_Y || opcode.mode == Indirect_Y) {
			_, pageCrossed := cpu.getOperandAddress(opcode.mode)
			if pageCrossed {
				return 1
			}
		}
	case 0x87, 0x97, 0x83, 0x8f:
		cpu.sax(opcode.mode)
	case 0xc7, 0xd7, 0xcf, 0xdf, 0xdb, 0xc3, 0xd3:
		cpu.dcp(opcode.mode)
	case 0xe7, 0xf7, 0xef, 0xff, 0xfb, 0xe3, 0xf3:
		cpu.inc(opcode.mode)
		cpu.sbc(opcode.mode)
	case 0x07, 0x17, 0x0f, 0x1f, 0x1b, 0x03, 0x13:
		cpu.asl(opcode.mode)
		cpu.ora(opcode.mode)
	case 0x27, 0x37, 0x2f, 0x3f, 0x3b, 0x23, 0x33:
		cpu.rol(opcode.mode)
		cpu.and(opcode.mode)
	case 0x47, 0x57, 0x4f, 0x5f, 0x5b, 0x43, 0x53:
		cpu.lsr(opcode.mode)
		cpu.eor(opcode.mode)
	case 0x67, 0x77, 0x6f, 0x7f, 0x7b, 0x63, 0x73:
		cpu.ror(opcode.mode)
		cpu.adc(opcode.mode)
	default:
		panic(fmt.Errorf("Unsupported opcode: %#02x", code))
	}
	return 0
}

func Trace(c *CPU) string {
	code := c.MemRead(c.programCounter)
	opcode := c.opcodes()[code]

	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("%04X  ", c.programCounter))
	sb.WriteString(fmt.Sprintf("%02X ", code))

	if opcode.len == 1 {
		sb.WriteString("      ")
	} else if opcode.len == 2 {
		sb.WriteString(fmt.Sprintf("%02X    ", c.MemRead(c.programCounter+1)))
	} else {
		sb.WriteString(fmt.Sprintf("%02X %02X ", c.MemRead(c.programCounter+1), c.MemRead(c.programCounter+2)))
	}

	opcodeString := c.OpcodeToAssembly(opcode)
	if opcodeString[0] != '*' {
		sb.WriteString(" ")
	}
	sb.WriteString(opcodeString)
	sb.WriteString(strings.Repeat(" ", 48-sb.Len()))

	sb.WriteString(fmt.Sprintf("A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d", c.registerA, c.registerX, c.registerY, c.status, c.stackPointer, c.Bus.GetCycles()))

	return sb.String()
}

func (c *CPU) OpcodeToAssembly(opcode *OpCode) string {
	var sb strings.Builder
	sb.WriteString(opcode.mnemonic)

	if opcode.mode == NoneAddressing {
		if opcode.code == 0x4a ||
			opcode.code == 0x0a ||
			opcode.code == 0x6a ||
			opcode.code == 0x2a {
			sb.WriteString(" A")
		}
		return sb.String()
	}

	sb.WriteString(" ")

	var addr uint16
	c.programCounter += 1
	addr, _ = c.getOperandAddress(opcode.mode)
	c.programCounter -= 1

	if opcode.code == 0x6c { // indirect jump
		sb.WriteString(fmt.Sprintf("($%04X) = %04X", addr, c.getJmpIndirectAdress(addr)))
	} else if opcode.code == 0x90 || // branching instructions
		opcode.code == 0xb0 || opcode.code == 0xf0 ||
		opcode.code == 0x30 || opcode.code == 0xd0 ||
		opcode.code == 0x10 || opcode.code == 0x50 ||
		opcode.code == 0x70 {
		sb.WriteString(fmt.Sprintf("$%04X", uint16(int8(c.MemRead(addr)))+uint16(c.programCounter)+2))
	} else if opcode.mode == Immediate {
		sb.WriteString(fmt.Sprintf("#$%02X", c.MemRead(addr)))
	} else if opcode.mode == ZeroPage {
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("$%02X = %02X", addr, val))
	} else if opcode.mode == ZeroPage_X {
		pos := c.MemRead(c.programCounter + 1)
		sb.WriteString(fmt.Sprintf("$%02X,X ", pos))
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("@ %02X = %02X", addr, val))
	} else if opcode.mode == ZeroPage_Y {
		pos := c.MemRead(c.programCounter + 1)
		sb.WriteString(fmt.Sprintf("$%02X,Y ", pos))
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("@ %02X = %02X", addr, val))
	} else if opcode.mode == Absolute {
		sb.WriteString(fmt.Sprintf("$%04X ", addr))
		if opcode.code != 0x4c && opcode.code != 0x20 {
			sb.WriteString(fmt.Sprintf("= %02X", c.MemRead(addr)))
		}
	} else if opcode.mode == Absolute_X {
		base := c.MemRead_u16(c.programCounter + 1)
		sb.WriteString(fmt.Sprintf("$%04X,X ", base))
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("@ %04X = %02X", addr, val))
	} else if opcode.mode == Absolute_Y {
		base := c.MemRead_u16(c.programCounter + 1)
		sb.WriteString(fmt.Sprintf("$%04X,Y ", base))
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("@ %04X = %02X", addr, val))
	} else if opcode.mode == Indirect_X {
		base := c.MemRead(c.programCounter + 1)
		sb.WriteString(fmt.Sprintf("($%02X,X) ", base))
		ptr := base + c.registerX
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("@ %02X = %04X = %02X", ptr, addr, val))
	} else if opcode.mode == Indirect_Y {
		base := c.MemRead(c.programCounter + 1)
		lo := uint16(c.MemRead(uint16(base)))
		hi := uint16(c.MemRead(uint16(base + 1)))
		deref_base := (hi << 8) | lo
		sb.WriteString(fmt.Sprintf("($%02X),Y = %04X ", base, deref_base))
		val := c.MemRead(addr)
		sb.WriteString(fmt.Sprintf("@ %04X = %02X", addr, val))
	}
	return sb.String()
}
