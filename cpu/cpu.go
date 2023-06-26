package cpu

import "fmt"

type CPU struct {
	register_a      uint8
	register_x      uint8
	register_y      uint8
	status          uint8
	program_counter uint16
	opcodes         func() map[uint8]*OpCode
	memory          [0xFFFF]uint8
}

func (cpu *CPU) mem_read(addr uint16) uint8 {
	return cpu.memory[addr]
}

func (cpu *CPU) mem_read_u16(pos uint16) uint16 {
	lo := uint16(cpu.mem_read(pos))
	hi := uint16(cpu.mem_read(pos + 1))
	return (hi << 8) | lo
}

func (cpu *CPU) mem_write(addr uint16, data uint8) {
	cpu.memory[addr] = data
}

func (cpu *CPU) mem_write_u16(pos uint16, data uint16) {
	hi := data >> 8
	lo := data & 0xff
	cpu.mem_write(pos, uint8(lo))
	cpu.mem_write(pos+1, uint8(hi))
}

func (cpu *CPU) reset() {
	cpu.register_a = 0
	cpu.register_x = 0
	cpu.status = 0
	cpu.opcodes = OpCodesMapFunc()

	cpu.program_counter = cpu.mem_read_u16(0xFFFC)
}

func (cpu *CPU) load_and_run(program []uint8) {
	cpu.load(program)
	cpu.reset()
	cpu.run()
}

func (cpu *CPU) load(program []uint8) {
	copy(cpu.memory[0x8000:0x8000+len(program)], program)
	cpu.mem_write_u16(0xFFFC, 0x8000)
}

func (cpu *CPU) run() {
	opcodes := cpu.opcodes()
	for {
		code := cpu.mem_read(cpu.program_counter)
		cpu.program_counter += 1

		opcode := opcodes[code]

		switch code {
		case 0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1:
			cpu.lda(opcode.mode)
		case 0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91:
			cpu.sta(opcode.mode)
		case 0xaa:
			cpu.tax()
		case 0xe8:
			cpu.inx()
		case 0x00:
			return
		default:
			return
		}

		cpu.program_counter += uint16(opcode.len) - 1
	}
}

type AddressingMode int

const (
	Immediate = iota
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

func (cpu *CPU) lda(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.mem_read(addr)

	cpu.register_a = value
	cpu.updateZeroAndNegativeFlags(cpu.register_a)
}

func (cpu *CPU) tax() {
	cpu.register_x = cpu.register_a
	cpu.updateZeroAndNegativeFlags(cpu.register_x)
}

func (cpu *CPU) inx() {
	cpu.register_x += 1
	cpu.updateZeroAndNegativeFlags(cpu.register_x)
}

func (cpu *CPU) sta(mode AddressingMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.mem_write(addr, cpu.register_a)
}
func (cpu *CPU) updateZeroAndNegativeFlags(result uint8) {
	if result == 0 {
		cpu.status = cpu.status | 0b0000_0010
	} else {
		cpu.status = cpu.status & 0b1111_1101
	}

	if result&0b1000_0000 != 0 {
		cpu.status = cpu.status | 0b1000_0000
	} else {
		cpu.status = cpu.status & 0b0111_1111
	}
}
