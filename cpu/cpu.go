package cpu

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
	cpu.opcodes = OpCodes

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
		case 0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31:
			cpu.and(opcode.mode)
		case 0x0a, 0x06, 0x16, 0x0e, 0x1e:
			cpu.asl(opcode.mode)
		case 0x24, 0x2c:
			cpu.bit(opcode.mode)
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
		case 0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1:
			cpu.lda(opcode.mode)
		case 0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91:
			cpu.sta(opcode.mode)
		case 0xaa:
			cpu.tax()
		case 0xe8:
			cpu.inx()
		case 0x18:
			cpu.clc()
		case 0xd8:
			cpu.cld()
		case 0x58:
			cpu.cli()
		case 0xb8:
			cpu.clv()
		case 0x00:
			return
		default:
			panic("Unsupported opcode")
		}

		cpu.program_counter += uint16(opcode.len) - 1
	}
}
