package bus

import (
	cart "al/nes-emulator/rom"
)

type Bus struct {
	cpuRam [2048]uint8
	rom    *cart.Rom
}

// Mapped memory locations
const (
	CPU_RAM                   uint16 = 0x0000
	CPU_RAM_MIRRORS_END              = 0x1fff
	PPU_REGISTERS                    = 0x2000
	PPU_REGISTERS_MIRRORS_END        = 0x401f
	ROM_EXPANSION                    = 0x4020
	ROM_EXPANSION_END                = 0x5ffff
	ROM_SAVE                         = 0x6000
	ROM_SAVE_END                     = 0x7ffff
	ROM_PROGRAM                      = 0x8000
	ROM_PROGRAM_END                  = 0xffff
)

func (bus *Bus) SetRom(rom *cart.Rom) {
	bus.rom = rom
}

func (bus *Bus) MemRead(addr uint16) uint8 {
	if addr <= CPU_RAM_MIRRORS_END {
		mirrored_addr := addr & 0x7ff
		return bus.cpuRam[mirrored_addr]
	} else if ROM_PROGRAM <= addr && addr <= ROM_PROGRAM_END {
		return bus.readPrgRom(addr)
	} else {
		panic("invalid memory read")
	}
}

func (bus *Bus) MemRead_u16(pos uint16) uint16 {
	lo := uint16(bus.MemRead(pos))
	hi := uint16(bus.MemRead(pos + 1))
	return (hi << 8) | lo
}

func (bus *Bus) MemWrite(addr uint16, data uint8) {
	if addr <= CPU_RAM_MIRRORS_END {
		mirrored_addr := addr & 0x7ff
		bus.cpuRam[mirrored_addr] = data
	} else if ROM_PROGRAM <= addr && addr <= ROM_PROGRAM_END {
		bus.writePrgRom(addr, data)
	} else {
		panic("invalid memory write")
	}
}

func (bus *Bus) MemWrite_u16(pos uint16, data uint16) {
	hi := data >> 8
	lo := data & 0xff
	bus.MemWrite(pos, uint8(lo))
	bus.MemWrite(pos+1, uint8(hi))
}

func (bus *Bus) readPrgRom(addr uint16) uint8 {
	addr -= 0x8000
	// mirror adress if necessary
	if addr >= 0x4000 && len(bus.rom.PrgRom) == 0x4000 {
		addr = addr % 0x4000
	}
	return bus.rom.PrgRom[addr]
}

func (bus *Bus) writePrgRom(addr uint16, data uint8) {
	addr -= 0x8000
	// mirror adress if necessary
	if addr >= 0x4000 && len(bus.rom.PrgRom) == 0x4000 {
		addr = addr % 0x4000
	}
	bus.rom.PrgRom[addr] = data
}
