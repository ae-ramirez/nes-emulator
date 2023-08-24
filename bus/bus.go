package bus

import (
	"al/nes-emulator/ppu"
	"al/nes-emulator/rom"
	"fmt"
)

type Bus struct {
	cpuRam [2048]uint8
	prgRom []uint8
	ppu    ppu.PPU
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

func (bus *Bus) SetRom(rom *rom.Rom) {
	bus.ppu.Init(rom.ChrRom, rom.ScreenMirroring)
	bus.prgRom = rom.PrgRom
}

func (bus *Bus) MemRead(addr uint16) uint8 {
	switch {
	case addr <= CPU_RAM_MIRRORS_END:
		mirrored_addr := addr & 0x7ff
		return bus.cpuRam[mirrored_addr]
	case addr == 0x2000 || addr == 0x2001 || addr == 0x2003 ||
		addr == 0x2005 || addr == 0x2006 || addr == 0x4014:
		panic(fmt.Sprintf("Attempting to write to a read only PPU adress: %04x", addr))
	case addr == 0x2007:
		return bus.ppu.ReadData()
	case addr <= PPU_REGISTERS_MIRRORS_END:
		mirroredAddr := addr & 0b0010_0000_0000_0111
		return bus.MemRead(mirroredAddr)
	case ROM_PROGRAM <= addr && addr <= ROM_PROGRAM_END:
		return bus.readPrgRom(addr)
	default:
		panic(fmt.Sprintf("invalid memory read of addr: %04x", addr))
	}
}

func (bus *Bus) MemRead_u16(pos uint16) uint16 {
	lo := uint16(bus.MemRead(pos))
	hi := uint16(bus.MemRead(pos + 1))
	return (hi << 8) | lo
}

func (bus *Bus) MemWrite(addr uint16, data uint8) {
	switch {
	case addr <= CPU_RAM_MIRRORS_END:
		mirrored_addr := addr & 0x7ff
		bus.cpuRam[mirrored_addr] = data
	case addr == 0x2000:
		bus.ppu.WriteToControl(data)
	case addr == 0x2006:
		bus.ppu.WriteToPPUAddress(data)
	case addr == 0x2007:
		bus.ppu.WriteToData(data)
	case addr <= PPU_REGISTERS_MIRRORS_END:
		mirroredAddr := addr & 0b0010_0000_0000_0111
		bus.MemWrite(mirroredAddr, data)
	case ROM_PROGRAM <= addr && addr <= ROM_PROGRAM_END:
		bus.writePrgRom(addr, data)
	default:
		panic(fmt.Sprintf("invalid memory write to addr: %04x", addr))
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
	if addr >= 0x4000 && len(bus.prgRom) == 0x4000 {
		addr = addr % 0x4000
	}
	return bus.prgRom[addr]
}

func (bus *Bus) writePrgRom(addr uint16, data uint8) {
	addr -= 0x8000
	// mirror adress if necessary
	if addr >= 0x4000 && len(bus.prgRom) == 0x4000 {
		addr = addr % 0x4000
	}
	bus.prgRom[addr] = data
}
