package bus

import (
	"al/nes-emulator/controller"
	"al/nes-emulator/ppu"
	"al/nes-emulator/rom"
	"fmt"
)

type Bus struct {
	cpuRam     [2048]uint8
	prgRom     []uint8
	Ppu        ppu.PPU
	controller controller.Controller

	cycles uint

	callback func()
}

// Mapped memory locations
const (
	CPU_RAM                   uint16 = 0x0000
	CPU_RAM_MIRRORS_END              = 0x1fff
	PPU_REGISTERS                    = 0x2000
	PPU_REGISTERS_MIRRORS_END        = 0x3fff
	IO_REGISTER_END                  = 0x401f
	ROM_EXPANSION                    = 0x4020
	ROM_EXPANSION_END                = 0x5ffff
	ROM_SAVE                         = 0x6000
	ROM_SAVE_END                     = 0x7ffff
	ROM_PROGRAM                      = 0x8000
	ROM_PROGRAM_END                  = 0xffff
)

func (bus *Bus) Tick(cycles uint8) {
	bus.cycles += uint(cycles)

	newFrame := bus.Ppu.Tick(cycles * 3)

	if newFrame {
		// if !nmiOld && nmiNew {
		bus.callback()
	}
}

func (bus *Bus) PollNMIStatus() bool {
	return bus.Ppu.PollInterruptNMI()
}

func (bus *Bus) ResetNMIStatus() {
	bus.Ppu.ResetInterruptNMI()
}

func (bus *Bus) GetCycles() uint {
	return bus.cycles
}

func (bus *Bus) SetRom(rom *rom.Rom) {
	bus.cycles = 7
	bus.Ppu.Init(rom.ChrRom, rom.ScreenMirroring)
	bus.prgRom = rom.PrgRom
}

func (bus *Bus) SetCallback(callback func()) {
	bus.callback = callback
}

func (bus *Bus) MemRead(addr uint16) uint8 {
	switch {
	case addr <= CPU_RAM_MIRRORS_END:
		mirrored_addr := addr & 0x7ff
		return bus.cpuRam[mirrored_addr]
	case addr == 0x2000 || addr == 0x2001 || addr == 0x2003 ||
		addr == 0x2005 || addr == 0x2006 || addr == 0x4014:
		return 0
	case addr == 0x2002:
		return bus.Ppu.ReadStatus()
	case addr == 0x2004:
		return bus.Ppu.ReadOAMData()
	case addr == 0x2007:
		return bus.Ppu.ReadData()
	case addr == 0x2008:
		return bus.Ppu.ReadData()
	case addr <= PPU_REGISTERS_MIRRORS_END:
		mirroredAddr := addr & 0b0010_0000_0000_0111
		return bus.MemRead(mirroredAddr)
	case addr == 0x4016:
		return bus.controller.ReadControllerData()
	case addr <= IO_REGISTER_END:
		return 0
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
		bus.Ppu.WriteToControl(data)
	case addr == 0x2001:
		bus.Ppu.WriteToMask(data)
	case addr == 0x2003:
		bus.Ppu.WriteToOAMAddress(data)
	case addr == 0x2004:
		bus.Ppu.WriteToOAMData(data)
	case addr == 0x2005:
		bus.Ppu.WriteToScroll(data)
	case addr == 0x2006:
		bus.Ppu.WriteToPPUAddress(data)
	case addr == 0x2007:
		bus.Ppu.WriteToData(data)
	case addr == 0x4014:
		baseAddr := uint16(data) << 8
		bus.copyToOamData(baseAddr)
	case addr <= PPU_REGISTERS_MIRRORS_END:
		mirroredAddr := addr & 0b0010_0000_0000_0111
		bus.MemWrite(mirroredAddr, data)
	case addr == 0x4016:
		bus.controller.WriteToController(data)
	case addr <= IO_REGISTER_END:
		return
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

func (bus *Bus) copyToOamData(baseAddr uint16) {
	data := make([]uint8, 256)
	for i := 0; i < 256; i++ {
		data[i] = bus.MemRead(baseAddr + uint16(i))
	}
	bus.Ppu.CopyToOamData(data)
}
