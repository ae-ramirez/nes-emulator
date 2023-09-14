package ppu

import (
	"al/nes-emulator/rom"
	"fmt"
)

type PPU struct {
	ChrRom       []uint8
	Vram         [2048]uint8
	paletteTable [32]uint8
	oamData      [256]uint8

	internalDataBuffer uint8
	mirroring          rom.Mirroring

	// registers
	control    controlRegister // 0x2000
	mask       maskRegister    // 0x2001
	status     statusRegister  // 0x2002
	oamAddr    OAMAddrRegister // 0x2003
	scroll     scrollRegister  // 0x2005
	addr       AddrRegister    // 0x2006

	// internal register
	w_latch bool

	scanline uint16
	cycles   uint

	interruptNMI bool
}

func (ppu *PPU) Init(chrRom []uint8, mirroring rom.Mirroring) {
	ppu.ChrRom = chrRom
	ppu.mirroring = mirroring
}

func (ppu *PPU) Tick(cycles uint8) bool {
	ppu.cycles += uint(cycles)

	if ppu.cycles >= 341 {
		ppu.cycles = ppu.cycles - 341
		ppu.scanline += 1
	}

	if ppu.scanline == 241 {
		if ppu.control.isFlagSet(GenerateNMI) {
			ppu.status.setFlag(VerticalBlank, true)
			ppu.triggerInterruptNMI()
		}
	}

	if ppu.scanline >= 262 {
		ppu.scanline = 0
		ppu.status.setFlag(VerticalBlank, false)
		return true
	}
	return false
}

func (ppu *PPU) resetLatch() {
	ppu.w_latch = false
}

func (ppu *PPU) triggerInterruptNMI() {
	ppu.interruptNMI = true
}

func (ppu *PPU) PollInterruptNMI() bool {
	oldInterruptNMI := ppu.interruptNMI
	ppu.interruptNMI = false
	return oldInterruptNMI
}

func (ppu *PPU) WriteToPPUAddress(value uint8) {
	ppu.addr.update(value, ppu.w_latch)
	ppu.w_latch = !ppu.w_latch
}

func (ppu *PPU) WriteToControl(value uint8) {
	oldNMIStatus := ppu.control.isFlagSet(GenerateNMI)
	ppu.control.write(value)
	if !oldNMIStatus &&
		ppu.control.isFlagSet(GenerateNMI) &&
		ppu.status.isFlagSet(VerticalBlank) {
		ppu.triggerInterruptNMI()
	}
}

func (ppu *PPU) WriteToMask(value uint8) {
	ppu.mask.write(value)
}

func (ppu *PPU) ReadStatus() uint8 {
	ppu.resetLatch()
	return ppu.status.read()
}

func (ppu *PPU) WriteToOAMAddress(value uint8) {
	ppu.oamAddr.write(value)
}

func (ppu *PPU) WriteToOAMData(value uint8) {
	ppu.oamData[ppu.oamAddr.read()] = value
	ppu.oamAddr.incremenmt()
}

func (ppu *PPU) ReadOAMData() uint8 {
	return ppu.oamData[ppu.oamAddr.read()]
}

func (ppu *PPU) WriteToScroll(value uint8) {
	ppu.scroll.write(value, ppu.w_latch)
	ppu.w_latch = !ppu.w_latch
}

func (ppu *PPU) WriteToData(value uint8) {
	ppu.writeData(value)
}

func (ppu *PPU) incrementVramAddress() {
	ppu.addr.increment(ppu.control.getvramAdressIncrementSize())
}

func (ppu *PPU) vramMirrorAddress(addr uint16) uint16 {
	mirroredVram := addr & 0b11_1111_1111_1111
	vramIndex := mirroredVram - 0x2000
	nameTable := vramIndex / 0x0400
	if ppu.mirroring == rom.VERTICAL {
		if nameTable == 2 || nameTable == 3 {
			return vramIndex - 0x0800
		} else {
			return vramIndex
		}
	} else if ppu.mirroring == rom.HORIZONTAL {
		if nameTable == 1 || nameTable == 2 {
			return vramIndex - 0x400
		} else if nameTable == 3 {
			return vramIndex - 0x0800
		} else {
			return vramIndex
		}
	} else {
		return vramIndex
	}
}

func (ppu *PPU) ReadData() uint8 {
	addr := ppu.addr.get()
	ppu.incrementVramAddress()

	switch {
	case addr <= 0x1fff:
		result := ppu.internalDataBuffer
		ppu.internalDataBuffer = ppu.ChrRom[addr]
		return result
	case addr <= 0x2fff:
		result := ppu.internalDataBuffer
		ppu.internalDataBuffer = ppu.Vram[ppu.vramMirrorAddress(addr)]
		return result
	case 0x3000 <= addr && addr <= 0x3eff:
		panic(fmt.Sprintf("addr space 0x3000..0x3eff is not expected to be used, addr = %02X", addr))
	case addr <= 0x3fff:
		ppu.internalDataBuffer = ppu.paletteTable[addr-0x1000]
		return ppu.paletteTable[addr-0x3f00]
	default:
		panic(fmt.Sprintf("unexpected access to mirrored space, addr = %02X", addr))
	}
}

func (ppu *PPU) writeData(data uint8) {
	addr := ppu.addr.get()
	ppu.incrementVramAddress()

	switch {
	case addr <= 0x1fff:
		ppu.ChrRom[addr] = data
	case addr <= 0x2fff:
		ppu.Vram[ppu.vramMirrorAddress(addr)] = data
	case 0x3000 <= addr && addr <= 0x3eff:
		panic(fmt.Sprintf("addr space 0x3000..0x3eff is not expected to be used, addr = %02X", addr))
	case addr <= 0x3fff:
		ppu.paletteTable[addr-0x3f00] = data
	default:
		panic(fmt.Sprintf("unexpected write to mirrored space, addr = %02X", addr))
	}
}
