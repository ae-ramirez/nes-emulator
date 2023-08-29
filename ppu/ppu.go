package ppu

import (
	"al/nes-emulator/rom"
	"fmt"
)

type PPU struct {
	chrRom       []uint8
	vram         [2048]uint8
	paletteTable [32]uint8
	oamData      [256]uint8

	internalDataBuffer uint8
	mirroring          rom.Mirroring

	// registers
	control    controlRegister // 0x2000
	mask       maskRegister    // 0x2001
	status     statusRegister  // 0x2002
	oamAddr    OAMAddrRegister // 0x2003
	oamDataReg OAMDataRegister // 0x2004
	addr       AddrRegister    // 0x2006

}

func (ppu *PPU) Init(chrRom []uint8, mirroring rom.Mirroring) {
	ppu.chrRom = chrRom
	ppu.mirroring = mirroring
}

func (ppu *PPU) WriteToPPUAddress(value uint8) {
	ppu.addr.update(value)
}

func (ppu *PPU) WriteToControl(value uint8) {
	ppu.control.write(value)
}

func (ppu *PPU) WriteToMask(value uint8) {
	ppu.mask.write(value)
}

func (ppu *PPU) ReadStatus() uint8 {
	ppu.addr.resetLatch()
	return ppu.status.read()
}

func (ppu *PPU) WriteToOAMAddress(value uint8) {
	ppu.oamAddr.write(value)
}

func (ppu *PPU) WriteToOAMData(value uint8) {
	ppu.oamAddr.incremenmt()
	ppu.oamDataReg.write(value)
}

func (ppu *PPU) ReadOAMData() uint8 {
	return ppu.oamData[ppu.oamAddr.read()]
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
		ppu.internalDataBuffer = ppu.chrRom[addr]
		return result
	case addr <= 0x2fff:
		result := ppu.internalDataBuffer
		ppu.internalDataBuffer = ppu.vram[ppu.vramMirrorAddress(addr)]
		return result
	case addr <= 0x3eff:
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
		ppu.chrRom[addr] = data
	case addr <= 0x2fff:
		ppu.vram[ppu.vramMirrorAddress(addr)] = data
	case addr <= 0x3eff:
		panic(fmt.Sprintf("addr space 0x3000..0x3eff is not expected to be used, addr = %02X", addr))
	case addr <= 0x3fff:
		ppu.paletteTable[addr-0x3f00] = data
	default:
		panic(fmt.Sprintf("unexpected write to mirrored space, addr = %02X", addr))
	}
}
