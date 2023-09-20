package ppu

import (
	"al/nes-emulator/rom"
	"fmt"
)

type PPU struct {
	ChrRom       []uint8
	Vram         [2048]uint8
	paletteTable [32]uint8
	OamData      [256]uint8

	internalDataBuffer uint8
	mirroring          rom.Mirroring

	// registers
	control controlRegister // 0x2000
	mask    maskRegister    // 0x2001
	status  statusRegister  // 0x2002
	scroll  scrollRegister  // 0x2005
	addr    AddrRegister    // 0x2006

	// internal register
	wLatch  bool
	oamAddr uint8 // 0x2003

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

		if ppu.scanline == 241 {
			ppu.status.setFlag(VerticalBlank, true)
			ppu.status.setFlag(SpriteZeroHit, false)
			if ppu.control.isFlagSet(GenerateNMI) {
				ppu.triggerInterruptNMI()
			}
		}

		if ppu.scanline >= 262 {
			ppu.scanline = 0
			ppu.ResetInterruptNMI()
			ppu.status.setFlag(VerticalBlank, false)
			ppu.status.setFlag(SpriteZeroHit, false)
			return true
		}
	}
	return false
}

func (ppu *PPU) resetLatch() {
	ppu.wLatch = false
}

func (ppu *PPU) triggerInterruptNMI() {
	ppu.interruptNMI = true
}

func (ppu *PPU) ResetInterruptNMI() {
	ppu.interruptNMI = false
}

func (ppu *PPU) PollInterruptNMI() bool {
	oldInterruptNMI := ppu.interruptNMI
	ppu.interruptNMI = false
	return oldInterruptNMI
}

func (ppu *PPU) BackgroundPatternAddress() uint16 {
	if ppu.control.isFlagSet(BackgroundPatternAddress) {
		return 0x1000
	} else {
		return 0x0000
	}
}

func (ppu *PPU) SpritePatternAddress() uint16 {
	if ppu.control.isFlagSet(SpritePatternAddress) {
		return 0x1000
	} else {
		return 0x0000
	}
}

func (ppu *PPU) WriteToPPUAddress(value uint8) {
	ppu.addr.update(value, ppu.wLatch)
	ppu.wLatch = !ppu.wLatch
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
	currentStatus := ppu.status.read()
	ppu.status.setFlag(VerticalBlank, false)
	ppu.resetLatch()
	return currentStatus
}

func (ppu *PPU) WriteToOAMAddress(value uint8) {
	ppu.oamAddr = value
}

func (ppu *PPU) WriteToOAMData(value uint8) {
	ppu.OamData[ppu.oamAddr] = value
	ppu.oamAddr += 1
}

func (ppu *PPU) ReadOAMData() uint8 {
	return ppu.OamData[ppu.oamAddr]
}

func (ppu *PPU) WriteToScroll(value uint8) {
	ppu.scroll.write(value, ppu.wLatch)
	ppu.wLatch = !ppu.wLatch
}

func (ppu *PPU) WriteToData(value uint8) {
	ppu.writeData(value)
}

func (ppu *PPU) incrementVramAddress() {
	ppu.addr.increment(ppu.control.getvramAdressIncrementSize())
}

func (ppu *PPU) vramMirrorAddress(addr uint16) uint16 {
	mirroredVram := addr & 0b10_1111_1111_1111
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
		ppu.internalDataBuffer = ppu.paletteTable[(addr-0x3f00)&0b1_1111]
		return ppu.internalDataBuffer
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
		ppu.paletteTable[(addr-0x3f00)&0b1_1111] = data
	default:
		panic(fmt.Sprintf("unexpected write to mirrored space, addr = %02X", addr))
	}
}

func (ppu *PPU) GetBackgroundPalette(paletteN uint8) [4]uint8 {
	var palette [4]uint8
	paletteAddr := 0x01 + paletteN*4
	palette[0] = ppu.paletteTable[0]
	palette[1] = ppu.paletteTable[paletteAddr]
	palette[2] = ppu.paletteTable[paletteAddr+1]
	palette[3] = ppu.paletteTable[paletteAddr+2]
	return palette
}

func (ppu *PPU) GetSpritePalette(paletteN uint8) [4]uint8 {
	var palette [4]uint8
	paletteAddr := 0x11 + paletteN*4
	palette[0] = 0
	palette[1] = ppu.paletteTable[paletteAddr]
	palette[2] = ppu.paletteTable[paletteAddr+1]
	palette[3] = ppu.paletteTable[paletteAddr+2]
	return palette
}
