package ppu

// controlRegister (0x2000) has various flags to controll ppu operartion.
// See: https://www.nesdev.org/wiki/PPU_registers#Controller_($2000)_%3E_write
type controlRegister uint8
type controlRegisterFlag uint8

const (
	Nametable1 controlRegisterFlag = iota
	Nametable2
	VramAddIncrement
	SpritePatternAddress
	BackgroundPatternAddress
	SpriteSize
	MasterSlaveSelect
	GenerateNMI
)

func (cr controlRegister) hasFlag(flag controlRegisterFlag) bool {
	return uint8(cr)&uint8(flag) != 0
}

func (cr controlRegister) getvramAdressIncrementSize() uint8 {
	if cr.hasFlag(VramAddIncrement) {
		return 1
	} else {
		return 32
	}
}

func (cr *controlRegister) update(data uint8) {
	*cr = controlRegister(data)
}

// maskRegister is the ppu mask register (0x2001).
type maskRegister uint8
type maskRegisterFlag uint8

const (
	Greyscale maskRegisterFlag = iota
	ShowBackgroundInEightLeftmostPixels
	ShowSpritesInEightLeftmostPixels
	ShowBackground
	ShowSprites
	EmphasizeRed
	EmphasizeGreen
	EmphasizeBlue
)

func (mr maskRegister) hasFlag(flag maskRegisterFlag) bool {
	return uint8(mr)&uint8(flag) != 0
}

func (mr *maskRegister) update(data uint8) {
	*mr = maskRegister(data)
}

// statusRegister is the status register (0x2002).
// See: https://www.nesdev.org/wiki/PPU_registers#PPUSTATUS
type statusRegister uint8
type statusRegisterFlag uint8

const (
	PPUOpenBus0 statusRegisterFlag = iota
	PPUOpenBus1
	PPUOpenBus2
	PPUOpenBus3
	PPUOpenBus4
	SpriteOverflow
	SpriteZeroHit
	VerticalBlank
)

func (sr statusRegister) read() uint8 {
	return uint8(sr)
}

// AddrRegister is the ppu adress register (0x2006) used by the cpu to access
// ppu memory. Two total writes are used to specify a location to read/write to.
// The address register for the ppu does not use little endian notation.
// See: https://www.nesdev.org/wiki/PPU_registers#Address_($2006)_%3E%3E_write_x2
type AddrRegister struct {
	valHi  uint8
	valLow uint8
	loPtr  bool
}

func (ar *AddrRegister) set(data uint16) {
	ar.valHi = uint8(data >> 8)
	ar.valLow = uint8(data & 0xff)
}

func (ar *AddrRegister) get() uint16 {
	return (uint16(ar.valHi) << 8) | uint16(ar.valLow)
}

func (ar *AddrRegister) update(data uint8) {
	if ar.loPtr {
		ar.valHi = data
	} else {
		ar.valLow = data
	}

	addr := ar.get()
	if addr > 0x3fff {
		addr = addr & 0b11_1111_1111_1111
	}

	ar.set(addr)
	ar.loPtr = !ar.loPtr
}

func (ar *AddrRegister) increment(data uint8) {
	lo := ar.valLow
	ar.valLow += 1
	if lo > ar.valLow {
		ar.valHi += 1
	}

	addr := ar.get()
	if addr > 0x3fff {
		addr = addr & 0b11_1111_1111_1111
	}
}

func (ar *AddrRegister) resetLatch() {
	ar.loPtr = false
}
