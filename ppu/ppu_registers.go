package ppu

// controlRegister (0x2000) has various flags to controll ppu operartion.
// See: https://www.nesdev.org/wiki/PPU_registers#Controller_($2000)_%3E_write
type controlRegister uint8
type controlRegisterFlag uint8

const (
	Nametable1 controlRegisterFlag = 1 << iota
	Nametable2
	VramAddIncrement
	SpritePatternAddress
	BackgroundPatternAddress
	SpriteSize
	MasterSlaveSelect
	GenerateNMI
)

func (cr controlRegister) isFlagSet(flag controlRegisterFlag) bool {
	return uint8(cr)&uint8(flag) != 0
}

func (cr controlRegister) getvramAdressIncrementSize() uint8 {
	if cr.isFlagSet(VramAddIncrement) {
		return 1
	} else {
		return 32
	}
}

func (cr *controlRegister) write(data uint8) {
	*cr = controlRegister(data)
}

// maskRegister is the ppu mask register (0x2001).
type maskRegister uint8
type maskRegisterFlag uint8

const (
	Greyscale maskRegisterFlag = 1 << iota
	ShowBackgroundInEightLeftmostPixels
	ShowSpritesInEightLeftmostPixels
	ShowBackground
	ShowSprites
	EmphasizeRed
	EmphasizeGreen
	EmphasizeBlue
)

func (mr maskRegister) isFlagSet(flag maskRegisterFlag) bool {
	return uint8(mr)&uint8(flag) != 0
}

func (mr *maskRegister) write(data uint8) {
	*mr = maskRegister(data)
}

// statusRegister is the status register (0x2002).
// See: https://www.nesdev.org/wiki/PPU_registers#PPUSTATUS
type statusRegister uint8
type statusRegisterFlag uint8

const (
	PPUOpenBus0 statusRegisterFlag = 1 << iota
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

func (sr statusRegister) isFlagSet(flag statusRegisterFlag) bool {
	return uint8(sr)&uint8(flag) != 0
}

func (sr *statusRegister) setFlag(flag statusRegisterFlag, set bool) {
	if set {
		*sr = *sr | statusRegister(flag)
	} else {
		*sr = *sr & statusRegister((^uint8(flag)))
	}
}

// OAMAddrRegister is the OAM address register (0x2003).
type OAMAddrRegister uint8

func (odr OAMAddrRegister) read() uint8 {
	return uint8(odr)
}

func (oar *OAMAddrRegister) write(data uint8) {
	*oar = OAMAddrRegister(data)
}

func (oar *OAMAddrRegister) incremenmt() {
	*oar = OAMAddrRegister(uint8(*oar) + 1)
}

// scrollRegister is the scroll register (0x2005)
type scrollRegister struct {
	posX uint8
	posY uint8
}

func (sr scrollRegister) set(x uint8, y uint8) {
	sr.posX = x
	sr.posY = y
}

func (sr scrollRegister) write(data uint8, writePosX bool) {
	if writePosX {
		sr.posX = data
	} else {
		sr.posY = data
	}
}

func (sr scrollRegister) getPosX() uint8 {
	return sr.posX
}

func (sr scrollRegister) getPosY() uint8 {
	return sr.posY
}

// AddrRegister is the ppu adress register (0x2006) used by the cpu to access
// ppu memory. Two total writes are used to specify a location to read/write to.
// The address register for the ppu does not use little endian notation.
// See: https://www.nesdev.org/wiki/PPU_registers#Address_($2006)_%3E%3E_write_x2
type AddrRegister struct {
	valHi  uint8
	valLow uint8
}

func (ar *AddrRegister) set(data uint16) {
	ar.valHi = uint8(data >> 8)
	ar.valLow = uint8(data & 0xff)
}

func (ar *AddrRegister) get() uint16 {
	return (uint16(ar.valHi) << 8) | uint16(ar.valLow)
}

func (ar *AddrRegister) update(data uint8, loPtr bool) {
	if loPtr {
		ar.valHi = data
	} else {
		ar.valLow = data
	}

	addr := ar.get()
	if addr > 0x3fff {
		addr = addr & 0b11_1111_1111_1111
	}

	ar.set(addr)
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
