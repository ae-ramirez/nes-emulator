package rom

import (
	"fmt"
)

type Mirroring int

const (
	VERTICAL    Mirroring = iota
	HORIZONTAL            = iota
	FOUR_SCREEN           = iota
)

const (
	PRG_ROM_PAGE_SIZE uint = 1024 * 16
	CHR_ROM_PAGE_SIZE      = 1024 * 8
)

var NES_TAG [4]uint8 = [4]uint8{0x4e, 0x45, 0x53, 0x1a}

const FAILED_TO_INITIALIZE_ROM = "Failed to initialize rom: %s"

type Rom struct {
	PrgRom          []uint8
	ChrRom          []uint8
	Mapper          uint8
	ScreenMirroring Mirroring
}

func (rom *Rom) Init(rawData []uint8) error {
	if [4]uint8(rawData[0:4]) != NES_TAG {
		return fmt.Errorf(FAILED_TO_INITIALIZE_ROM, "File is not in iNES file format")
	}

	iNesVer := (rawData[7] >> 2) & 0b11
	if iNesVer != 0 {
		return fmt.Errorf(FAILED_TO_INITIALIZE_ROM, "NES2.0 format is not supported")
	}

	rom.Mapper = (rawData[7] & 0b1111_0000) | (rawData[6] >> 4)

	if rawData[6]&0b1000 != 0 {
		rom.ScreenMirroring = FOUR_SCREEN
	} else if rawData[6]&0b1 != 0 {
		rom.ScreenMirroring = VERTICAL
	} else {
		rom.ScreenMirroring = HORIZONTAL
	}

	prgRomSize := uint(PRG_ROM_PAGE_SIZE) * uint(rawData[4])
	rom.PrgRom = make([]uint8, prgRomSize)
	chrRomSize := uint(CHR_ROM_PAGE_SIZE) * uint(rawData[5])
	rom.ChrRom = make([]uint8, chrRomSize)

	skipTrainer := rawData[6]&0b100 != 0

	prgRomStart := 16
	if skipTrainer {
		prgRomStart += 512
	}
	chrRomStart := prgRomStart + int(prgRomSize)

	rom.PrgRom = rawData[prgRomStart : prgRomStart+int(prgRomSize)]
	rom.ChrRom = rawData[chrRomStart : chrRomStart+int(chrRomSize)]

	return nil
}

func (rom *Rom) InitMemory() {
	rom.PrgRom = make([]uint8, PRG_ROM_PAGE_SIZE)
	rom.ChrRom = make([]uint8, CHR_ROM_PAGE_SIZE)
}
