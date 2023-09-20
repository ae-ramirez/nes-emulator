package render

import PPU "al/nes-emulator/ppu"

const (
	frameWidth  = 256
	frameHeight = 240
)

type Frame struct {
	Data [frameWidth * frameHeight * 3]uint8
}

func (frame *Frame) SetPixel(x int, y int, rgb [3]uint8) {
	base := y*3*frameWidth + x*3
	if base+2 < len(frame.Data) {
		frame.Data[base] = rgb[0]
		frame.Data[base+1] = rgb[1]
		frame.Data[base+2] = rgb[2]

	}
}

func getTile(ppu *PPU.PPU, tileN uint8, bankAddr uint16) []uint8 {
	addr := bankAddr + uint16(tileN)*16
	return ppu.ChrRom[addr : addr+16]
}

func Render(ppu *PPU.PPU, frame *Frame) {
	drawBackground(ppu, frame)
	drawSprites(ppu, frame)
}

func drawBackground(ppu *PPU.PPU, frame *Frame) {
	bankAddr := ppu.BackgroundPatternAddress()
	for i := 0; i < 0x03c0; i++ {
		tileN := ppu.Vram[i]
		tileX := i % 32
		tileY := i / 32
		tile := getTile(ppu, tileN, bankAddr)
		palette := getBackgroundPalette(ppu, tileX, tileY)

		drawBackgroundTile(frame, tile, palette, tileX, tileY)
	}
}

// NOTE: currently we aren't considering 8x16 sprites or sprite priorities
// see: https://www.nesdev.org/wiki/PPU_OAM
func drawSprites(ppu *PPU.PPU, frame *Frame) {
	bankAddr := ppu.SpritePatternAddress()
	for i := 0; i < 256; i += 4 {
		spriteY := int(ppu.OamData[i])
		spriteIndex := ppu.OamData[i+1]
		spriteAttributes := ppu.OamData[i+2]
		spriteX := int(ppu.OamData[i+3])

		tile := getTile(ppu, spriteIndex, bankAddr)
		palette := ppu.GetSpritePalette(spriteAttributes & 0b11)
		mirrorH := spriteAttributes&0b1000_0000 != 0
		mirrorV := spriteAttributes&0b0100_0000 != 0

		drawSpriteTile(frame, tile, palette, spriteX, spriteY, mirrorH, mirrorV)
	}
}

func drawBackgroundTile(frame *Frame, tile []uint8, palette [4]uint8, tileX int, tileY int) {
	for y := 0; y < 8; y++ {
		upper := tile[y]
		lower := tile[y+8]
		for x := 7; x >= 0; x-- {
			value := (1&lower)<<1 | (1 & upper)
			upper = upper >> 1
			lower = lower >> 1
			rgb := nesSystemColors[palette[value]]
			frame.SetPixel(tileX*8+x, tileY*8+y, rgb)
		}
	}
}

func drawSpriteTile(frame *Frame, tile []uint8, palette [4]uint8, tileX int, tileY int, mirrorH bool, mirrorV bool) {
	for y := 0; y < 8; y++ {
		upper := tile[y]
		lower := tile[y+8]
		for x := 7; x >= 0; x-- {
			value := (1&lower)<<1 | (1 & upper)
			upper = upper >> 1
			lower = lower >> 1
			if value == 0 {
				continue
			}
			rgb := nesSystemColors[palette[value]]
			xPos := tileX
			yPos := tileY
			if mirrorV {
				xPos = xPos + 7 - x
			} else {
				xPos = xPos + x
			}
			if mirrorH {
				yPos = yPos + 7 - y
			} else {
				yPos = yPos + y
			}
			frame.SetPixel(xPos, yPos, rgb)
		}
	}
}

func getBackgroundPalette(ppu *PPU.PPU, tileCol int, tileRow int) [4]uint8 {
	attributeTableAddr := int(tileRow/4*8 + tileCol/4)
	byte := ppu.Vram[0x3c0+attributeTableAddr]
	metaTile := ((tileRow % 4 / 2) << 1) | (tileCol % 4 / 2)
	shift := metaTile * 2
	palette := (byte >> shift) & 0b11
	return ppu.GetBackgroundPalette(palette)
}

var nesSystemColors = [][3]uint8{
	{0x80, 0x80, 0x80}, {0x00, 0x3D, 0xA6}, {0x00, 0x12, 0xB0}, {0x44, 0x00, 0x96}, {0xA1, 0x00, 0x5E},
	{0xC7, 0x00, 0x28}, {0xBA, 0x06, 0x00}, {0x8C, 0x17, 0x00}, {0x5C, 0x2F, 0x00}, {0x10, 0x45, 0x00},
	{0x05, 0x4A, 0x00}, {0x00, 0x47, 0x2E}, {0x00, 0x41, 0x66}, {0x00, 0x00, 0x00}, {0x05, 0x05, 0x05},
	{0x05, 0x05, 0x05}, {0xC7, 0xC7, 0xC7}, {0x00, 0x77, 0xFF}, {0x21, 0x55, 0xFF}, {0x82, 0x37, 0xFA},
	{0xEB, 0x2F, 0xB5}, {0xFF, 0x29, 0x50}, {0xFF, 0x22, 0x00}, {0xD6, 0x32, 0x00}, {0xC4, 0x62, 0x00},
	{0x35, 0x80, 0x00}, {0x05, 0x8F, 0x00}, {0x00, 0x8A, 0x55}, {0x00, 0x99, 0xCC}, {0x21, 0x21, 0x21},
	{0x09, 0x09, 0x09}, {0x09, 0x09, 0x09}, {0xFF, 0xFF, 0xFF}, {0x0F, 0xD7, 0xFF}, {0x69, 0xA2, 0xFF},
	{0xD4, 0x80, 0xFF}, {0xFF, 0x45, 0xF3}, {0xFF, 0x61, 0x8B}, {0xFF, 0x88, 0x33}, {0xFF, 0x9C, 0x12},
	{0xFA, 0xBC, 0x20}, {0x9F, 0xE3, 0x0E}, {0x2B, 0xF0, 0x35}, {0x0C, 0xF0, 0xA4}, {0x05, 0xFB, 0xFF},
	{0x5E, 0x5E, 0x5E}, {0x0D, 0x0D, 0x0D}, {0x0D, 0x0D, 0x0D}, {0xFF, 0xFF, 0xFF}, {0xA6, 0xFC, 0xFF},
	{0xB3, 0xEC, 0xFF}, {0xDA, 0xAB, 0xEB}, {0xFF, 0xA8, 0xF9}, {0xFF, 0xAB, 0xB3}, {0xFF, 0xD2, 0xB0},
	{0xFF, 0xEF, 0xA6}, {0xFF, 0xF7, 0x9C}, {0xD7, 0xE8, 0x95}, {0xA6, 0xED, 0xAF}, {0xA2, 0xF2, 0xDA},
	{0x99, 0xFF, 0xFC}, {0xDD, 0xDD, 0xDD}, {0x11, 0x11, 0x11}, {0x11, 0x11, 0x11},
}
