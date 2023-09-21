package ppu

import (
	"al/nes-emulator/rom"
	"log"
	"testing"
)

func TestPPUVramWrites(t *testing.T) {
	ppu := PPU{}
	ppu.WriteToPPUAddress(0x23)
	ppu.WriteToPPUAddress(0x05)
	ppu.writeData(0x66)

	if ppu.Vram[0x0305] != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", ppu.Vram[0x0305])
	}
}

func TestPPUVramReads(t *testing.T) {
    ppu := PPU{}
	ppu.WriteToControl(0)
	ppu.Vram[0x0305] = 0x66

	ppu.WriteToPPUAddress(0x23)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	addr := ppu.addr.get()
	if addr != 0x2306 {
		log.Fatalf("expected 0x2306 but got: %04X", addr)
	}
	val := ppu.ReadData()
	if val != 0x66 {
		log.Fatalf("expected 0x66 but got: %02X", val)
	}
}

func TestPPUVramReadsCrossPage(t *testing.T) {
	ppu := PPU{}
	ppu.WriteToControl(0)
	ppu.Vram[0x01ff] = 0x66
	ppu.Vram[0x0200] = 0x77

	ppu.WriteToPPUAddress(0x21)
	ppu.WriteToPPUAddress(0xff)

	ppu.ReadData()
	val1 := ppu.ReadData()
	if val1 != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val1)
	}
	val2 := ppu.ReadData()
	if val2 != 0x77 {
		t.Fatalf("expected 0x77 but got: %02X", val2)
	}
}

func TestPPUVramReadsStep32(t *testing.T) {
	t.Log("starting test im looking at")
    ppu := PPU{}
	ppu.WriteToControl(0b100)
	ppu.Vram[0x01ff] = 0x66;
	ppu.Vram[0x01ff + 32] = 0x77;
	ppu.Vram[0x01ff + 64] = 0x88;

	ppu.WriteToPPUAddress(0x21)
	ppu.WriteToPPUAddress(0xff)


	ppu.ReadData()
	val1 := ppu.ReadData()
	if val1 != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val1)
	}
	val2 := ppu.ReadData()
	if val2 != 0x77 {
		t.Fatalf("expected 0x77 but got: %02X", val2)
	}
	val3 := ppu.ReadData()
	if val3 != 0x88 {
		t.Fatalf("expected 0x88 but got: %02X", val3)
	}
}

func TestVramHorizontalMirror(t *testing.T) {
    ppu := PPU{mirroring: rom.HORIZONTAL}
	ppu.WriteToPPUAddress(0x24)
	ppu.WriteToPPUAddress(0x05)

	ppu.WriteToData(0x66)

	ppu.WriteToPPUAddress(0x28)
	ppu.WriteToPPUAddress(0x05)

	ppu.WriteToData(0x77)

	ppu.WriteToPPUAddress(0x20)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val1 := ppu.ReadData()
	if val1 != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val1)
	}

	ppu.WriteToPPUAddress(0x2c)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val2 := ppu.ReadData()
	if val2 != 0x77 {
		t.Fatalf("expected 0x77 but got: %02X", val2)
	}
}

func TestVramVerticalMirror(t *testing.T) {
    ppu := PPU{mirroring: rom.VERTICAL}
	ppu.WriteToPPUAddress(0x20)
	ppu.WriteToPPUAddress(0x05)

	ppu.WriteToData(0x66)

	ppu.WriteToPPUAddress(0x2c)
	ppu.WriteToPPUAddress(0x05)

	ppu.WriteToData(0x77)

	ppu.WriteToPPUAddress(0x28)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val1 := ppu.ReadData()
	if val1 != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val1)
	}

	ppu.WriteToPPUAddress(0x24)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val2 := ppu.ReadData()
	if val2 != 0x77 {
		t.Fatalf("expected 0x77 but got: %02X", val2)
	}
}

func TestReadStatusResetsLatch(t *testing.T) {
    ppu := PPU{ChrRom: make([]uint8, 1400)}
	ppu.Vram[0x0305] = 0x66

	ppu.WriteToPPUAddress(0x21)
	ppu.WriteToPPUAddress(0x23)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val1 := ppu.ReadData()
	if val1 == 0x66 {
		t.Fatal("ReadData should not return 0x66")
	}

	ppu.ReadStatus()

	ppu.WriteToPPUAddress(0x23)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val2 := ppu.ReadData()
	if val2 != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val2)
	}
}

func TestPPUVramMirroring(t *testing.T) {
    ppu := PPU{}
	ppu.WriteToControl(0)
	ppu.Vram[0x0305] = 0x66

	ppu.WriteToPPUAddress(0x63)
	ppu.WriteToPPUAddress(0x05)

	ppu.ReadData()
	val := ppu.ReadData()
	if val != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val)
	}
}

func TestReadStatusResetsVblank(t *testing.T) {
    ppu := PPU{}
	ppu.status.setFlag(VerticalBlank, true)

	status := ppu.ReadStatus()

	if status >> 7 != 1 {
		t.Fatal("Status does not have vertical blank flag set")
	}
	status2 := ppu.ReadStatus()
	if status2 >> 7 != 0 {
		t.Fatal("Status has vertical blank flag set")
	}
}

func TestOamReadWrite(t *testing.T) {
    ppu := PPU{}
	ppu.WriteToOAMAddress(0x10)
	ppu.WriteToOAMData(0x66)
	ppu.WriteToOAMData(0x77)

	ppu.WriteToOAMAddress(0x10)
	val1 := ppu.ReadOAMData()
	if val1 != 0x66 {
		t.Fatalf("expected 0x66 but got: %02X", val1)
	}

	ppu.WriteToOAMAddress(0x11)
	val2 := ppu.ReadOAMData()
	if val2 != 0x77 {
		t.Fatalf("expected 0x77 but got: %02X", val2)
	}
}

func TestOamDma(t *testing.T) {
    ppu := PPU{}
	data := make([]uint8, 256)
	for i := 0; i < 255; i++ {
		data[i] = 0x66
	}
	data[0] = 0x77
	data[255] = 0x88

	ppu.WriteToOAMAddress(0x10)
	ppu.CopyToOamData(data)

	ppu.WriteToOAMAddress(0x0f)
	val := ppu.ReadOAMData()
	if val != 0x88 {
		t.Fatalf("expected 0x88 but got: %02X", val)
	}
}
