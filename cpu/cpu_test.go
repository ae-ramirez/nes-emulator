package cpu

import "testing"

func Test0xa9LDAImmediateLoadData(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x05, 0x00})

	if cpu.register_a != 0x05 {
		t.Errorf("Failed to load to register\nregister_s: %#v", cpu.register_a)
	}
	if cpu.status&0b0000_0010 != 0b00 {
		t.Error("Expected zero flag to be unset")
	}
	if cpu.status&0b1000_0000 != 0 {
		t.Error("Expected negative flag to be unset")
	}
}

func Test0xa9LDAZeroFlag(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x00, 0x00})

	if cpu.status&0b0000_0010 != 0b10 {
		t.Errorf("Zero flag not set\ncpu status: %#08b", cpu.status)
	}
}

func Test0xaaTaxMoveAToX(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x0a, 0xaa, 0x00})

	if cpu.register_x != 10 {
		t.Errorf("Unexpected value in register x: %d", cpu.register_x)
	}
}

func Test0xe8INXOverflow(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00})

	if cpu.register_x != 1 {
		t.Errorf("Unexpected value in register x: %d", cpu.register_x)
	}
}

func Test5OpsWorkingTogether(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0xc0, 0xaa, 0xe8, 0x00})

	if cpu.register_x != 0xc1 {
		t.Errorf("Unexpected value in register x: %d", cpu.register_x)
	}
}

func TestLDAFromMemory(t *testing.T) {
	cpu := &CPU{}
	cpu.mem_write(0x10, 0x55)
	cpu.load_and_run([]uint8{0xa5, 0x10, 0x00})

	if cpu.register_a != 0x55 {
		t.Errorf("Unexpected value in register x: %#08b", cpu.register_a)
	}
}

func Test0x85STAMoveAToMemory(t *testing.T) {
	cpu := &CPU{}
	addr := uint8(0x10)
	data := uint8(0x12)
	cpu.load_and_run([]uint8{0xa9, data, 0x85, addr})

	if cpu.mem_read(uint16(addr)) != data {
		t.Errorf("Wrote unexpected value to addr [%#08b]: %#08b", addr, data)
	}
}
