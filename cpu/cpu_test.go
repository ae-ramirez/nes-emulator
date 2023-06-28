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
		t.Errorf("Wrote unexpected value to addr: %#08b", data)
	}
}

func Test0x29ANDImmediate(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x0f, 0x29, 0xf0, 0x00})

	if cpu.register_a != 0x00 {
		t.Errorf("Expected 0x00 in register, found: %#08b", cpu.register_a)
	}
}

func Test0x29ANDZeroPage(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x0f, 0x85, 0x01, 0xa9, 0xf0, 0x25, 0x01, 0x00})

	if cpu.register_a != 0x00 {
		t.Errorf("Expected 0x00 in register, found: %#08b", cpu.register_a)
	}
}

func TestSetCarryFlag(t *testing.T) {
	cpu := &CPU{}
	cpu.setCarryFlag(true)
	if cpu.status&0b0000_0001 == 0 {
		t.Errorf("Expected carry flag to be set, status: %#08b", cpu.status)
	}
	cpu.setCarryFlag(false)
	if cpu.status&0b0000_0001 != 0 {
		t.Errorf("Expected carry flag to be unset, status: %#08b", cpu.status)
	}
}

func Test0x0aASLCarryFlag(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0x80)
	cpu.load_and_run([]uint8{0xa9, val, 0x0a, 0x00})

	if cpu.status&0b0000_0001 == 0 {
		t.Errorf("Expected carry flag to be set, cpu status: %#08b", cpu.status)
	}

	val = uint8(0x40)
	cpu.load_and_run([]uint8{0xa9, val, 0x0a, 0x00})

	if cpu.status&0b0000_0001 != 0 {
		t.Errorf("Expected carry flag to be unset, cpu status: %#08b", cpu.status)
	}
}

func Test0x0eASLFromMemory(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0x80)
	addr := uint8(0x0f)
	cpu.load_and_run([]uint8{0xa9, val, 0x85, addr, 0xa9, 0x00, 0x06, addr, 0x00})

	newVal := cpu.mem_read(uint16(addr))
	if newVal != 0 {
		t.Errorf("Expected 0x00 at [addr], instead found: %#08b", newVal)
	}
	if cpu.status&0b0000_0001 == 0 {
		t.Errorf("Expected carry flag to be set, cpu status: %#08b", cpu.status)
	}

	val = uint8(0x40)
	cpu.load_and_run([]uint8{0xa9, val, 0x85, addr, 0xa9, 0x00, 0x06, addr, 0x00})

	newVal = cpu.mem_read(uint16(addr))
	if newVal != 0x80 {
		t.Errorf("Expected 0x80 at [addr], instead found: %#08b", newVal)
	}
	if cpu.status&0b0000_0001  != 0 {
		t.Errorf("Expected carry flag to be unset, cpu status: %#08b", cpu.status)
	}
}
