package cpu

import "testing"

const (
	expectedSetZeroFlag       = "Expected zero flag to be set"
	expectedUnsetZeroFlag     = "Expected zero flag to be unset"
	expectedSetNegativeFlag   = "Expected negative flag to be set"
	expectedUnsetNegativeFlag = "Expected negative flag to be unset"
	expectedSetCarryFlag      = "Expected carry flag to be set"
	expectedUnsetCarryFlag    = "Expected carry flag to be unset"
	UnexpectedValInRegister   = "Expected %#02x in register %v, got: %#02x"
	UnexpectedValInAddr       = "Expected %#02x in [addr], got: %#02x"
)

func Test0xa9LDAImmediateLoadData(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x05)
	cpu.load_and_run([]uint8{0xa9, uint8(want), 0x00})

	if cpu.register_a != want {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.register_a)
	}
	if cpu.status&ZeroFlag != 0 {
		t.Error(expectedUnsetZeroFlag)
	}
	if cpu.status&NegativeFlag != 0 {
		t.Error(expectedUnsetNegativeFlag)
	}
}

func Test0xa9LDAZeroFlag(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x00, 0x00})

	if cpu.status&ZeroFlag == 0 {
		t.Error(expectedSetZeroFlag)
	}
}

func Test0xaaTaxMoveAToX(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x0a)
	cpu.load_and_run([]uint8{0xa9, want, 0xaa, 0x00})

	if cpu.register_x != want {
		t.Errorf(UnexpectedValInRegister, want, "x", cpu.register_x)
	}
}

func Test0xe8INXOverflow(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x01)
	cpu.load_and_run([]uint8{0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00})

	if cpu.register_x != want {
		t.Errorf(UnexpectedValInRegister, want, "x", cpu.register_x)
	}
}

func Test5OpsWorkingTogether(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0xc1)
	cpu.load_and_run([]uint8{0xa9, 0xc0, 0xaa, 0xe8, 0x00})

	if cpu.register_x != want {
		t.Errorf(UnexpectedValInRegister, want, "x", cpu.register_x)
	}
}

func TestLDAFromMemory(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x55)
	cpu.mem_write(0x10, want)
	cpu.load_and_run([]uint8{0xa5, 0x10, 0x00})

	if cpu.register_a != want {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.register_a)
	}
}

func Test0x85STAMoveAToMemory(t *testing.T) {
	cpu := &CPU{}
	addr := uint8(0x10)
	want := uint8(0x12)
	cpu.load_and_run([]uint8{0xa9, want, 0x85, addr})

	got := cpu.mem_read(uint16(addr))
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
}

func Test0x29ANDImmediate(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x0f, 0x29, 0xf0, 0x00})

	want := uint8(0x00)
	if cpu.register_a != want {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.register_a)
	}
}

func Test0x29ANDZeroPage(t *testing.T) {
	cpu := &CPU{}
	cpu.load_and_run([]uint8{0xa9, 0x0f, 0x85, 0x01, 0xa9, 0xf0, 0x25, 0x01, 0x00})

	want := uint8(0x00)
	if cpu.register_a != 0x00 {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.register_a)
	}
}

func TestSetCarryFlag(t *testing.T) {
	cpu := &CPU{}
	cpu.setCarryFlag(true)
	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}

	cpu.setCarryFlag(false)
	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}

func Test0x0aASLCarryFlag(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0x80)
	cpu.load_and_run([]uint8{0xa9, val, 0x0a, 0x00})

	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}

	val = uint8(0x40)
	cpu.load_and_run([]uint8{0xa9, val, 0x0a, 0x00})

	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}

func Test0x0eASLFromMemory(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0x80)
	addr := uint8(0x0f)
	cpu.load_and_run([]uint8{0xa9, val, 0x85, addr, 0xa9, 0x00, 0x06, addr, 0x00})

	got := cpu.mem_read(uint16(addr))
	want := uint8(0x00)
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}

	val = uint8(0x40)
	cpu.load_and_run([]uint8{0xa9, val, 0x85, addr, 0xa9, 0x00, 0x06, addr, 0x00})

	got = cpu.mem_read(uint16(addr))
	want = uint8(0x80)
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}
