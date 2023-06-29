package cpu

import (
	"testing"
)

const (
	expectedSetCarryFlag              = "Expected carry flag to be set"
	expectedUnsetCarryFlag            = "Expected carry flag to be unset"
	expectedSetZeroFlag               = "Expected zero flag to be set"
	expectedUnsetZeroFlag             = "Expected zero flag to be unset"
	expectedSetInterruptDisableFlag   = "Expected interrupt disable flag to be set"
	expectedUnsetInterruptDisableFlag = "Expected interrupt disable flag to be unset"
	expectedSetDecimalModeFlag        = "Expected decimal mode flag to be set"
	expectedUnsetDecimalModeFlag      = "Expected decimal mode flag to be unset"
	expectedSetBreakCommandFlag       = "Expected break command flag to be set"
	expectedUnsetBreakCommandFlag     = "Expected break command flag to be unset"
	expectedSetOverflowFlag           = "Expected overflow flag to be set"
	expectedUnsetOverflowFlag         = "Expected overflow flag to be unset"
	expectedSetNegativeFlag           = "Expected negative flag to be set"
	expectedUnsetNegativeFlag         = "Expected negative flag to be unset"

	UnexpectedValInRegister = "Expected %#02x in register %v, got: %#02x"
	UnexpectedValInAddr     = "Expected %#02x in [addr], got: %#02x"
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
	cpu.setStatusFlag(CarryFlag, true)
	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}

	cpu.setStatusFlag(CarryFlag, false)
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

func Test0x24BITZeroPage(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0xff)
	addr := uint8(0x00)
	cpu.mem_write(uint16(addr), val)
	cpu.load_and_run([]uint8{0xa9, 0x00, 0x24, addr, 0x00})

	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
	if cpu.status&OverflowFlag == 0 {
		t.Error(expectedSetOverflowFlag)
	}
	if cpu.status&ZeroFlag == 0 {
		t.Error(expectedSetZeroFlag)
	}
}

func Test0x18CLC(t *testing.T) {
	cpu := &CPU{}
	cpu.load([]uint8{0x18, 0x00})
	cpu.reset()
	cpu.setStatusFlag(CarryFlag, true)
	cpu.run()

	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}

func Test0xd8CLD(t *testing.T) {
	cpu := &CPU{}
	cpu.load([]uint8{0xd8, 0x00})
	cpu.reset()
	cpu.setStatusFlag(DecimalModeFlag, true)
	cpu.run()

	if cpu.status&DecimalModeFlag != 0 {
		t.Error(expectedUnsetDecimalModeFlag)
	}
}

func Test0x58CLI(t *testing.T) {
	cpu := &CPU{}
	cpu.load([]uint8{0x58, 0x00})
	cpu.reset()
	cpu.setStatusFlag(InterruptDisableFlag, true)
	cpu.run()

	if cpu.status&InterruptDisableFlag != 0 {
		t.Error(expectedUnsetInterruptDisableFlag)
	}
}

func Test0xb8CLV(t *testing.T) {
	cpu := &CPU{}
	cpu.load([]uint8{0xb8, 0x00})
	cpu.reset()
	cpu.setStatusFlag(OverflowFlag, true)
	cpu.run()

	if cpu.status&OverflowFlag != 0 {
		t.Error(expectedUnsetOverflowFlag)
	}
}

func Test0xc9CMPEqualValues(t *testing.T) {
	cpu := &CPU{}
	val := uint8(26)
	cpu.load([]uint8{0xc9, val, 0x00})
	cpu.reset()
	cpu.register_a = val
	cpu.run()

	if cpu.status&ZeroFlag == 0 {
		t.Error(expectedSetZeroFlag)
	}
	if cpu.status&NegativeFlag != 0 {
		t.Error(expectedUnsetNegativeFlag)
	}
	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}
}

func Test0xc9CMPGreaterToLessThan(t *testing.T) {
	cpu := &CPU{}
	val := uint8(26)
	cpu.load([]uint8{0xc9, val, 0x00})
	cpu.reset()
	cpu.register_a = 48
	cpu.run()

	if cpu.status&ZeroFlag != 0 {
		t.Error(expectedUnsetZeroFlag)
	}
	if cpu.status&NegativeFlag != 0 {
		t.Error(expectedUnsetNegativeFlag)
	}
	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}
}

func Test0xc9CMPLessThanToGreater(t *testing.T) {
	cpu := &CPU{}
	val := uint8(26)
	cpu.load([]uint8{0xc9, val, 0x00})
	cpu.reset()
	cpu.register_a = 8
	cpu.run()

	if cpu.status&ZeroFlag != 0 {
		t.Error(expectedUnsetZeroFlag)
	}
	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}

func Test0xc6DECOverflow(t *testing.T) {
	cpu := &CPU{}
	addr := uint8(0x0f)
	val := uint8(0)
	want := uint8(255)
	cpu.mem_write(uint16(addr), val)
	cpu.load([]uint8{0xc6, addr, 0x00})
	cpu.reset()
	cpu.run()

	got := cpu.mem_read(uint16(addr))
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
}

func Test0xcaDEXDecrementToZero(t *testing.T) {
	cpu := &CPU{}
	cpu.load([]uint8{0xca, 0x00})
	cpu.reset()
	cpu.register_x = 0x01
	cpu.run()

	got := cpu.register_x
	want := uint8(0)

	if got != want {
		t.Errorf(UnexpectedValInRegister, want, "x", got)
	}
}

func Test0x88DEYDecrementToNegative(t *testing.T) {
	cpu := &CPU{}
	cpu.load([]uint8{0x88, 0x00})
	cpu.reset()
	cpu.register_y = 0xff
	cpu.run()

	got := cpu.register_y
	want := uint8(0xfe)

	if got != want {
		t.Errorf(UnexpectedValInRegister, want, "y", got)
	}
	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
}

func Test0x49EORImmediateValue(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0b1111_0000)
	cpu.load([]uint8{0x49, val, 0x00})
	cpu.reset()
	cpu.register_a = 0b0000_1111
	cpu.run()

	got := cpu.register_a
	want := uint8(0b1111_1111)

	if got != want {
		t.Errorf(UnexpectedValInRegister, want, "a", got)
	}
}

func Test0xe6INCIncrementFromMemory(t *testing.T) {
	cpu := &CPU{}
	addr := uint8(0x0f)
	val := uint8(0xfe)
	cpu.mem_write(uint16(addr), val)
	cpu.load_and_run([]uint8{0xe6, addr, 0x00})

	got := cpu.mem_read(uint16(addr))
	want := uint8(0xff)

	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
}
