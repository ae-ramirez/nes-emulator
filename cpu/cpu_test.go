package cpu

import (
	"al/nes-emulator/bus"
	"al/nes-emulator/rom"
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
	cpu.LoadAndRun([]uint8{0xa9, uint8(want), 0x00})

	if cpu.registerA != want {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.registerA)
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
	cpu.LoadAndRun([]uint8{0xa9, 0x00, 0x00})

	if cpu.status&ZeroFlag == 0 {
		t.Error(expectedSetZeroFlag)
	}
}

func Test0xaaTaxMoveAToX(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x0a)
	cpu.LoadAndRun([]uint8{0xa9, want, 0xaa, 0x00})

	if cpu.registerX != want {
		t.Errorf(UnexpectedValInRegister, want, "x", cpu.registerX)
	}
}

func Test0xe8INXOverflow(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x01)
	cpu.LoadAndRun([]uint8{0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00})

	if cpu.registerX != want {
		t.Errorf(UnexpectedValInRegister, want, "x", cpu.registerX)
	}
}

func Test5OpsWorkingTogether(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0xc1)
	cpu.LoadAndRun([]uint8{0xa9, 0xc0, 0xaa, 0xe8, 0x00})

	if cpu.registerX != want {
		t.Errorf(UnexpectedValInRegister, want, "x", cpu.registerX)
	}
}

func TestLDAFromMemory(t *testing.T) {
	cpu := &CPU{}
	want := uint8(0x55)
	cpu.MemWrite(0x10, want)
	cpu.LoadAndRun([]uint8{0xa5, 0x10, 0x00})

	if cpu.registerA != want {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.registerA)
	}
}

func Test0x85STAMoveAToMemory(t *testing.T) {
	cpu := &CPU{}
	addr := uint8(0x10)
	want := uint8(0x12)
	cpu.LoadAndRun([]uint8{0xa9, want, 0x85, addr})

	got := cpu.MemRead(uint16(addr))
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
}

func Test0x29ANDImmediate(t *testing.T) {
	cpu := &CPU{}
	cpu.LoadAndRun([]uint8{0xa9, 0x0f, 0x29, 0xf0, 0x00})

	want := uint8(0x00)
	if cpu.registerA != want {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.registerA)
	}
}

func Test0x29ANDZeroPage(t *testing.T) {
	cpu := &CPU{}
	cpu.LoadAndRun([]uint8{0xa9, 0x0f, 0x85, 0x01, 0xa9, 0xf0, 0x25, 0x01, 0x00})

	want := uint8(0x00)
	if cpu.registerA != 0x00 {
		t.Errorf(UnexpectedValInRegister, want, "a", cpu.registerA)
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
	cpu.LoadAndRun([]uint8{0xa9, val, 0x0a, 0x00})

	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}

	val = uint8(0x40)
	cpu.LoadAndRun([]uint8{0xa9, val, 0x0a, 0x00})

	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}

func Test0x0eASLFromMemory(t *testing.T) {
	cpu := &CPU{}
	val := uint8(0x80)
	addr := uint8(0x0f)
	cpu.LoadAndRun([]uint8{0xa9, val, 0x85, addr, 0xa9, 0x00, 0x06, addr, 0x00})

	got := cpu.MemRead(uint16(addr))
	want := uint8(0x00)
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&CarryFlag == 0 {
		t.Error(expectedSetCarryFlag)
	}

	val = uint8(0x40)
	cpu.LoadAndRun([]uint8{0xa9, val, 0x85, addr, 0xa9, 0x00, 0x06, addr, 0x00})

	got = cpu.MemRead(uint16(addr))
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
	cpu.MemWrite(uint16(addr), val)
	cpu.LoadAndRun([]uint8{0xa9, 0x00, 0x24, addr, 0x00})

	if cpu.status&NegativeFlag != 0 {
		t.Error(expectedUnsetNegativeFlag)
	}
	if cpu.status&OverflowFlag != 0 {
		t.Error(expectedUnsetOverflowFlag)
	}
	if cpu.status&ZeroFlag == 0 {
		t.Error(expectedSetZeroFlag)
	}
}

func Test0x18CLC(t *testing.T) {
	cpu := &CPU{}
	cpu.Load([]uint8{0x18, 0x00})
	cpu.Reset()
	cpu.setStatusFlag(CarryFlag, true)
	cpu.Run()

	if cpu.status&CarryFlag != 0 {
		t.Error(expectedUnsetCarryFlag)
	}
}

func Test0xd8CLD(t *testing.T) {
	cpu := &CPU{}
	cpu.Load([]uint8{0xd8, 0x00})
	cpu.Reset()
	cpu.setStatusFlag(DecimalModeFlag, true)
	cpu.Run()

	if cpu.status&DecimalModeFlag != 0 {
		t.Error(expectedUnsetDecimalModeFlag)
	}
}

func Test0x58CLI(t *testing.T) {
	cpu := &CPU{}
	cpu.Load([]uint8{0x58, 0x00})
	cpu.Reset()
	cpu.setStatusFlag(InterruptDisableFlag, true)
	cpu.Run()

	if cpu.status&InterruptDisableFlag != 0 {
		t.Error(expectedUnsetInterruptDisableFlag)
	}
}

func Test0xb8CLV(t *testing.T) {
	cpu := &CPU{}
	cpu.Load([]uint8{0xb8, 0x00})
	cpu.Reset()
	cpu.setStatusFlag(OverflowFlag, true)
	cpu.Run()

	if cpu.status&OverflowFlag != 0 {
		t.Error(expectedUnsetOverflowFlag)
	}
}

func Test0xc9CMPEqualValues(t *testing.T) {
	cpu := &CPU{}
	val := uint8(26)
	cpu.Load([]uint8{0xc9, val, 0x00})
	cpu.Reset()
	cpu.registerA = val
	cpu.Run()

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
	cpu.Load([]uint8{0xc9, val, 0x00})
	cpu.Reset()
	cpu.registerA = 48
	cpu.Run()

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
	cpu.Load([]uint8{0xc9, val, 0x00})
	cpu.Reset()
	cpu.registerA = 8
	cpu.Run()

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
	cpu.MemWrite(uint16(addr), val)
	cpu.Load([]uint8{0xc6, addr, 0x00})
	cpu.Reset()
	cpu.Run()

	got := cpu.MemRead(uint16(addr))
	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
}

func Test0xcaDEXDecrementToZero(t *testing.T) {
	cpu := &CPU{}
	cpu.Load([]uint8{0xca, 0x00})
	cpu.Reset()
	cpu.registerX = 0x01
	cpu.Run()

	got := cpu.registerX
	want := uint8(0)

	if got != want {
		t.Errorf(UnexpectedValInRegister, want, "x", got)
	}
}

func Test0x88DEYDecrementToNegative(t *testing.T) {
	cpu := &CPU{}
	cpu.Load([]uint8{0x88, 0x00})
	cpu.Reset()
	cpu.registerY = 0xff
	cpu.Run()

	got := cpu.registerY
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
	cpu.Load([]uint8{0x49, val, 0x00})
	cpu.Reset()
	cpu.registerA = 0b0000_1111
	cpu.Run()

	got := cpu.registerA
	want := uint8(0b1111_1111)

	if got != want {
		t.Errorf(UnexpectedValInRegister, want, "a", got)
	}
}

func Test0xe6INCIncrementFromMemory(t *testing.T) {
	cpu := &CPU{}
	addr := uint8(0x0f)
	val := uint8(0xfe)
	cpu.MemWrite(uint16(addr), val)
	cpu.LoadAndRun([]uint8{0xe6, addr, 0x00})

	got := cpu.MemRead(uint16(addr))
	want := uint8(0xff)

	if got != want {
		t.Errorf(UnexpectedValInAddr, want, got)
	}
	if cpu.status&NegativeFlag == 0 {
		t.Error(expectedSetNegativeFlag)
	}
}

func TestTraceFormat(t *testing.T) {
	programCounter := uint16(0x64)

	cpu := &CPU{}
	rom := &rom.Rom{}
	rom.InitMemory()
	cpu.Bus.SetRom(rom)
	cpu.LoadIntoLocation([]uint8{0xa2, 0x01, 0xca, 0x88, 0x00}, programCounter)
	cpu.Reset()
	cpu.SetProgramCounter(programCounter)
	cpu.registerA = 1
	cpu.registerX = 2
	cpu.registerY = 3

	var result []string
	cpu.RunWithCallback(func(c *CPU) {
		trace := Trace(c)
		result = append(result, trace)
	})

	expected := "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD"
	if result[0] != expected {
		t.Errorf("result:\n%s\ndoes not equal expected result:\n%s\n", result[0], expected)
	}

	expected = "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD"
	if result[1] != expected {
		t.Errorf("result:\n%s\ndoes not equal expected result:\n%s", result[1], expected)
	}

	expected = "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD"
	if result[2] != expected {
		t.Errorf("result:\n%s\ndoes not equal expected result:\n%s", result[2], expected)
	}
}

func TestTraceMemoryAccess(t *testing.T) {
	rom := rom.Rom{}
	rom.InitMemory()
	bus := bus.Bus{}
	bus.SetRom(&rom)
	// instruction
	bus.MemWrite(100, 0x11)
	bus.MemWrite(101, 0x33)
	// data
	bus.MemWrite(0x33, 00)
	bus.MemWrite(0x34, 04)
	// target cell
	bus.MemWrite(0x400, 0xAA)

	cpu := &CPU{Bus: bus}
	cpu.Reset()
	cpu.SetProgramCounter(0x64)
	cpu.registerY = 0

	var result []string
	cpu.RunWithCallback(func(c *CPU) {
		trace := Trace(c)
		result = append(result, trace)
	})

	expected := "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD"
	if result[0] != expected {
		t.Errorf("result:\n%s\ndoes not equal expected result:\n%s", result[0], expected)
	}
}
