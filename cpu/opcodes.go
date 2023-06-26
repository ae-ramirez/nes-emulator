package cpu

type OpCode struct {
	code     uint8
	mnemonic string
	len      uint8
	cycles   uint8
	mode     AddressingMode
}

func newOpCode(code uint8, mnemonic string, len uint8, cycles uint8, mode AddressingMode) *OpCode {
	return &OpCode{
		code:     code,
		mnemonic: mnemonic,
		len:      len,
		cycles:   cycles,
		mode:     mode,
	}
}

func OpCodesMapFunc() func() map[uint8]*OpCode {
	var m map[uint8]*OpCode

	OpCodes := []*OpCode{
		newOpCode(0x00, "BRK", 1, 7, NoneAddressing),
		newOpCode(0xaa, "TAX", 1, 7, NoneAddressing),
		newOpCode(0xe8, "INX", 1, 2, NoneAddressing),

		newOpCode(0xa9, "LDA", 2, 2, Immediate),
		newOpCode(0xa5, "LDA", 2, 3, ZeroPage),
		newOpCode(0xb5, "LDA", 2, 4, ZeroPage_X),
		newOpCode(0xad, "LDA", 3, 4, Absolute),
		newOpCode(0xbd, "LDA", 3, 4 /* +1 if page crossed */, Absolute_X),
		newOpCode(0xb9, "LDA", 3, 4 /* +1 if page crossed */, Absolute_Y),
		newOpCode(0xa1, "LDA", 2, 6, Indirect_X),
		newOpCode(0xb1, "LDA", 2, 5 /* +1 if page crossed */, Indirect_Y),

		newOpCode(0x85, "STA", 2, 3, ZeroPage),
		newOpCode(0x95, "STA", 2, 4, ZeroPage_X),
		newOpCode(0x8d, "STA", 3, 4, Absolute),
		newOpCode(0x9d, "STA", 3, 5, Absolute_X),
		newOpCode(0x99, "STA", 3, 5, Absolute_Y),
		newOpCode(0x81, "STA", 2, 6, Indirect_X),
		newOpCode(0x91, "STA", 2, 6, Indirect_Y),
	}

	return func() map[uint8]*OpCode {
		if m != nil {
			return m
		}

		m := make(map[uint8]*OpCode)
		for _, opcode := range OpCodes {
			m[opcode.code] = opcode
		}
		return m
	}
}

var OpCodes = OpCodesMapFunc()
