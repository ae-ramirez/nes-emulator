package main

import (
	"al/nes-emulator/cpu"
	"al/nes-emulator/rom"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("./nestest.nes")
	if os.IsNotExist(err) {
		log.Fatal("Could not open nestest.nes")
	}
	fstat, err := file.Stat()
	if err != nil {
		panic(err)
	}

	nestestCode := make([]uint8, fstat.Size())
	_, err = file.Read(nestestCode)
	if err != nil {
		panic(err)
	}

	c := &cpu.CPU{}
	c.Bus.SetCallback(func() {})
	rom := &rom.Rom{}
	if err := rom.Init(nestestCode); err != nil {
		log.Fatalf("Could not initialize rom: %s", err.Error())
	}
	c.Bus.SetRom(rom)
	c.Reset()
	c.SetProgramCounter(0xc000)

	c.RunWithCallback(func(c *cpu.CPU) {
		fmt.Println(cpu.Trace(c))
	})
}
