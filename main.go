package main

import (
	"al/nes-emulator/cpu"
	"al/nes-emulator/render"
	"al/nes-emulator/rom"
	"flag"
	"log"
	"os"
	"time"
	"unsafe"

	"github.com/veandco/go-sdl2/sdl"
)

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")

func main() {
	// init sdl2
	if err := sdl.Init(sdl.INIT_EVERYTHING); err != nil {
		log.Fatalf("failed to initialize sdl: %v", err)
	}
	defer sdl.Quit()

	window, err := sdl.CreateWindow("test", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		(256 * 3), (240 * 3), sdl.WINDOW_SHOWN)
	if err != nil {
		log.Fatal(err)
	}
	defer window.Destroy()

	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)
	if err != nil {
		log.Fatal(err)
	}
	defer renderer.Destroy()

	if err = renderer.SetScale(3.0, 3.0); err != nil {
		log.Fatal(err)
	}

	texture, err := renderer.CreateTexture(sdl.PIXELFORMAT_RGB24, sdl.TEXTUREACCESS_STATIC, 256, 240)
	if err != nil {
		log.Fatal(err)
	}

	// load rom file
	filename := os.Args[1]
	file, err := os.Open(filename)
	if os.IsNotExist(err) {
		log.Fatalf("Could not open %s\n", filename)
	}
	fstat, err := file.Stat()
	if err != nil {
		panic(err)
	}

	romData := make([]uint8, fstat.Size())
	_, err = file.Read(romData)
	if err != nil {
		panic(err)
	}
	rom := &rom.Rom{}
	if err := rom.Init(romData); err != nil {
		log.Fatal(err)
	}

	frame := &render.Frame{}

	c := &cpu.CPU{}
	c.Bus.SetRom(rom)
	c.Bus.SetCallback(func() {
		render.Render(&c.Bus.Ppu, frame)
		renderer.Clear()
		texture.Update(nil, unsafe.Pointer(&frame.Data[0]), 256*3)
		renderer.Copy(texture, nil, nil)
		renderer.Present()
		time.Sleep(time.Millisecond * 10)
	})
	c.Reset()
	c.Run()
}
