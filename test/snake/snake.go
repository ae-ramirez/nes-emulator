package main

import (
	"al/nes-emulator/cpu"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"os"
	"runtime/pprof"
	"time"
	"unsafe"

	"github.com/veandco/go-sdl2/sdl"
)

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")

func main() {
	flag.Parse()
	// run profiling with -cpuprofile flag
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal(err)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	file, err := os.Open("./snake.nes")
	if os.IsNotExist(err) {
		log.Fatal("Could not open snake.nes")
	}
	fstat, err := file.Stat()
	if err != nil {
		panic(err)
	}

	gameCode := make([]uint8, fstat.Size())
	_, err = file.Read(gameCode)
	if err != nil {
		panic(err)
	}

	c := &cpu.CPU{}
	c.Load(gameCode)
	c.Reset()

	// init sdl2
	if err := sdl.Init(sdl.INIT_EVERYTHING); err != nil {
		panic(err)
	}
	defer sdl.Quit()

	window, err := sdl.CreateWindow("test", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		(32 * 10), (32 * 10), sdl.WINDOW_SHOWN)
	if err != nil {
		panic(err)
	}
	defer window.Destroy()

	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)
	if err != nil {
		panic(fmt.Errorf("yo, %e", err))
	}
	defer renderer.Destroy()

	if err = renderer.SetScale(10.0, 10.0); err != nil {
		panic(fmt.Errorf("couldn't set scale, %e", err))
	}

	texture, err := renderer.CreateTexture(sdl.PIXELFORMAT_RGB24, sdl.TEXTUREACCESS_STATIC, 32, 32)
	if err != nil {
		panic(fmt.Errorf("no, %e", err))
	}
	pixels := make([]uint8, 32*32*3)

	c.RunWithCallback(func(*cpu.CPU) {
		// read user input and write it to mem[0xFF]
		handleUserInput(c)

		// update mem[0xFE] with new Random Number
		c.MemWrite(0xfe, uint8(rand.Intn(15)+1))

		// // read mem mapped screen state
		if readScreenState(c, pixels) {
			// render screen state
			texture.Update(nil, unsafe.Pointer(&pixels[0]), 32*3)
			renderer.Clear()
			renderer.Copy(texture, nil, nil)
			renderer.Present()
		}

		time.Sleep(time.Microsecond * 5)
	})
}

func handleUserInput(c *cpu.CPU) {
	for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
		switch t := event.(type) {
		case *sdl.KeyboardEvent:
			key := t.Keysym
			if t.State == sdl.PRESSED {
				if key.Sym == sdl.K_q {
					os.Exit(0)
				} else if key.Sym == sdl.K_f {
					c.MemWrite(0xff, 0x77)
				} else if key.Sym == sdl.K_s {
					c.MemWrite(0xff, 0x73)
				} else if key.Sym == sdl.K_r {
					c.MemWrite(0xff, 0x61)
				} else if key.Sym == sdl.K_t {
					c.MemWrite(0xff, 0x64)
				}
			}
		}
	}

}

func readScreenState(c *cpu.CPU, screenState []uint8) bool {
	frameIdx := 0
	update := false

	for i := 0x0200; i < 0x0600; i++ {
		colorIdx := c.MemRead(uint16(i))
		b1, b2, b3 := colorByte(colorIdx)
		if screenState[frameIdx] != uint8(b1) || screenState[frameIdx+1] != uint8(b2) || screenState[frameIdx+2] != uint8(b3) {
			screenState[frameIdx] = uint8(b1)
			screenState[frameIdx+1] = uint8(b2)
			screenState[frameIdx+2] = uint8(b3)
			update = true
		}
		frameIdx += 3
	}

	return update
}

func colorByte(byte uint8) (r uint8, g uint8, b uint8) {
	switch byte {
	case 0: // black
		return 0x0, 0x0, 0x0
	case 1: // white
		return 0xff, 0xff, 0xff
	case 2, 9:
		return 0x12, 0x65, 0xf0
	case 3, 10:
		return 0xff, 0x0, 0x0
	case 4, 11:
		return 0x0, 0xff, 0x0
	case 5, 12:
		return 0x0, 0x0, 0xff
	case 6, 13:
		return 0x0f, 0x0f, 0xff
	case 7, 14:
		return 0x0f, 0x0f, 0x0
	default:
		return 0x12, 0x0f, 0x82
	}
}
