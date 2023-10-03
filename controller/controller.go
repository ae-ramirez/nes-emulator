package controller

import (
	"os"

	"github.com/veandco/go-sdl2/sdl"
)

type nesControllerBitmask uint8
type ControllerMapping map[sdl.Keycode]nesControllerBitmask

const (
	ButtonA nesControllerBitmask = 1 << iota
	ButtonB
	ButtonSelect
	ButtonStart
	ButtonUp
	ButtonDown
	ButtonLeft
	ButtonRight
)

type Controller struct {
	strobe      bool
	buttons     uint8
	buttonIndex int
	mapping     ControllerMapping
}

func defaultMappingColemakDH() ControllerMapping {
	m := make(map[sdl.Keycode]nesControllerBitmask)
	m[sdl.K_t] = ButtonRight
	m[sdl.K_r] = ButtonLeft
	m[sdl.K_s] = ButtonDown
	m[sdl.K_f] = ButtonUp
	m[sdl.K_g] = ButtonStart
	m[sdl.K_v] = ButtonSelect
	m[sdl.K_i] = ButtonB
	m[sdl.K_n] = ButtonA
	return m
}

func NewControllerInput() *Controller {
	c := &Controller{mapping: defaultMappingColemakDH()}
	return c
}

func (controller *Controller) WriteToController(data uint8) {
	oldStrobeVal := controller.strobe
	controller.strobe = data&1 != 0

	if controller.strobe == true {
		controller.buttonIndex = 0
	} else if oldStrobeVal && !controller.strobe {
		// "lock in" controller input for all buttons
		// see: https://www.nesdev.org/wiki/Standard_controller#Input_($4016_write)
		controller.handleUserInput()
	}
}

func (controller *Controller) ReadControllerData() uint8 {
	if controller.buttonIndex > 7 {
		return 1
	}

	data := (controller.buttons >> uint8(controller.buttonIndex)) & 1
	if !controller.strobe {
		controller.buttonIndex += 1
	}
	return data
}

func (controller *Controller) handleUserInput() {
	if len(controller.mapping) == 0 {
		controller.mapping = defaultMappingColemakDH()
	}
	for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
		switch t := event.(type) {
		case *sdl.QuitEvent:
			os.Exit(0)
		case *sdl.KeyboardEvent:
			if t.Keysym.Sym == sdl.K_q && t.GetType() == sdl.KEYUP {
				os.Exit(0)
			}
			bitmask, isMapped := controller.mapping[t.Keysym.Sym]
			if isMapped {
				if t.State == sdl.PRESSED {
					controller.buttons = controller.buttons | uint8(bitmask)
				} else {
					controller.buttons = controller.buttons & ^(uint8(bitmask))
				}
			}
		}
	}

}
