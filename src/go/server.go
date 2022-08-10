package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"net/http"
	"os"
	"sync"
	"time"

	"go.bug.st/serial"
)

type point struct {
	X float32 `json:"x"`
	Y float32 `json:"y"`
}

type state struct {
	sync.Mutex
	unparsed       []byte
	Newpoints      []point `json:"Newpoints"`
	SerialIsOnline bool    `json:"SerialIsOnline"`
}

var State state

func mock_update_points() {
	State.SerialIsOnline = true
	var t float32 = 0
	var step float32 = 0.2
	for {
		time.Sleep(time.Millisecond * 10)
		State.Lock()
		State.Newpoints = append(State.Newpoints, point{t, float32(math.Sin(float64(t)))})
		State.Unlock()
		t += step
	}
}

func update_points() {
	var err error
	var ser serial.Port
	buf := make([]byte, 4100)
	for {
		State.Lock()
		cond := State.SerialIsOnline
		State.Unlock()

		if !cond {
			ser, err = init_serial()
			if err != nil {
				log.Println("Error al abrir el serial:", err)
				time.Sleep(time.Second)
				continue
			}
			log.Println("Conectando al serial...")
		}
		n, err := ser.Read(buf)
		if err != nil {
			log.Println("Error al leer el serial:", err)
			ser.Close()
			State.Lock()
			State.SerialIsOnline = false
			State.Unlock()
			time.Sleep(time.Second)
			continue
		}
		State.Lock()
		State.SerialIsOnline = true
		State.unparsed = append(State.unparsed, buf[0:n]...)
		State.Newpoints, State.unparsed = scanPoints(State.Newpoints, State.unparsed)
		State.Unlock()
	}
}

func init_serial() (serial.Port, error) {
	ports, err := serial.GetPortsList()
	if err != nil {
		return nil, err
	}
	if len(ports) == 0 {
		return nil, fmt.Errorf("No se encontraron puertos disponibles")
	}

	// Open the first serial port detected at 9600bps N81
	mode := &serial.Mode{
		BaudRate: 9600,
		Parity:   serial.NoParity,
		DataBits: 8,
		StopBits: serial.OneStopBit,
	}
	return serial.Open(ports[0], mode)
}

func handler(w http.ResponseWriter, req *http.Request) {
	State.Lock()
	b, err := json.Marshal(&State)
	if err != nil {
		log.Println(err)
	}
	_, err = w.Write(b)
	if err != nil {
		log.Println(err)
	}
	State.Newpoints = State.Newpoints[0:0]
	State.Unlock()
}

// pure function
func scanPoints(bag []point, input []byte) (out []point, rest []byte) {
	var x, y float32
	reader := bytes.NewReader(input)
	_, err := fmt.Fscanf(reader, "%f,%f\r\n", &x, &y)
	if err == nil {
		rest, _ = io.ReadAll(reader)
		return scanPoints(append(bag, point{X: x, Y: y}), rest)
	}
	i := bytes.IndexByte(input, '\n')
	if i != -1 {
		return scanPoints(bag, input[(i+1):])
	}
	return bag, []byte{}
}

func main() {
	if len(os.Args) > 1 && os.Args[1] == "-mock" {
		go mock_update_points()
	} else {
		go update_points()
	}

	http.HandleFunc("/newpoints", handler)
	http.Handle("/", http.FileServer(http.Dir("./static")))

	log.Println("Empezando el server en http://localhost:8080/")
	err := http.ListenAndServe("localhost:8080", nil)
	if err != nil {
		log.Printf("Error en el server: %s", err)
	}
}
