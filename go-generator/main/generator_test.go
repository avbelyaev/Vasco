package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestDisplaySimple(t *testing.T) {
	expected := `
package main

import "fmt"

func main() {
fmt.Println(314)

}
`
	tokens := LexExp("(display 314)")
	program := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}

func TestDisplaySumInts(t *testing.T) {
	expected := `
package main

import "fmt"

func main() {
fmt.Println(111 + 222)

}
`
	tokens := LexExp("(display (+ 111 222))")
	program := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}

func TestPrinter314(t *testing.T) {
	expected := `
package main

import "fmt"

func main() {
var printer = func(foo int) {
fmt.Println(foo + 1)
}

var pi = 314

printer(pi)

}
`
	tokens := LexExp(`
		(define (printer foo) 
			(display (+ foo 1)))

		(define pi 314) 

		(printer pi)
	`)
	program := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}
