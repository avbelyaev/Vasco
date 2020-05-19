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
fmt.Print(314)

// avoiding unused import of "fmt"
fmt.Println()
}
`
	tokens := LexExp("(display 314)")
	program, _ := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}

func TestDisplaySumInts(t *testing.T) {
	expected := `
package main

import "fmt"

func main() {
fmt.Print(111 + 222)

// avoiding unused import of "fmt"
fmt.Println()
}
`
	tokens := LexExp("(display (+ 111 222))")
	program, _ := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}

func TestPrinter314(t *testing.T) {
	expected := `
package main

import "fmt"

func main() {
var printer = func(foo int) {
fmt.Print(foo + 1)
}

var pi = 314

printer(pi)

// avoiding unused import of "fmt"
fmt.Println()
}
`
	tokens := LexExp(`
		(define (printer foo) 
			(display (+ foo 1)))

		(define pi 314) 

		(printer pi)
	`)
	program, _ := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}

func TestSimpleIfThenElse(t *testing.T) {
	expected := `
package main

import "fmt"

func main() {
var myAbs = func(x int) {
if x < 0 {
	fmt.Print(111)
} else {
	fmt.Println(222)
}
}

myAbs(1)

// avoiding unused import of "fmt"
fmt.Println()
}
`
	tokens := LexExp(`
		(define (myAbs x)
  			(if (< x 0) (display 111) (displayln 222)))

		(myAbs 1)
	`)
	program, _ := ParseTokens(tokens)
	generated, _ := GenerateForEachRoot(program)

	assert.Equal(t, expected, generated)
}

// TODO test mangle name my-abs -> my_abs
