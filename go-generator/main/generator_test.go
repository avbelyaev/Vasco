package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestDisplaySimple(t *testing.T) {
	expected := "print(314)"

	tokens := LexExp("(display 314)")
	program := ParseTokens(tokens)
	generated, _ := GenerateCode(program.Children()[0])

	assert.Equal(t, expected, generated)
}

func TestDisplaySumInts(t *testing.T) {
	expected := "print(111 + 222)"

	tokens := LexExp("(display (+ 111 222))")
	program := ParseTokens(tokens)
	generated, _ := GenerateCode(program.Children()[0])

	assert.Equal(t, expected, generated)
}
