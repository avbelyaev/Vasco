package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestDisplaySimple(t *testing.T) {
	expected := "print(314)"

	tokens := LexExp("(display 314)")
	program := ParseTokens(tokens)
	generated, _ := GenerateTarget(program.GetSubNodes()[0])

	assert.Equal(t, expected, generated)
}
