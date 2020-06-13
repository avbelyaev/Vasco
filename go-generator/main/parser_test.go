package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
	. "wascho/go-generator/main/node"
)

func checkProgram(program *Program, expectedProgram *Program, t *testing.T) {
	for i, subNode := range program.Children() {
		expectedSubNode := expectedProgram.Children()[i]
		assert.Equal(t, expectedSubNode.Type(), subNode.Type())
	}
}

func TestParseSingleExp(t *testing.T) {
	tokens := LexExp("(+ 5 3)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewAddExp(NewIntLiteral(5), NewIntLiteral(3)))
	checkProgram(program, expectedProgram, t)
}

func TestNestedExp(t *testing.T) {
	tokens := LexExp("(* (- 8 (+ 5 6)) 52)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewMulExp(NewSubExp(NewIntLiteral(8), NewAddExp(NewIntLiteral(5), NewIntLiteral(6))), NewIntLiteral(52)))
	checkProgram(program, expectedProgram, t)
}

func TestMultipleExp(t *testing.T) {
	tokens := LexExp("(+ 3 4)\n(+ 5 6)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewAddExp(NewIntLiteral(3), NewIntLiteral(4)), NewAddExp(NewIntLiteral(5), NewIntLiteral(6)))
	checkProgram(program, expectedProgram, t)
}

func TestParseFloatExp(t *testing.T) {
	tokens := LexExp("(/ 2.718 3.145)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewDivExp(NewFloatLiteral(2.718), NewFloatLiteral(3.145)))
	checkProgram(program, expectedProgram, t)
}

func TestParseLtCmpExp(t *testing.T) {
	tokens := LexExp("(<= (< 7 1) 10)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewLteExp(NewLtExp(NewIntLiteral(7), NewIntLiteral(1)), NewIntLiteral(10)))
	checkProgram(program, expectedProgram, t)
}

func TestParseGtCmpExp(t *testing.T) {
	tokens := LexExp("(>= (> 6 2) 9)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewGteExp(NewGtExp(NewIntLiteral(6), NewIntLiteral(2)), NewIntLiteral(9)))
	checkProgram(program, expectedProgram, t)
}

func TestEqExp(t *testing.T) {
	tokens := LexExp("(= (< 3 3) (>= 1 9))")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewEqExp(NewLtExp(NewIntLiteral(3), NewIntLiteral(3)), NewGteExp(NewIntLiteral(1), NewIntLiteral(9))))
	checkProgram(program, expectedProgram, t)
}

func TestStringLiteralExp(t *testing.T) {
	tokens := LexExp("(+ \"bi ba \" \"bo ba\")")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewAddExp(NewStringLiteral("bi ba "), NewStringLiteral("bo ba")))
	checkProgram(program, expectedProgram, t)
}

func TestBoolLiteralExp(t *testing.T) {
	tokens := LexExp("(= #t #f)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewEqExp(NewBoolLiteral(true), NewBoolLiteral(false)))
	checkProgram(program, expectedProgram, t)
}

func TestIfExp(t *testing.T) {
	tokens := LexExp("(if (> 6 5) \"true\" \"false\")")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewIfExp(NewGtExp(NewIntLiteral(6), NewIntLiteral(5)), NewStringLiteral("true"), NewStringLiteral("false")))
	checkProgram(program, expectedProgram, t)
}

func TestDefineExp(t *testing.T) {
	tokens := LexExp("(define x 5)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewDefExp("x", NewIntLiteral(5), Variable))
	checkProgram(program, expectedProgram, t)
}

func TestDefineExpWithNegativeNumber(t *testing.T) {
	tokens := LexExp("(define x -5)")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewDefExp("x", NewIntLiteral(-5), Variable))
	checkProgram(program, expectedProgram, t)
}

func TestLambdaExp(t *testing.T) {
	tokens := LexExp("(lambda (x y) (= x y))")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewLambdaExp([]string{"x", "y"}, []AstNode{NewEqExp(NewIdentExp("x"), NewIdentExp("y"))}))
	checkProgram(program, expectedProgram, t)
}

func TestDefLambdaExp(t *testing.T) {
	tokens := LexExp("(define (square x) (* x x))")
	program, _ := ParseTokens(tokens)
	expectedProgram := NewProgram(NewDefExp("square",
		NewLambdaExp([]string{"x"}, []AstNode{NewMulExp(NewIdentExp("x"), NewIdentExp("x"))}), Function))
	checkProgram(program, expectedProgram, t)
}

func TestDefLambdaLonghandExp(t *testing.T) {
	shorthandTokens := LexExp("(define (mul x y) (* x y))")
	shorthandProgram, _ := ParseTokens(shorthandTokens)
	longhandTokens := LexExp("(define mul (lambda (x y) (* x y)))")
	longhandProgram, _ := ParseTokens(longhandTokens)
	expectedProgram := NewProgram(NewDefExp("mul",
		NewLambdaExp([]string{"x", "y"}, []AstNode{NewMulExp(NewIdentExp("x"), NewIdentExp("y"))}), Function))
	checkProgram(shorthandProgram, expectedProgram, t)
	checkProgram(longhandProgram, expectedProgram, t)
}
