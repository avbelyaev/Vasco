package main

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"testing"
)

func NewTokenNum(tokenType TokenType, tokenString string) *Token {
	token := NewTokenString(tokenType, tokenString)
	token.Value = *bufferStringToNum(tokenType, token.Value)
	return token
}

func checkTokens(tokens []*Token, expectedTokens []*Token, t *testing.T) {
	for index, token := range tokens {
		expected := expectedTokens[index]
		assert.Equal(t, token.ValueStr, expected.ValueStr)
		assert.Equal(t, token.Type, expected.Type)
	}
}

func TestLexSingleExp(t *testing.T) {
	tokens := LexExp("(abc def ghi)")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "abc"),
		NewTokenString(TokenIdent, "def"),
		NewTokenString(TokenIdent, "ghi"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestLexIdentifierWithNumbers(t *testing.T) {
	tokens := LexExp("(define fuck123 4.56)")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "define"),
		NewTokenString(TokenIdent, "fuck123"),
		NewTokenString(TokenFloatLiteral, "4.56"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestLexCallIdentifierWithNumber(t *testing.T) {
	tokens := LexExp("(display fucker1488)")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "display"),
		NewTokenString(TokenIdent, "fucker1488"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestLexNestedExp(t *testing.T) {
	tokens := LexExp("(abc (def ghi (jkl)))")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "abc"),
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "def"),
		NewTokenString(TokenIdent, "ghi"),
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "jkl"),
		NewTokenString(TokenRParen, ")"),
		NewTokenString(TokenRParen, ")"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestIgnoreExtraWhitespace(t *testing.T) {
	tokens := LexExp("( ab   cd efg)")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "ab"),
		NewTokenString(TokenIdent, "cd"),
		NewTokenString(TokenIdent, "efg"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestLexIdentCorner(t *testing.T) {
	tokens := LexExp("(abc")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "abc")}
	checkTokens(tokens, expectedTokens, t)
}

func TestLexNumberLiterals(t *testing.T) {
	tokens := LexExp("(123 abc def 456.789 .012 345)")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenNum(TokenIntLiteral, "123"),
		NewTokenString(TokenIdent, "abc"),
		NewTokenString(TokenIdent, "def"),
		NewTokenNum(TokenFloatLiteral, "456.789"),
		NewTokenNum(TokenFloatLiteral, ".012"),
		NewTokenNum(TokenIntLiteral, "345"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestIdentSpecial(t *testing.T) {
	tokens := LexExp("ab.c . d|ef? |gh +i|")
	expectedTokens := []*Token{
		NewTokenString(TokenIdent, "ab.c"),
		NewTokenString(TokenDot, "."),
		NewTokenString(TokenIdent, "d|ef?"),
		NewTokenString(TokenIdent, "|gh +i|")}
	checkTokens(tokens, expectedTokens, t)
}

func TestOps(t *testing.T) {
	tokens := LexExp("(>= 150 (* (+ 10 3.2) 5))")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenOp, ">="),
		NewTokenNum(TokenIntLiteral, "150"),
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenOp, "*"),
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenOp, "+"),
		NewTokenNum(TokenIntLiteral, "10"),
		NewTokenNum(TokenFloatLiteral, "3.2"),
		NewTokenString(TokenRParen, ")"),
		NewTokenNum(TokenIntLiteral, "5"),
		NewTokenString(TokenRParen, ")"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestNewline(t *testing.T) {
	tokens := LexExp("(ab\ncd\nef)")
	expectedTokens := []*Token{
		NewTokenString(TokenLParen, "("),
		NewTokenString(TokenIdent, "ab"),
		NewTokenString(TokenIdent, "cd"),
		NewTokenString(TokenIdent, "ef"),
		NewTokenString(TokenRParen, ")")}
	checkTokens(tokens, expectedTokens, t)
}

func TestSingleFloat(t *testing.T) {
	tokens := LexExp("3.14")
	expectedTokens := []*Token{NewTokenNum(TokenFloatLiteral, "3.14")}
	checkTokens(tokens, expectedTokens, t)
}

func TestString(t *testing.T) {
	tokens := LexExp("\"la li lu le lo\"")
	expectedTokens := []*Token{NewTokenString(TokenStringLiteral, "la li lu le lo")}
	checkTokens(tokens, expectedTokens, t)
}

func TestBool(t *testing.T) {
	tokens := LexExp("#t #f bla")
	expectedTokens := []*Token{
		NewTokenRaw(TokenBoolLiteral, *bytes.NewBuffer([]byte{1}), "#t"),
		NewTokenRaw(TokenBoolLiteral, *bytes.NewBuffer([]byte{0}), "#f"),
		NewTokenString(TokenIdent, "bla")}
	checkTokens(tokens, expectedTokens, t)
}
