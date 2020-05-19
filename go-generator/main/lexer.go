package main

import (
	"bytes"
	"encoding/binary"
	"math"
	"strconv"
	"strings"
	"unicode"
)

type TokenType int

const (
	TokenNone TokenType = iota
	TokenRParen
	TokenLParen
	TokenIdent
	TokenIntLiteral
	TokenFloatLiteral
	TokenStringLiteral
	TokenBoolLiteral
	TokenDot
	TokenOp
	TokenChar
)

type overrideType int

const (
	overrideNone overrideType = iota
	overrideIdent
	overrideString
)

type Token struct {
	Type     TokenType
	Value    bytes.Buffer
	ValueStr string
}

func NewTokenString(tokenType TokenType, tokenString string) *Token {
	tokenValue := bytes.NewBufferString(tokenString)
	token := &Token{
		tokenType,
		*tokenValue,
		tokenString,
	}
	return token
}

func NewTokenRaw(tokenType TokenType, tokenBuffer bytes.Buffer, repr string) *Token {
	tokenSlice := make([]byte, tokenBuffer.Len(), tokenBuffer.Len())
	copy(tokenSlice, tokenBuffer.Bytes())
	tokenValue := bytes.NewBuffer(tokenSlice)
	token := Token{tokenType, *tokenValue, repr}
	return &token
}

func bufferStringToNum(tokenType TokenType, inputBuffer bytes.Buffer) *bytes.Buffer {
	bufferString := inputBuffer.String()
	var byteBuffer [binary.MaxVarintLen64]byte
	if tokenType == TokenFloatLiteral {
		num, _ := strconv.ParseFloat(bufferString, 64)
		binary.LittleEndian.PutUint64(byteBuffer[:], math.Float64bits(num))
	} else {
		num, _ := strconv.ParseInt(bufferString, 10, 64)
		binary.PutVarint(byteBuffer[:], num)
	}
	returnBuffer := bytes.NewBuffer(byteBuffer[:])
	return returnBuffer
}

var identContainsNumber = false

func flush(accType *TokenType, byteBuffer *bytes.Buffer, tokBuffer *[]*Token) {
	if *accType == TokenFloatLiteral || *accType == TokenIntLiteral {
		convertedBuffer := bufferStringToNum(*accType, *byteBuffer)
		*tokBuffer = append(*tokBuffer, NewTokenRaw(*accType, *convertedBuffer, string(byteBuffer.Bytes())))
	} else {
		if byteBuffer.Bytes()[0] == 1 {
			*tokBuffer = append(*tokBuffer, NewTokenRaw(*accType, *byteBuffer, "#t"))
		} else if byteBuffer.Bytes()[0] == 0 {
			*tokBuffer = append(*tokBuffer, NewTokenRaw(*accType, *byteBuffer, "#f"))
		} else {
			*tokBuffer = append(*tokBuffer, NewTokenRaw(*accType, *byteBuffer, string(byteBuffer.Bytes())))
		}
	}
	byteBuffer.Reset()
	*accType = TokenNone
	identContainsNumber = false
}

func peek(input string, currentIndex int) rune {
	if len(input)-1 == currentIndex {
		return '\000'
	}
	return rune(input[currentIndex+1])
}

func LexExp(input string) []*Token {
	var tokens []*Token
	isAcc := false
	var accType TokenType
	var accBuffer bytes.Buffer

	specialInitials := "!$%&*/:<=>?^_~"
	override := overrideNone
	operatorChars := "+-/*<=>"

	for i, glyphRune := range input {
		glyph := string(glyphRune)

		if override == overrideIdent {
			accBuffer.WriteString(glyph)
			if glyph == "|" {
				flush(&accType, &accBuffer, &tokens)
				isAcc = false
				override = overrideNone
			}

		} else if override == overrideString {
			if glyph == "\"" {
				flush(&accType, &accBuffer, &tokens)
				isAcc = false
				override = overrideNone
			} else {
				accBuffer.WriteString(glyph)
			}

		} else if unicode.IsSpace(glyphRune) {
			if isAcc == true {
				flush(&accType, &accBuffer, &tokens)
				isAcc = false
			}

		} else if glyph == "\n" {
			flush(&accType, &accBuffer, &tokens)
			isAcc = false

		} else if glyph == "(" {
			if isAcc == true {
				flush(&accType, &accBuffer, &tokens)
				isAcc = false
			}
			tokens = append(tokens, NewTokenString(TokenLParen, glyph))

		} else if glyph == ")" {
			if isAcc == true {
				flush(&accType, &accBuffer, &tokens)
				isAcc = false
			}
			tokens = append(tokens, NewTokenString(TokenRParen, glyph))

		} else if glyph == "\"" {
			if isAcc == true {
				flush(&accType, &accBuffer, &tokens)
			}
			isAcc = true
			accType = TokenStringLiteral
			override = overrideString

		} else if strings.ContainsAny(glyph, operatorChars) &&
			(accType == TokenOp || accType == TokenNone) {
			if (glyph == ">" || glyph == "<") && (peek(input, i) == '=') {
				isAcc = true
				accType = TokenOp
				accBuffer.WriteString(glyph)

			} else {
				if isAcc == true && glyph == "=" {
					accBuffer.WriteString(glyph)
					flush(&accType, &accBuffer, &tokens)
					isAcc = false
				} else {
					tokens = append(tokens, NewTokenString(TokenOp, glyph))
				}
			}
		} else if glyph == "|" {
			if isAcc == true && accType != TokenIdent && accType != TokenStringLiteral {
				flush(&accType, &accBuffer, &tokens)
			} else if isAcc == false {
				override = overrideIdent
			}
			isAcc = true
			accBuffer.WriteString(glyph)
			accType = TokenIdent

		} else if glyph == "." {
			if isAcc == true && accType == TokenIdent {
				accBuffer.WriteString(glyph)

			} else if chr := peek(input, i); !unicode.IsSpace(chr) && unicode.IsNumber(chr) {
				isAcc = true
				accType = TokenFloatLiteral
				accBuffer.WriteString(glyph)
			} else {
				tokens = append(tokens, NewTokenString(TokenDot, glyph))
			}

		} else if glyph == "#" || (isAcc == true && accType == TokenBoolLiteral) {

			if chr := peek(input, i); chr == 't' || chr == 'f' {
				isAcc = true
				accType = TokenBoolLiteral

			} else if isAcc == true {
				// true as 1 and false as 0
				if glyph == "t" {
					accBuffer.WriteByte(1)
				} else {
					accBuffer.WriteByte(0)
				}
				flush(&accType, &accBuffer, &tokens)
				isAcc = false
			} else {
				tokens = append(tokens, NewTokenString(TokenChar, glyph))
			}

			// ident
		} else if unicode.IsLetter(glyphRune) {
			if isAcc == true && accType != TokenIdent {
				flush(&accType, &accBuffer, &tokens)
			}
			isAcc = true
			accType = TokenIdent
			accBuffer.WriteString(glyph)

		} else if strings.ContainsAny(glyph, specialInitials) {
			if isAcc == true && accType == TokenIdent {
				accBuffer.WriteString(glyph)
			} else {
				tokens = append(tokens, NewTokenString(TokenChar, glyph))
			}

		} else if unicode.IsNumber(glyphRune) {
			if isAcc == true && accType == TokenIdent {
				identContainsNumber = true
			}
			isAcc = true
			if accType != TokenFloatLiteral {
				if identContainsNumber {
					// do nothing. dont change accumulator type
				} else {
					accType = TokenIntLiteral
				}
			}
			accBuffer.WriteString(glyph)

		} else {
			tokens = append(tokens, NewTokenString(TokenChar, glyph))
		}
	}
	if isAcc == true {
		flush(&accType, &accBuffer, &tokens)
		isAcc = false
	}
	return tokens
}
