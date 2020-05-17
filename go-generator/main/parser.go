package main

import (
	"bytes"
	"encoding/binary"
	"errors"
	"math"
)

var IDX = 0

// ParseTokens takes tokens and returns an AST (Abstract Syntax Tree) representation
func ParseTokens(tokens []*Token) *Program {
	program := NewProgram()
	for len(tokens) > IDX {
		node, _ := parseExpression(tokens)
		program.AddSubNode(node)
	}
	return program
}

// accept checks to see if the current token matches a given token type, and advances if so
func accept(tokens []*Token, expectedType TokenType) bool {
	if tokens[IDX].Type == expectedType {
		IDX++
		return true
	}
	return false
}

// grabAccepted returns the token just before current, useful for grabbing the value of an accepted token
func grabAccepted(tokens []*Token) *Token {
	return tokens[IDX-1]
}

// expect returns an error if the current token doesn't match the given type
func expect(tokens []*Token, expectedType TokenType) error {
	if len(tokens)-1 < IDX {
		return errors.New("Unexpected EOF")
	} else if tokens[IDX].Type != expectedType {
		return errors.New("Unexpected token " + tokens[IDX].Value.String())
	}
	return nil
}

func parseExpression(tokens []*Token) (AstNode, error) {
	// try literals/idents first
	currentType := tokens[IDX].Type
	if currentType == TokenIntLiteral {
		IDX++
		literal := grabAccepted(tokens)
		return NewIntLiteral(bufferToInt(literal.Value)), nil

	} else if currentType == TokenFloatLiteral {
		IDX++
		literal := grabAccepted(tokens)
		return NewFloatLiteral(bufferToFloat(literal.Value)), nil

	} else if currentType == TokenStringLiteral {
		IDX++
		literal := grabAccepted(tokens)
		return NewStringLiteral(literal.Value.String()), nil

	} else if currentType == TokenBoolLiteral {
		IDX++
		literal := grabAccepted(tokens)
		return NewBoolLiteral(literal.Value.Bytes()[0] == 1), nil

	} else if currentType == TokenIdent {
		IDX++
		identToken := grabAccepted(tokens)
		return NewIdentExp(identToken.Value.String()), nil
	}
	// not a literal, attempt to parse an expression
	lparenError := expect(tokens, TokenLParen)
	if nil != lparenError {
		return nil, lparenError
	}
	// jump past the lparen
	IDX++
	if accept(tokens, TokenOp) {
		// grab the operator token so we can find out which one it is
		opToken := grabAccepted(tokens)
		// parse the left-hand and right hand sides recursively
		// this also takes care of handling nested expressions
		lhs, lhsError := parseExpression(tokens)
		if nil != lhsError {
			return nil, lhsError
		}
		rhs, rhsError := parseExpression(tokens)
		if nil != rhsError {
			return nil, rhsError
		}

		// what sort of operator node do we want to build?
		var expNode AstNode
		switch opToken.Value.String() {
		case "+":
			expNode = NewAddExp(lhs, rhs)
		case "-":
			expNode = NewSubExp(lhs, rhs)
		case "*":
			expNode = NewMulExp(lhs, rhs)
		case "/":
			expNode = NewDivExp(lhs, rhs)
		case "<":
			expNode = NewLtExp(lhs, rhs)
		case "<=":
			expNode = NewLteExp(lhs, rhs)
		case ">":
			expNode = NewGtExp(lhs, rhs)
		case ">=":
			expNode = NewGteExp(lhs, rhs)
		case "=":
			expNode = NewEqExp(lhs, rhs)
		}

		// make sure the expression has a closing rparen
		expError := closeExp(tokens)
		if nil != expError {
			return nil, expError
		}
		return expNode, nil
	}
	if accept(tokens, TokenIdent) {
		identToken := grabAccepted(tokens)
		switch identToken.Value.String() {
		case "if":
			// TODO: error-handling here (and throughout the parser!)
			cond, _ := parseExpression(tokens)
			ifTrue, _ := parseExpression(tokens)
			ifFalse, _ := parseExpression(tokens)
			ifNode := NewIfExp(cond, ifTrue, ifFalse)
			expError := closeExp(tokens)
			if nil != expError {
				return nil, expError
			}
			return ifNode, nil
		case "define":
			// are we attempting to define a function?
			if accept(tokens, TokenLParen) {
				nameError := expect(tokens, TokenIdent)
				if nil != nameError {
					return nil, nameError
				}
				accept(tokens, TokenIdent)
				funcName := grabAccepted(tokens).Value.String()
				funcArgs, _ := parseArgs(tokens)
				lambdaExp, _ := parseExpression(tokens)
				expError := closeExp(tokens)
				if nil != expError {
					return nil, expError
				}
				lambdaNode := NewLambdaExp(funcArgs, lambdaExp)
				defNode := NewDefExp(funcName, lambdaNode)
				return defNode, nil
			} else {
				// defining something besides a function
				nameError := expect(tokens, TokenIdent)
				if nil != nameError {
					return nil, nameError
				}
				accept(tokens, TokenIdent)
				name := grabAccepted(tokens)
				// this handles longhand lambda definitions too
				newExp, _ := parseExpression(tokens)
				expError := closeExp(tokens)
				if nil != expError {
					return nil, expError
				}
				defNode := NewDefExp(name.Value.String(), newExp)
				return defNode, nil
			}
		case "lambda":
			lparenError := expect(tokens, TokenLParen)
			if nil != lparenError {
				return nil, lparenError
			}
			IDX++
			lambdaArgs, _ := parseArgs(tokens)
			lambdaExp, _ := parseExpression(tokens)
			expError := closeExp(tokens)
			if nil != expError {
				return nil, expError
			}
			lambdaNode := NewLambdaExp(lambdaArgs, lambdaExp)
			return lambdaNode, nil

		case "display":
			exprToDisplay, err := parseExpression(tokens)
			if nil != err {
				return nil, err
			}
			expError := closeExp(tokens)
			if nil != expError {
				return nil, expError
			}
			dispNode := NewDisplayExpr(exprToDisplay)
			return dispNode, nil
		}
	}
	// no matches?
	return nil, errors.New("Unexpected token")
}

// convenience function to ensure an expression is properly closed
func closeExp(tokens []*Token) error {
	rparenError := expect(tokens, TokenRParen)
	if nil != rparenError {
		return rparenError
	}
	IDX += 1
	return nil
}

// convenience function to parse the argument list for a function/lambda
func parseArgs(tokens []*Token) ([]string, error) {
	funcArgs := make([]string, 0)
	for {
		if accept(tokens, TokenIdent) {
			arg := grabAccepted(tokens).Value.String()
			funcArgs = append(funcArgs, arg)
		} else {
			expError := closeExp(tokens)
			if nil != expError {
				return nil, expError
			}
			break
		}
	}
	return funcArgs, nil
}

func bufferToInt(buffer bytes.Buffer) int64 {
	num, _ := binary.Varint(buffer.Bytes())
	return num
}
func bufferToFloat(buffer bytes.Buffer) float64 {
	bits := binary.LittleEndian.Uint64(buffer.Bytes())
	return math.Float64frombits(bits)
}
