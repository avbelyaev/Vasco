package main

import (
	"encoding/binary"
	"errors"
	"math"
	. "wascho/go-generator/main/node"
)

var IDX int

// ParseTokens takes tokens and returns an AST (Abstract Syntax Tree) representation
func ParseTokens(tokens []*Token) *Program {
	IDX = 0
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
		intValue, _ := binary.Varint(tokens[IDX-1].Value.Bytes())
		return NewIntLiteral(intValue), nil

	} else if currentType == TokenFloatLiteral {
		IDX++
		bits := binary.LittleEndian.Uint64(tokens[IDX-1].Value.Bytes())
		floatValue := math.Float64frombits(bits)
		return NewFloatLiteral(floatValue), nil

	} else if currentType == TokenStringLiteral {
		IDX++
		literal := tokens[IDX-1]
		return NewStringLiteral(literal.Value.String()), nil

	} else if currentType == TokenBoolLiteral {
		IDX++
		literal := tokens[IDX-1]
		return NewBoolLiteral(literal.Value.Bytes()[0] == 1), nil

	} else if currentType == TokenIdent {
		IDX++
		identToken := tokens[IDX-1]
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
		opToken := tokens[IDX-1]
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
		identToken := tokens[IDX-1]
		identTokenValue := identToken.ValueStr
		switch identTokenValue {
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
				funcName := tokens[IDX-1].Value.String()
				funcArgs, _ := parseArgs(tokens)
				lambdaExp, _ := parseExpression(tokens)
				expError := closeExp(tokens)
				if nil != expError {
					return nil, expError
				}
				lambdaNode := NewLambdaExp(funcArgs, lambdaExp)
				defNode := NewDefExp(funcName, lambdaNode, Function)
				return defNode, nil
			} else {
				// defining something besides a function
				nameError := expect(tokens, TokenIdent)
				if nil != nameError {
					return nil, nameError
				}
				accept(tokens, TokenIdent)
				name := tokens[IDX-1]
				// this handles longhand lambda definitions too
				newExp, _ := parseExpression(tokens)
				expError := closeExp(tokens)
				if nil != expError {
					return nil, expError
				}
				defNode := NewDefExp(name.Value.String(), newExp, Variable)
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

		default:
			callArg, err := parseExpression(tokens)
			if nil != err {
				return nil, err
			}
			expError := closeExp(tokens)
			if nil != expError {
				return nil, expError
			}
			callNode := NewCallExpr(identTokenValue, callArg)
			return callNode, nil
		}
	}
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
			arg := tokens[IDX-1].Value.String()
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
