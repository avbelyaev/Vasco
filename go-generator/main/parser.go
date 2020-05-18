package main

import (
	"encoding/binary"
	"errors"
	log "github.com/sirupsen/logrus"
	"math"
	. "wascho/go-generator/main/node"
)

var IDX int

func ParseTokens(tokens []*Token) *Program {
	IDX = 0
	program := NewProgram()
	for len(tokens) > IDX {
		node, err := parseExpression(tokens)
		Check(err)
		program.AddChild(node)
	}
	return program
}

func accept(tokens []*Token, expectedType TokenType) bool {
	if tokens[IDX].Type == expectedType {
		IDX++
		return true
	}
	return false
}

func expect(tokens []*Token, expectedType TokenType) error {
	if len(tokens)-1 < IDX {
		log.Errorf("Unexpected EOF at index %d", IDX)
		return errors.New("Unexpected EOF")

	} else if tokens[IDX].Type != expectedType {
		log.Errorf("Unexpected token '%s' near index %d", tokens[IDX].ValueStr, IDX)
		return errors.New("Unexpected token " + tokens[IDX].ValueStr)
	}
	return nil
}

func parseExpression(tokens []*Token) (AstNode, error) {
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

	IDX++
	if accept(tokens, TokenOp) {
		opToken := tokens[IDX-1]
		lhs, lhsError := parseExpression(tokens)
		if nil != lhsError {
			return nil, lhsError
		}
		rhs, rhsError := parseExpression(tokens)
		if nil != rhsError {
			return nil, rhsError
		}

		var expNode AstNode
		switch opToken.ValueStr {
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
			// defining function
			if accept(tokens, TokenLParen) {
				nameError := expect(tokens, TokenIdent)
				if nil != nameError {
					return nil, nameError
				}
				accept(tokens, TokenIdent)
				funcName := tokens[IDX-1].Value.String()
				funcArgs, _ := parseArgs(tokens)
				// lambda can have more than one expression inside body
				expressions, err := parseExpressions(tokens)
				if nil != err {
					return nil, err
				}
				lambdaNode := NewLambdaExp(funcArgs, expressions)
				defNode := NewDefExp(funcName, lambdaNode, Function)
				return defNode, nil

			} else {
				// defining variable
				nameError := expect(tokens, TokenIdent)
				if nil != nameError {
					return nil, nameError
				}
				accept(tokens, TokenIdent)
				name := tokens[IDX-1]
				// handle longhand lambda definitions
				newExp, _ := parseExpression(tokens)
				expError := closeExp(tokens)
				if nil != expError {
					return nil, expError
				}
				if newExp.Type() == LambdaNode {
					return NewDefExp(name.Value.String(), newExp, Function), nil
				} else {
					return NewDefExp(name.Value.String(), newExp, Variable), nil
				}
			}

		case "lambda":
			lparenError := expect(tokens, TokenLParen)
			if nil != lparenError {
				return nil, lparenError
			}
			IDX++
			lambdaArgs, _ := parseArgs(tokens)
			// lambda can have more than one expression inside body
			expressions, err := parseExpressions(tokens)
			if nil != err {
				return nil, err
			}
			lambdaNode := NewLambdaExp(lambdaArgs, expressions)
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
	log.Errorf("Unexpected token near index %d", IDX)
	return nil, errors.New("unexpected token")
}

func closeExp(tokens []*Token) error {
	rparenError := expect(tokens, TokenRParen)
	if nil != rparenError {
		return rparenError
	}
	IDX += 1
	return nil
}

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

func parseExpressions(tokens []*Token) ([]AstNode, error) {
	// lambda can have more than one expression inside body
	var expressions []AstNode
	for {
		expression, err := parseExpression(tokens)
		if nil != err {
			break
		}
		expressions = append(expressions, expression)
	}
	expError := closeExp(tokens)
	if nil != expError {
		return nil, expError
	}
	return expressions, nil
}
