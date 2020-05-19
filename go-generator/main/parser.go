package main

import (
	"encoding/binary"
	"github.com/palantir/stacktrace"
	log "github.com/sirupsen/logrus"
	"math"
	. "wascho/go-generator/main/node"
)

var IDX int

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
		return stacktrace.NewError("Unexpected EOF at index %d", IDX)

	} else if tokens[IDX].Type != expectedType {
		log.Errorf("Unexpected token '%s' near index %d", tokens[IDX].ValueStr, IDX)
		return stacktrace.NewError("Unexpected token at %d: %s", IDX, tokens[IDX].ValueStr)
	}
	return nil
}

func ParseTokens(tokens []*Token) (*Program, error) {
	log.Infof("Parsing %d tokens", len(tokens))

	program := NewProgram()
	IDX = 0
	for IDX < len(tokens) {
		node, err := parseExpression(tokens)
		if nil != err {
			return nil, stacktrace.Propagate(err, "error while parsing token. IDX: %d", IDX)
		}
		program.AddChild(node)
		log.Infof("IDX: %d/%d. Added node of type %v: %s", IDX, len(tokens), node.Type(), node.String())
	}

	log.Infof("Parse success")
	return program, nil
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
		return NewStringLiteral(literal.ValueStr), nil

	} else if currentType == TokenBoolLiteral {
		IDX++
		literal := tokens[IDX-1]
		return NewBoolLiteral(literal.Value.Bytes()[0] == 1), nil

	} else if currentType == TokenIdent {
		IDX++
		identToken := tokens[IDX-1]
		return NewIdentExp(identToken.ValueStr), nil
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
		identTokenValue := tokens[IDX-1].ValueStr
		switch identTokenValue {
		case "void":
			_ = closeExp(tokens)
			return NewVoidExp(), nil

		case "if":
			cond, _ := parseExpression(tokens)
			ifTrue, _ := parseExpression(tokens)
			ifFalse, _ := parseExpression(tokens)
			_ = closeExp(tokens)
			ifNode := NewIfExp(cond, ifTrue, ifFalse)
			return ifNode, nil

		case "define":
			// defining function
			if accept(tokens, TokenLParen) {
				nameError := expect(tokens, TokenIdent)
				if nil != nameError {
					return nil, nameError
				}
				accept(tokens, TokenIdent)
				funcName := tokens[IDX-1].ValueStr
				funcArgs, _ := parseArgs(tokens)
				// as there can be more than one expression, parse ALL (list)
				expressions, err := parseExpressions(tokens)
				if nil != err {
					return nil, err
				}
				_ = closeExp(tokens)
				// create node
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
				name := tokens[IDX-1].ValueStr
				// there can be only ONE expression as content
				newExp, _ := parseExpression(tokens)
				_ = closeExp(tokens)
				// (define foo 5)
				if IsLiteral(&newExp) {
					return NewDefExp(name, newExp, Variable), nil

				} else if newExp.Type() == LambdaNode {
					return NewDefExp(name, newExp, Function), nil

					// (define fact (void))
				} else if newExp.Type() == VoidNode {
					return NewDefExp(name, newExp, Variable), nil
				}
				return nil, stacktrace.NewError("Unexpected expression with name %s", name)
			}

		case "lambda":
			lparenError := expect(tokens, TokenLParen)
			if nil != lparenError {
				return nil, lparenError
			}
			IDX++
			lambdaArgs, _ := parseArgs(tokens)
			// as there can be more than one expression, parse ALL (list)
			expressions, err := parseExpressions(tokens)
			if nil != err {
				return nil, err
			}
			_ = closeExp(tokens)
			lambdaNode := NewLambdaExp(lambdaArgs, expressions)
			return lambdaNode, nil

		case "set!":
			// setting variable
			nameError := expect(tokens, TokenIdent)
			if nil != nameError {
				return nil, stacktrace.Propagate(nameError, "error in 'set!' at index %d", IDX)
			}
			accept(tokens, TokenIdent)
			setName := tokens[IDX-1].ValueStr
			// get contents of "set! fact (...)"
			// there can be only ONE expression as content
			expr, err := parseExpression(tokens)
			if nil != err {
				return nil, stacktrace.Propagate(err, "error in 'set!' at index %d: %s", IDX, tokens[IDX].ValueStr)
			}
			_ = closeExp(tokens)
			// create new node
			setNode := NewSetExp(setName, expr)
			return setNode, nil

		default:
			callArg, err := parseExpression(tokens)
			if nil != err {
				return nil, err
			}
			_ = closeExp(tokens)
			callNode := NewCallExpr(identTokenValue, callArg)
			return callNode, nil
		}
	}
	log.Errorf("Unexpected token at %d: %s", IDX, tokens[IDX].ValueStr)
	return nil, stacktrace.NewError("Unexpected token at %d: %s", IDX, tokens[IDX].ValueStr)
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
			arg := tokens[IDX-1].ValueStr
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

// parseExpressions parses for multiple expressions.
// Make sure to manually closeExp after parsing all expressions!!
func parseExpressions(tokens []*Token) ([]AstNode, error) {
	// lambda can have more than one expression inside body
	var expressions []AstNode
	for {
		expression, err := parseExpression(tokens)
		// roll the pointer back since he hopped over the symbol in `parseExpression`
		if nil != err {
			IDX--
			log.Warnf("Rolling back to index %d. There might be an error line above", IDX)
			break
		}
		_ = closeExp(tokens)
		expressions = append(expressions, expression)
	}
	return expressions, nil
}
