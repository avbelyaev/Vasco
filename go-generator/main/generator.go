package main

import (
	"errors"
	"fmt"
	. "wascho/go-generator/main/node"
)

var prefixStub = `
package main

func main() {
`

var postfixStub = `
}
`

var LAMBDA_IDX = 0

func generateLambdaName() string {
	LAMBDA_IDX++
	return fmt.Sprintf("lambda%d", LAMBDA_IDX)
}

func GenerateForEachRoot(program *Program) (string, error) {
	c := prefixStub
	for _, root := range program.GetSubNodes() {
		generatedRoot, err := GenerateCode(root)
		if nil != err {
			return "", err
		}
		c += generatedRoot + "\n"
	}
	c += postfixStub
	return c, nil
}

func GenerateCode(node AstNode) (string, error) {
	nodeType := node.GetType()
	if nodeType == AddNode {
		leftExprGenerated, err := GenerateCode(node.GetSubNodes()[0])
		if nil != err {
			return "", err
		}
		rightExprGenerated, err := GenerateCode(node.GetSubNodes()[1])
		if nil != err {
			return "", err
		}
		c := leftExprGenerated + " + " + rightExprGenerated
		return c, nil

	} else if nodeType == IntNode {
		toEmit := node.String()
		return toEmit, nil

	} else if nodeType == LambdaNode {
		lambda := node.(*LambdaExp)
		args := ""
		for _, arg := range lambda.Args {
			args += ", " + arg
		}
		lambdaName := generateLambdaName()
		c := fmt.Sprintf("var %s func(interface{}) interface{}\n", lambdaName)
		c += fmt.Sprintf("%s = func(x interface{}) interface{} {\n", lambdaName)
		body, err := GenerateCode(lambda.GetSubNodes()[0])
		if nil != err {
			return "", err
		}
		c += body
		c += "}"

	} else if nodeType == DefNode {
		define := node.(*DefExp)
		if define.DefType == Function {
			// extract lambda
			lambda := define.GetSubNodes()[0].(*LambdaExp)
			body, err := GenerateCode(lambda.GetSubNodes()[0])
			if nil != err {
				return "", err
			}
			c := fmt.Sprintf("func %s (x interface{}) {\n", define.Name)
			c += body + "\n"
			c += "}\n"
			return c, nil

		} else if define.DefType == Variable {

		}

	} else if nodeType == CallNode {
		call := node.(*CallExp)
		callName := call.WhatToCall
		if callName == "display" {
			callName = "print"
		}
		callArgs, err := GenerateCode(call.GetSubNodes()[0])
		if nil != err {
			return "", err
		}
		c := fmt.Sprintf("%s(%s)", callName, callArgs)
		return c, nil
	}

	return "", errors.New(fmt.Sprintf("unexpected node %s", node.String()))
}

func main() {
	var lambda5 func(interface{}) interface{}
	lambda5 = func(x interface{}) interface{} {
		switch x.(type) {
		case string:
			return fmt.Sprintf("%v_str", x)
		case int:
			fmt.Printf("%d\n", x.(int))
			if x.(int) < 500 {
				return lambda5(x.(int) + 100).(int)
			} else {
				return 0
			}
		}
		return 0
	}
	lambda5(3)
	lambda5("asda")

	tokens := LexExp("(define (printer x) (display 3)) (printer 5)")
	program := ParseTokens(tokens)
	code, err := GenerateForEachRoot(program)
	if nil != err {
		print(err)
	}
	print(code)
}
