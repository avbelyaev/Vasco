package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	. "wascho/go-generator/main/node"
)

var prefixStub = `
package main

import "fmt"

func main() {
`

var postfixStub = `
}
`

var LambdaCounter int
var ProgramRoot *Program

func generateLambdaName() string {
	LambdaCounter++
	return fmt.Sprintf("lambda%d", LambdaCounter)
}

func GenerateForEachRoot(program *Program) (string, error) {
	LambdaCounter = 0
	NodeCounter = 0
	c := prefixStub
	for _, root := range program.Children() {
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
	nodeType := node.Type()
	if nodeType == AddNode {
		leftExprGenerated, err := GenerateCode(node.Children()[0])
		if nil != err {
			return "", err
		}
		rightExprGenerated, err := GenerateCode(node.Children()[1])
		if nil != err {
			return "", err
		}
		c := leftExprGenerated + " + " + rightExprGenerated
		return c, nil

	} else if nodeType == IntNode {
		intNode := node.(*IntLiteral)
		c := fmt.Sprintf("%d", intNode.Value)
		return c, nil

	} else if nodeType == IdentNode {
		ident := node.(*IdentExp)
		return ident.Name, nil

	} else if nodeType == LambdaNode {
		lambda := node.(*LambdaExp)
		args := ""
		for _, arg := range lambda.Args {
			args += ", " + arg
		}
		lambdaName := generateLambdaName()
		c := fmt.Sprintf("var %s func(interface{}) interface{}\n", lambdaName)
		c += fmt.Sprintf("%s = func(x interface{}) interface{} {\n", lambdaName)
		body, err := GenerateCode(lambda.Children()[0])
		if nil != err {
			return "", err
		}
		c += body
		c += "}"

	} else if nodeType == DefNode {
		define := node.(*DefExp)

		if define.DefType == Function {
			funcName := define.Name
			// extract lambda
			lambda := define.Children()[0].(*LambdaExp)
			body, err := GenerateCode(lambda.Children()[0])
			if nil != err {
				return "", err
			}
			// extract lambda args
			argString := ""
			for _, arg := range lambda.Args {
				argString += fmt.Sprintf("%s interface{}, ", arg)
			}
			argString = argString[:len(argString)-2]
			c := fmt.Sprintf("var %s = func(%s) {\n", funcName, argString)
			c += body + "\n"
			c += "}\n"
			return c, nil

		} else if define.DefType == Variable {
			varName := define.Name
			varValue, err := GenerateCode(define.Children()[0])
			if nil != err {
				return "", err
			}
			c := fmt.Sprintf("var %s = %s\n", varName, varValue)
			return c, nil
		}

	} else if nodeType == CallNode {
		call := node.(*CallExp)
		callName := call.WhatToCall
		if callName == "display" {
			callName = "fmt.Println"
		}
		callArgs, err := GenerateCode(call.Children()[0])
		if nil != err {
			return "", err
		}
		c := fmt.Sprintf("%s(%s)", callName, callArgs)
		return c, nil
	}

	return "", errors.New(fmt.Sprintf("unexpected node %v", node))
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
	//lambda5(3)
	//lambda5("asda")

	tokens := LexExp(`
		(define (printer foo) 
			(display (+ foo 1)))

		(define pi 315) 

		(printer pi)
	`)
	ProgramRoot = ParseTokens(tokens)
	code, err := GenerateForEachRoot(ProgramRoot)
	if nil != err {
		print(err)
	}

	outDir := "server/static"
	if _, err := os.Stat(outDir); os.IsNotExist(err) {
		err = os.Mkdir(outDir, 0700)
		Check(err)
	}

	fileName := path.Join(outDir, "out.go")
	if _, err := os.Stat(fileName); os.IsExist(err) {
		err = os.Remove(fileName)
		Check(err)
	}

	err = ioutil.WriteFile(fileName, []byte(code), 0666)
	Check(err)

	fmt.Println("Emitted code:", code)
}
