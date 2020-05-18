package main

import (
	"errors"
	"fmt"
	log "github.com/sirupsen/logrus"
	"io/ioutil"
	"os"
	"path"
	"strings"
	. "wascho/go-generator/main/node"
)

var programHeader = `
package main

import "fmt"

func main() {
`

var programFooter = `
// avoiding unused import of "fmt"
fmt.Println()
}
`

var ProgramRoot *Program
var LambdaCounter int
var identifierNames map[string]string

func generateLambdaName() string {
	LambdaCounter++
	return fmt.Sprintf("lambda%d", LambdaCounter)
}

func GetIdentifier(name string) string {
	if fixedName, exists := identifierNames[name]; exists {
		return fixedName
	}
	if strings.Contains(name, "O") {
		noReplacementLimit := -1
		fixed := strings.Replace(name, "O", "_", noReplacementLimit)
		log.Infof("Fixed identifier: %s -> %s", name, fixed)

		identifierNames[name] = fixed
		return fixed
	}
	return name
}

func GenerateForEachRoot(program *Program) (string, error) {
	identifierNames = make(map[string]string, 10)
	LambdaCounter = 0
	NodeCounter = 0
	c := programHeader
	for _, root := range program.Children() {
		generatedRoot, err := GenerateCode(root)
		if nil != err {
			return "", err
		}
		c += generatedRoot + "\n"
	}
	c += programFooter
	return c, nil
}

func GenerateCode(node AstNode) (string, error) {
	nodeType := node.Type()
	if IsArithmetic(&node) {
		lhsString, err := GenerateCode(node.Children()[0])
		if nil != err {
			return "", err
		}
		rhsString, err := GenerateCode(node.Children()[1])
		if nil != err {
			return "", err
		}
		opeartor := GetOperator(&node)
		c := fmt.Sprintf("%s %s %s", lhsString, opeartor, rhsString)
		return c, nil

		// TODO maybe merge arithmetic & comparison
	} else if IsComparison(&node) {
		lhsString, err := GenerateCode(node.Children()[0])
		if nil != err {
			return "", err
		}
		rhsString, err := GenerateCode(node.Children()[1])
		if nil != err {
			return "", err
		}
		opeartor := GetOperator(&node)
		c := fmt.Sprintf("%s %s %s", lhsString, opeartor, rhsString)
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
		defNode := node.(*DefExp)

		if defNode.DefType == Function {
			lambda := defNode.Children()[0].(*LambdaExp)
			funcName := GetIdentifier(defNode.Name)

			// extract args
			argString := ""
			for _, arg := range lambda.Args {
				argString += fmt.Sprintf("%s int, ", arg)
			}
			argString = argString[:len(argString)-2]
			c := fmt.Sprintf("var %s = func(%s) int {\n", funcName, argString)

			// extract body expect last one expr
			var i = 0
			for i = 0; i < len(lambda.Children())-1; i++ {
				expr := lambda.Children()[i]
				exprString, err := GenerateCode(expr)
				if nil != err {
					return "", err
				}
				c += exprString + "\n"
			}
			// manually extract last expr
			lastExpr, err := GenerateCode(lambda.Children()[i])
			if nil != err {
				return "", err
			}
			c += fmt.Sprintf("var last = %s\n", lastExpr)
			c += fmt.Sprint("return last\n}\n")
			return c, nil

		} else if defNode.DefType == Variable {
			varName := defNode.Name
			varValue, err := GenerateCode(defNode.Children()[0])
			if nil != err {
				return "", err
			}
			c := fmt.Sprintf("var %s = %s\n", varName, varValue)
			return c, nil
		}

	} else if nodeType == CallNode {
		call := node.(*CallExp)
		callName := GetIdentifier(call.WhatToCall)
		if callName == "display" {
			callName = "fmt.Print"
		} else if callName == "displayln" {
			callName = "fmt.Println"
		}
		callArgs, err := GenerateCode(call.Children()[0])
		if nil != err {
			return "", err
		}
		c := fmt.Sprintf("%s(%s)", callName, callArgs)
		return c, nil

	} else if nodeType == IfNode {
		branch := node.(*IfExp)
		condString, err := GenerateCode(branch.Cond)
		if nil != err {
			return "", err
		}
		thenString, err := GenerateCode(branch.Then)
		if nil != err {
			return "", err
		}
		elseString, err := GenerateCode(branch.Else)
		if nil != err {
			return "", err
		}
		c := fmt.Sprintf("if %s {\n", condString)
		c += fmt.Sprintf("\t%s\n", thenString)
		c += fmt.Sprintf("} else {\n")
		c += fmt.Sprintf("\t%s\n", elseString)
		c += fmt.Sprintf("}")
		return c, err
	}

	return "", errors.New(fmt.Sprintf("unexpected node %v", node))
}

type Value struct {
	value interface{}
}

func NewValue(v interface{}) *Value {
	value := &Value{
		value: v,
	}
	return value
}

func main() {
	var identity = func(x int) int {
		var last = x
		return last
	}
	_ = identity(5)
	tokens := LexExp(`
		(define identity
			(lambda (foo)
				foo))

		(display (identity 5))
	`)
	root := ParseTokens(tokens)
	ProgramRoot = root
	code, err := GenerateForEachRoot(root)
	if nil != err {
		print(err)
	}

	emitCode(code)
	log.Infof("Emitted code:\n%s", code)
}

func emitCode(code string) {
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

	err := ioutil.WriteFile(fileName, []byte(code), 0666)
	Check(err)
}
