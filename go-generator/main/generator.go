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
		define := node.(*DefExp)

		if define.DefType == Function {
			funcName := GetIdentifier(define.Name)
			// extract lambda
			lambda := define.Children()[0].(*LambdaExp)
			body, err := GenerateCode(lambda.Children()[0])
			if nil != err {
				return "", err
			}
			// extract lambda args
			argString := ""
			for _, arg := range lambda.Args {
				argString += fmt.Sprintf("%s int, ", arg)
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
		callName := GetIdentifier(call.WhatToCall)
		if callName == "display" {
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
		(define (myAbs x)
  			(if (< x 0) (display 111) (display 222)))

		(myAbs 1)
	`)
	root := ParseTokens(tokens)
	ProgramRoot = root
	code, err := GenerateForEachRoot(root)
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
