package main

import (
	"bufio"
	"errors"
	"fmt"
	"github.com/palantir/stacktrace"
	log "github.com/sirupsen/logrus"
	"io/ioutil"
	"os"
	"path"
	"strings"
	. "wascho/go-generator/main/node"
)

const outputDirName = "server"
const outputModuleName = "module.wat"

const outputModuleExportFunctionName = "dummy"

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
	if strings.Contains(name, "-") {
		replaceAll := -1
		fixed := strings.Replace(name, "-", "_", replaceAll)
		log.Infof("Fixed identifier: %s -> %s", name, fixed)

		identifierNames[name] = fixed
		return fixed
	}
	return name
}

func GenerateForEachRoot(program *Program, sourceCode string) (string, error) {
	log.Infof("Generating code")

	identifierNames = make(map[string]string, 10)
	LambdaCounter = 0
	NodeCounter = 0

	// generate comment block before program body
	var targetCode string
	scanner := bufio.NewScanner(strings.NewReader(sourceCode))
	for scanner.Scan() {
		targetCode += ";; " + scanner.Text() + "\n"
	}
	targetCode += ";;\n"

	targetCode += "(module\n"
	for _, root := range program.Children() {
		generatedRoot, err := GenerateCode(root)
		if nil != err {
			return "", stacktrace.Propagate(err,
				"error while generating code of root node of type %v: %s", root.Type(), root.String())
		}
		targetCode += generatedRoot + "\n"
		log.Infof("Generated code for root node of type %v: %s", root.Type(), root.String())
	}
	targetCode += ")"

	log.Infof("Generated successfully")
	return targetCode, nil
}

func GenerateCode(node AstNode) (string, error) {
	nodeType := node.Type()
	if IsArithmetic(&node) || IsComparison(&node) {
		lhsString, err := GenerateCode(node.Children()[0])
		if nil != err {
			return "", err
		}
		rhsString, err := GenerateCode(node.Children()[1])
		if nil != err {
			return "", err
		}
		operation := GetOperation(&node)
		c := lhsString
		c += rhsString
		c += operation + "\n"
		return c, nil

	} else if nodeType == IntNode {
		intNode := node.(*IntLiteral)
		c := fmt.Sprintf("i32.const %d\n", intNode.Value)
		return c, nil

	} else if nodeType == IdentNode {
		ident := node.(*IdentExp)
		// `get local` is a newer version of `local.get`
		c := fmt.Sprintf("get_local $%s\n", ident.Name)
		return c, nil

	} else if nodeType == LambdaNode {
		lambda := node.(*LambdaExp)
		argString := generateArgString(lambda.Args)
		lambdaName := generateLambdaName()

		c := fmt.Sprintf("%s = func(%s) int {\n", lambdaName, argString)
		body, err := GenerateCode(lambda.Children()[0])
		if nil != err {
			return "", err
		}
		c += body
		c += "}"
		return c, nil

	} else if nodeType == DefNode {
		defNode := node.(*DefExp)

		if defNode.DefType == Function {
			lambda := defNode.Children()[0].(*LambdaExp)
			funcName := GetIdentifier(defNode.Name)
			argString := generateArgString(lambda.Args)
			c := fmt.Sprintf("(func $%s %s (result i32)\n", funcName, argString)

			// extract body except last one expr
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
			c += lastExpr + "\n"
			c += ")\n"
			// TODO its probably better to generate all the exports at the very end of WAT,
			//   not immediate after function def
			c += fmt.Sprintf("(export \"%s\" (func $%s))\n", outputModuleExportFunctionName, funcName)
			return c, nil

		} else if defNode.DefType == Variable {
			varName := defNode.Name

			isTheOnlyChild := 1 == len(defNode.Children())
			if isTheOnlyChild {
				if defNode.Children()[0].Type() == VoidNode {
					// void node is a function declaration
					c := fmt.Sprintf("var %s func(int)int\n", varName)
					return c, nil

				} else {
					// any other node is a variable creations
					varValue, err := GenerateCode(defNode.Children()[0])
					if nil != err {
						return "", err
					}
					c := fmt.Sprintf("var %s = %s\n", varName, varValue)
					return c, nil
				}
			}
			// TODO can (define foo ...) have more than 1 children?
			return "", stacktrace.NewError(
				"More than one child for node with ID %s: %s", defNode.NodeID, defNode.String())
		}

	} else if nodeType == SetNode {
		setNode := node.(*SetExp)

		setCode, err := GenerateCode(setNode.Children()[0])
		if nil != err {
			return "", stacktrace.Propagate(err,
				"error while generating code for child[0] of nodeID %s: %s", setNode.NodeID, setNode.String())
		}
		return setCode, nil

	} else if nodeType == CallNode {
		call := node.(*CallExp)
		callName := GetIdentifier(call.WhatToCall)

		callArgs, err := GenerateCode(call.Children()[0])
		if nil != err {
			return "", err
		}
		c := callArgs + "\n"
		c += fmt.Sprintf("call $%s\n", callName)
		return c, nil

	} else if nodeType == IfNode {
		branch := node.(*IfExp)
		// condition
		condString, err := GenerateCode(branch.Cond)
		if nil != err {
			return "", err
		}
		c := condString
		// see https://blog.scottlogic.com/2018/04/26/webassembly-by-hand.html
		c += "(if (result i32)\n"

		// then
		if IsLeaf(branch.Then) {
			thenString, err := GenerateCode(branch.Then)
			if nil != err {
				return "", err
			}
			c += fmt.Sprintf("(then %s)\n", thenString)
		}

		// else
		if IsLeaf(branch.Else) {
			elseString, err := GenerateCode(branch.Else)
			if nil != err {
				return "", err
			}
			c += fmt.Sprintf("(else %s)\n", elseString)
		}
		c += ")\n"
		return c, err

	} else if nodeType == VoidNode {
		// void is only a declaration of function
		// TODO set up args, retValue properly
		c := "func(int)int"
		return c, nil
	}

	return "", errors.New(fmt.Sprintf("unexpected node %v", node))
}

func generateArgString(args []string) string {
	argString := ""
	for _, arg := range args {
		argString += fmt.Sprintf("(param $%s i32) ", arg)
	}
	return argString
}

// greetings roma, vlad :)
var sourceCode = `
(define (fact n)
	(if (= n 0)
 		1
 		(* n (fact (- n 1)))))
`

func main() {
	tokens := LexExp(sourceCode)
	astRoot, err := ParseTokens(tokens)
	Check(err)

	targetCode, err := GenerateForEachRoot(astRoot, sourceCode)
	if nil != err {
		print(err)
	}

	// TODO there aren't any formatters/syntax-highlighters for WAT and Idea
	emitCode(targetCode)
	log.Infof("Emitted targetCode:\n%s", targetCode)
}

func emitCode(code string) {
	if _, err := os.Stat(outputDirName); os.IsNotExist(err) {
		err = os.Mkdir(outputDirName, 0700)
		Check(err)
	}

	fileName := path.Join(outputDirName, outputModuleName)
	if _, err := os.Stat(fileName); os.IsExist(err) {
		err = os.Remove(fileName)
		Check(err)
	}

	err := ioutil.WriteFile(fileName, []byte(code), 0666)
	Check(err)
}
