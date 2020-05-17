package main

import (
	"errors"
	"fmt"
)

var prefixStub = `
package main

func main() {
`

var postfixStub = `
}
`

func GenerateTarget(node AstNode) (string, error) {

	toEmit := ""
	nodeType := node.GetType()

	if nodeType == AddNode {
		leftExprGenerated, err := GenerateTarget(node.GetSubNodes()[0])
		if nil != err {
			return "", err
		}
		rightExprGenerated, err := GenerateTarget(node.GetSubNodes()[1])
		if nil != err {
			return "", err
		}
		toEmit += leftExprGenerated + " + " + rightExprGenerated
		return toEmit, nil

	} else if nodeType == IntNode {
		toEmit := node.DebugString()
		return toEmit, nil

	} else if nodeType == DisplayNode {
		toEmit := "print("
		targetStr, err := GenerateTarget(node.GetSubNodes()[0])
		if nil != err {
			return "", err
		}
		toEmit += targetStr + ")"
		return toEmit, nil
	}
	return "", errors.New(fmt.Sprintf("unexpected node %s", node.DebugString()))
}

func main() {
	tokens := LexExp("(display 314)")
	program := ParseTokens(tokens)
	root := program.GetSubNodes()[0]
	s := prefixStub
	targetStr, err := GenerateTarget(root)
	if nil != err {
		print(err)
	}
	s += targetStr
	s += postfixStub
	print(s)
}
