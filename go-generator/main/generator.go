package main

var prefixStub = `
package main

func main() {
`

var postfixStub = `
}
`

func GenerateTarget(node AstNode) string {

	toEmit := ""
	nodeType := node.GetType()
	switch nodeType {
	case AddNode:
		leftExprGenerated := GenerateTarget(node.GetSubNodes()[0])
		rightExprGenerated := GenerateTarget(node.GetSubNodes()[1])
		toEmit += leftExprGenerated + " + " + rightExprGenerated
		return toEmit

	case IntNode:
		toEmit := node.DebugString()
		return toEmit
	case DisplayNode:
		toEmit := "print("
		toEmit += GenerateTarget(node)
	}
	return toEmit
}

func main() {
	tokens := LexExp("(display 314)")
	program := ParseTokens(tokens)
	root := program.GetSubNodes()[0]
	s := prefixStub
	s += GenerateTarget(root)
	s += postfixStub
	print(s)
}
