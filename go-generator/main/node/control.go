package node

type IfExp struct {
	SExp
}

func NewIfExp(cond AstNode, onTrue AstNode, onFalse AstNode) *IfExp {
	node := new(IfExp)
	node.AddSubNode(cond)
	node.AddSubNode(onTrue)
	node.AddSubNode(onFalse)
	return node
}
func (i IfExp) GetType() AstNodeType {
	return IfNode
}
func (i IfExp) String() string {
	return "IfExp(" + i.SubNodes[0].String() + ", " + i.SubNodes[1].String() + ", " + i.SubNodes[2].String() + ")"
}

type CallExp struct {
	SExp
	WhatToCall string
}

func NewCallExpr(whatToCall string, callArg AstNode) *CallExp {
	node := new(CallExp)
	node.AddSubNode(callArg)
	node.WhatToCall = whatToCall
	return node
}

func (a CallExp) GetType() AstNodeType {
	return CallNode
}

func (a CallExp) String() string {
	return "Call"
}
