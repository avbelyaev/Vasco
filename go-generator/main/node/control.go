package node

type IfExp struct {
	SExp
	Cond AstNode
	Then AstNode
	Else AstNode
}

func NewIfExp(cond AstNode, onTrue AstNode, onFalse AstNode) *IfExp {
	node := new(IfExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChildren(cond)
	node.AddChildren(onTrue)
	node.AddChildren(onFalse)
	node.Cond = cond
	node.Then = onTrue
	node.Else = onFalse
	return node
}
func (a IfExp) Type() AstNodeType {
	return IfNode
}

type CallExp struct {
	SExp
	WhatToCall string
}

func NewCallExpr(whatToCall string, callArg AstNode) *CallExp {
	node := new(CallExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChildren(callArg)
	node.WhatToCall = whatToCall
	return node
}
func (a CallExp) Type() AstNodeType {
	return CallNode
}
