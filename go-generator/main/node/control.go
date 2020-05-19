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
	node.AddChild(cond)
	node.AddChild(onTrue)
	node.AddChild(onFalse)
	node.Cond = cond
	node.Then = onTrue
	node.Else = onFalse
	return node
}
func (a IfExp) Type() AstNodeType {
	return IfNode
}
func (a IfExp) String() string {
	return "(if ... then ... else ... )"
}

type CallExp struct {
	SExp
	WhatToCall string
}

func NewCallExpr(whatToCall string, callArg AstNode) *CallExp {
	node := new(CallExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(callArg)
	node.WhatToCall = whatToCall
	return node
}
func (a CallExp) Type() AstNodeType {
	return CallNode
}
func (a CallExp) String() string {
	return "call " + a.WhatToCall
}
