package node

type LtExp struct {
	SExp
}

func NewLtExp(lhs AstNode, rhs AstNode) *LtExp {
	node := new(LtExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	return node
}
func (l LtExp) Type() AstNodeType {
	return LtNode
}

type LteExp struct {
	SExp
}

func NewLteExp(lhs AstNode, rhs AstNode) *LteExp {
	node := new(LteExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	return node
}
func (a LteExp) Type() AstNodeType {
	return LteNode
}

type GtExp struct {
	SExp
}

func NewGtExp(lhs AstNode, rhs AstNode) *GtExp {
	node := new(GtExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	return node
}
func (a GtExp) Type() AstNodeType {
	return GtNode
}

type GteExp struct {
	SExp
}

func NewGteExp(lhs AstNode, rhs AstNode) *GteExp {
	node := new(GteExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	return node
}
func (a GteExp) Type() AstNodeType {
	return LteNode
}

type EqExp struct {
	SExp
}

func NewEqExp(lhs AstNode, rhs AstNode) *EqExp {
	node := new(EqExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	return node
}
func (a EqExp) Type() AstNodeType {
	return EqNode
}
