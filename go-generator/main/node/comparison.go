package node

type LtExp struct {
	SExp
}

func NewLtExp(lhs AstNode, rhs AstNode) *LtExp {
	node := new(LtExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (l LtExp) GetType() AstNodeType {
	return LtNode
}

type LteExp struct {
	SExp
}

func NewLteExp(lhs AstNode, rhs AstNode) *LteExp {
	node := new(LteExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (l LteExp) GetType() AstNodeType {
	return LteNode
}

type GtExp struct {
	SExp
}

func NewGtExp(lhs AstNode, rhs AstNode) *GtExp {
	node := new(GtExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (g GtExp) GetType() AstNodeType {
	return GtNode
}

type GteExp struct {
	SExp
}

func NewGteExp(lhs AstNode, rhs AstNode) *GteExp {
	node := new(GteExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (g GteExp) GetType() AstNodeType {
	return LteNode
}

type EqExp struct {
	SExp
}

func NewEqExp(lhs AstNode, rhs AstNode) *EqExp {
	node := new(EqExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (e EqExp) GetType() AstNodeType {
	return EqNode
}
