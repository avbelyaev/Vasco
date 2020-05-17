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
func (l LtExp) String() string {
	return "LtExp(" + l.SubNodes[0].String() + ", " + l.SubNodes[1].String() + ")"
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
func (l LteExp) String() string {
	return "LteExp(" + l.SubNodes[0].String() + ", " + l.SubNodes[1].String() + ")"
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
func (g GtExp) String() string {
	return "GtExp(" + g.SubNodes[0].String() + ", " + g.SubNodes[1].String() + ")"
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
func (g GteExp) String() string {
	return "GteExp(" + g.SubNodes[0].String() + ", " + g.SubNodes[1].String() + ")"
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
func (e EqExp) String() string {
	return "EqExp(" + e.SubNodes[0].String() + ", " + e.SubNodes[1].String() + ")"
}
