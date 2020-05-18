package node

type AddExp struct {
	SExp
	Op string
}

func NewAddExp(lhs AstNode, rhs AstNode) *AddExp {
	node := new(AddExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	node.Op = "+"
	return node
}
func (a AddExp) GetType() AstNodeType {
	return AddNode
}

type SubExp struct {
	SExp
	Op string
}

func NewSubExp(lhs AstNode, rhs AstNode) *SubExp {
	node := new(SubExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	node.Op = "-"
	return node
}
func (s SubExp) GetType() AstNodeType {
	return SubNode
}

type MulExp struct {
	SExp
	Op string
}

func NewMulExp(lhs AstNode, rhs AstNode) *MulExp {
	node := new(MulExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	node.Op = "*"
	return node
}
func (m MulExp) GetType() AstNodeType {
	return MulNode
}

type DivExp struct {
	SExp
	Op string
}

func NewDivExp(lhs AstNode, rhs AstNode) *DivExp {
	node := new(DivExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	node.Op = "/"
	return node
}

func (d DivExp) GetType() AstNodeType {
	return DivNode
}
