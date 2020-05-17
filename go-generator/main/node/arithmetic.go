package node

// ADD
type AddExp struct {
	SExp
}

func NewAddExp(lhs AstNode, rhs AstNode) *AddExp {
	node := new(AddExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (a AddExp) GetType() AstNodeType {
	return AddNode
}
func (a AddExp) String() string {
	return "AddExp(" + a.SubNodes[0].String() + ", " + a.SubNodes[1].String() + ")"
}

// SUB
type SubExp struct {
	SExp
}

func NewSubExp(lhs AstNode, rhs AstNode) *SubExp {
	node := new(SubExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (s SubExp) GetType() AstNodeType {
	return SubNode
}
func (s SubExp) String() string {
	return "SubExp(" + s.SubNodes[0].String() + ", " + s.SubNodes[1].String() + ")"
}

// DIV
type DivExp struct {
	SExp
}

func NewDivExp(lhs AstNode, rhs AstNode) *DivExp {
	node := new(DivExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (d DivExp) GetType() AstNodeType {
	return DivNode
}
func (d DivExp) String() string {
	return "DivExp(" + d.SubNodes[0].String() + ", " + d.SubNodes[1].String() + ")"
}

// MUL
type MulExp struct {
	SExp
}

func NewMulExp(lhs AstNode, rhs AstNode) *MulExp {
	node := new(MulExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (m MulExp) GetType() AstNodeType {
	return MulNode
}
func (m MulExp) String() string {
	return "MulExp(" + m.SubNodes[0].String() + ", " + m.SubNodes[1].String() + ")"
}
