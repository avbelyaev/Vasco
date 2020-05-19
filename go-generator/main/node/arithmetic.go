package node

type AddExp struct {
	SExp
	Op string
}

func NewAddExp(lhs AstNode, rhs AstNode) *AddExp {
	node := new(AddExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	node.Op = "+"
	return node
}
func (a AddExp) Type() AstNodeType {
	return AddNode
}
func (a AddExp) String() string {
	return a.Op
}

type SubExp struct {
	SExp
	Op string
}

func NewSubExp(lhs AstNode, rhs AstNode) *SubExp {
	node := new(SubExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	node.Op = "-"
	return node
}
func (a SubExp) Type() AstNodeType {
	return SubNode
}
func (a SubExp) String() string {
	return a.Op
}

type MulExp struct {
	SExp
	Op string
}

func NewMulExp(lhs AstNode, rhs AstNode) *MulExp {
	node := new(MulExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	node.Op = "*"
	return node
}
func (a MulExp) Type() AstNodeType {
	return MulNode
}
func (a MulExp) String() string {
	return a.Op
}

type DivExp struct {
	SExp
	Op string
}

func NewDivExp(lhs AstNode, rhs AstNode) *DivExp {
	node := new(DivExp)
	node.SExp.NodeID = NextNodeID()
	node.AddChild(lhs)
	node.AddChild(rhs)
	node.Op = "/"
	return node
}
func (a DivExp) Type() AstNodeType {
	return DivNode
}
func (a DivExp) String() string {
	return a.Op
}
