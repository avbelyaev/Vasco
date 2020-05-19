package node

import "fmt"

type IntLiteral struct {
	SExp
	Value int64
}

func NewIntLiteral(value int64) *IntLiteral {
	node := new(IntLiteral)
	node.SExp.NodeID = NextNodeID()
	node.Value = value
	return node
}
func (a IntLiteral) Type() AstNodeType {
	return IntNode
}
func (a IntLiteral) String() string {
	return fmt.Sprintf("%d", a.Value)
}

type FloatLiteral struct {
	SExp
	Value float64
}

func NewFloatLiteral(value float64) *FloatLiteral {
	node := new(FloatLiteral)
	node.SExp.NodeID = NextNodeID()
	node.Value = value
	return node
}
func (a FloatLiteral) Type() AstNodeType {
	return FloatNode
}
func (a FloatLiteral) String() string {
	return fmt.Sprintf("%f", a.Value)
}

type StringLiteral struct {
	SExp
	Value string
}

func NewStringLiteral(value string) *StringLiteral {
	node := new(StringLiteral)
	node.SExp.NodeID = NextNodeID()
	node.Value = value
	return node
}
func (a StringLiteral) Type() AstNodeType {
	return StringNode
}
func (a StringLiteral) String() string {
	return a.Value
}

type BoolLiteral struct {
	SExp
	Value bool
}

func NewBoolLiteral(value bool) *BoolLiteral {
	node := new(BoolLiteral)
	node.SExp.NodeID = NextNodeID()
	node.Value = value
	return node
}
func (a BoolLiteral) Type() AstNodeType {
	return BoolNode
}
func (a BoolLiteral) String() string {
	if a.Value {
		return "#t"
	}
	return "#f"
}
