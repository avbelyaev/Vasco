package node

import "strconv"

type IntLiteral struct {
	SExp
	Value int64
}

func NewIntLiteral(value int64) *IntLiteral {
	node := new(IntLiteral)
	node.Value = value
	return node
}
func (i IntLiteral) GetType() AstNodeType {
	return IntNode
}
func (i IntLiteral) String() string {
	return strconv.FormatInt(i.Value, 10)
}
func (i IntLiteral) GetValue() int64 {
	return i.Value
}

type FloatLiteral struct {
	SExp
	Value float64
}

func NewFloatLiteral(value float64) *FloatLiteral {
	node := new(FloatLiteral)
	node.Value = value
	return node
}
func (f FloatLiteral) GetType() AstNodeType {
	return FloatNode
}
func (f FloatLiteral) String() string {
	return strconv.FormatFloat(f.Value, 'g', -1, 64)
}

type StringLiteral struct {
	SExp
	Value string
}

func NewStringLiteral(value string) *StringLiteral {
	node := new(StringLiteral)
	node.Value = value
	return node
}
func (s StringLiteral) GetType() AstNodeType {
	return StringNode
}
func (s StringLiteral) String() string {
	return "\"" + s.Value + "\""
}

type BoolLiteral struct {
	SExp
	Value bool
}

func NewBoolLiteral(value bool) *BoolLiteral {
	node := new(BoolLiteral)
	node.Value = value
	return node
}
func (b BoolLiteral) GetType() AstNodeType {
	return BoolNode
}
func (b BoolLiteral) String() string {
	return strconv.FormatBool(b.Value)
}
