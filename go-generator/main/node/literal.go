package node

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
