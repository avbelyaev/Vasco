package node

type AstNodeType int

const (
	ProgramNode AstNodeType = iota
	AddNode
	SubNode
	MulNode
	DivNode
	GtNode
	LtNode
	GteNode
	LteNode
	EqNode
	IfNode
	DefNode
	CallNode
	LambdaNode
	IdentNode
	IntNode
	FloatNode
	StringNode
	BoolNode
)

// base interface for functions needing to accept any kind of AST node
type AstNode interface {
	GetSubNodes() []AstNode
	AddSubNode(AstNode)
	GetType() AstNodeType
	String() string
}

// base struct that all AST node implementations build off of
type SExp struct {
	SubNodes []AstNode
}

func (s *SExp) GetSubNodes() []AstNode {
	return s.SubNodes
}
func (s *SExp) AddSubNode(node AstNode) {
	s.SubNodes = append(s.SubNodes, node)
}

type Program struct {
	SExp
}

func NewProgram(nodes ...AstNode) *Program {
	program := new(Program)
	for _, node := range nodes {
		program.AddSubNode(node)
	}
	return program
}
func (p Program) GetType() AstNodeType {
	return ProgramNode
}
