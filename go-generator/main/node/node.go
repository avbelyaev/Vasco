package node

import "strconv"

type AstNodeType int

const (
	ProgramNode AstNodeType = iota
	// arithmetic
	AddNode
	SubNode
	MulNode
	DivNode
	// comparison
	GtNode
	GteNode
	LtNode
	LteNode
	EqNode
	// literals
	IntNode
	FloatNode
	StringNode
	BoolNode
	// control flow
	IfNode
	CallNode
	// definitions
	DefNode
	LambdaNode
	VoidNode
	IdentNode
	SetNode
)

var NodeCounter int

func NextNodeID() string {
	NodeCounter++
	return "node_" + strconv.Itoa(NodeCounter)
}

type AstNode interface {
	Children() []AstNode
	AddChild(AstNode)
	Type() AstNodeType
	String() string
}

type SExp struct {
	NodeID       string
	childrenList []AstNode
}

func (s *SExp) ID() string {
	return s.NodeID
}
func (s *SExp) Children() []AstNode {
	return s.childrenList
}
func (s *SExp) AddChild(node AstNode) {
	s.childrenList = append(s.childrenList, node)
}

type Program struct {
	SExp
}

func NewProgram(nodes ...AstNode) *Program {
	program := new(Program)
	program.SExp.NodeID = NextNodeID()
	for _, node := range nodes {
		program.AddChild(node)
	}
	return program
}
func (p Program) Type() AstNodeType {
	return ProgramNode
}
func (p Program) String() string {
	return "Program"
}
