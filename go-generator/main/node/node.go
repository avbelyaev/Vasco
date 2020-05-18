package node

import "strconv"

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

var NodeCounter int

func NextNodeID() string {
	NodeCounter++
	return "node_" + strconv.Itoa(NodeCounter)
}

type AstNode interface {
	Children() []AstNode
	AddChild(AstNode)
	Type() AstNodeType
}

type SExp struct {
	NodeID       string
	ChildrenList []AstNode
}

func (s *SExp) ID() string {
	return s.NodeID
}
func (s *SExp) Children() []AstNode {
	return s.ChildrenList
}
func (s *SExp) AddChild(node AstNode) {
	s.ChildrenList = append(s.ChildrenList, node)
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
