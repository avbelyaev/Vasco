package node

import (
	"fmt"
	"strings"
)

type DefineType int

const (
	Function DefineType = iota
	Variable
)

type DefExp struct {
	SExp
	Name    string
	DefType DefineType
}

func NewDefExp(name string, exp AstNode, defType DefineType) *DefExp {
	node := new(DefExp)
	node.Name = name
	node.DefType = defType
	node.AddSubNode(exp)
	return node
}
func (d DefExp) GetType() AstNodeType {
	return DefNode
}
func (d DefExp) String() string {
	return "DefExp(" + d.Name + ", " + d.SubNodes[0].String() + ")"
}

type LambdaExp struct {
	SExp
	Args []string
}

func NewLambdaExp(args []string, exp AstNode) *LambdaExp {
	node := new(LambdaExp)
	// copy to avoid the fact that the slice refers to data that could and will
	// get overwritten
	node.Args = append([]string(nil), args...)
	node.AddSubNode(exp)
	return node
}
func (l LambdaExp) GetType() AstNodeType {
	return LambdaNode
}

func (l LambdaExp) String() string {
	return "LambdaExp(" + strings.Trim(fmt.Sprintf("%v", l.Args), "[]") + ", " + l.SubNodes[0].String() + ")"
}

type IdentExp struct {
	SExp
	Name string
}

func NewIdentExp(name string) *IdentExp {
	node := new(IdentExp)
	node.Name = name
	return node
}
func (i IdentExp) GetType() AstNodeType {
	return IdentNode
}
func (i IdentExp) String() string {
	return i.Name
}
