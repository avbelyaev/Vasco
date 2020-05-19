package node

import "strings"

type DefineType string

const (
	Function DefineType = "function-type"
	Variable DefineType = "variable-type"
)

type DefExp struct {
	SExp
	Name    string
	DefType DefineType
}

func NewDefExp(name string, exp AstNode, defType DefineType) *DefExp {
	node := new(DefExp)
	node.NodeID = NextNodeID()
	node.Name = name
	node.DefType = defType
	node.AddChild(exp)
	return node
}
func (a DefExp) Type() AstNodeType {
	return DefNode
}
func (a DefExp) String() string {
	return "(define " + a.Name + " ... )"
}

type LambdaExp struct {
	SExp
	Args []string
}

func NewLambdaExp(args []string, expressions []AstNode) *LambdaExp {
	node := new(LambdaExp)
	node.NodeID = NextNodeID()
	node.Args = append([]string(nil), args...)
	for _, expr := range expressions {
		node.AddChild(expr)
	}
	return node
}
func (a LambdaExp) Type() AstNodeType {
	return LambdaNode
}
func (a LambdaExp) String() string {
	return "(lambda (" + strings.Join(a.Args, " ") + " ... )"
}

type IdentExp struct {
	SExp
	Name string
}

func NewIdentExp(name string) *IdentExp {
	node := new(IdentExp)
	node.NodeID = NextNodeID()
	node.Name = name
	return node
}
func (a IdentExp) Type() AstNodeType {
	return IdentNode
}
func (a IdentExp) String() string {
	return a.Name
}

type VoidExp struct {
	SExp
}

func NewVoidExp() *VoidExp {
	node := new(VoidExp)
	node.NodeID = NextNodeID()
	return node
}
func (a VoidExp) Type() AstNodeType {
	return VoidNode
}
func (a VoidExp) String() string {
	return "(void)"
}

type SetExp struct {
	SExp
	Variable string
}

func NewSetExp(variableToSet string, expr AstNode) *SetExp {
	node := new(SetExp)
	node.NodeID = NextNodeID()
	node.Variable = variableToSet
	node.AddChild(expr)
	return node
}
func (a SetExp) Type() AstNodeType {
	return SetNode
}
func (a SetExp) String() string {
	return "(set! " + a.Variable + " ... )"
}
