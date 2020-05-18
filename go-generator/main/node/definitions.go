package node

type DefineType string

const (
	Function DefineType = "func"
	Variable DefineType = "var"
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
	node.AddChildren(exp)
	return node
}
func (a DefExp) Type() AstNodeType {
	return DefNode
}

type LambdaExp struct {
	SExp
	Args []string
}

func NewLambdaExp(args []string, exp AstNode) *LambdaExp {
	node := new(LambdaExp)
	node.NodeID = NextNodeID()
	node.Args = append([]string(nil), args...)
	node.AddChildren(exp)
	return node
}
func (a LambdaExp) ID() string {
	return a.NodeID
}
func (a LambdaExp) Type() AstNodeType {
	return LambdaNode
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
func (a IdentExp) ID() string {
	return a.NodeID
}
func (a IdentExp) Type() AstNodeType {
	return IdentNode
}
