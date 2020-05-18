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
	node.Name = name
	node.DefType = defType
	node.AddSubNode(exp)
	return node
}
func (d DefExp) GetType() AstNodeType {
	return DefNode
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
