package main

import (
	"fmt"
	"strconv"
	"strings"
)

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
	DisplayNode
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
	subNodes []AstNode
}

func (s *SExp) GetSubNodes() []AstNode {
	return s.subNodes
}
func (s *SExp) AddSubNode(node AstNode) {
	s.subNodes = append(s.subNodes, node)
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

type AddExp struct {
	SExp
}

func NewAddExp(lhs AstNode, rhs AstNode) *AddExp {
	node := new(AddExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (a AddExp) GetType() AstNodeType {
	return AddNode
}
func (a AddExp) String() string {
	return "AddExp(" + a.subNodes[0].String() + ", " + a.subNodes[1].String() + ")"
}

type SubExp struct {
	SExp
}

func NewSubExp(lhs AstNode, rhs AstNode) *SubExp {
	node := new(SubExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (s SubExp) GetType() AstNodeType {
	return SubNode
}
func (s SubExp) String() string {
	return "SubExp(" + s.subNodes[0].String() + ", " + s.subNodes[1].String() + ")"
}

type MulExp struct {
	SExp
}

func NewMulExp(lhs AstNode, rhs AstNode) *MulExp {
	node := new(MulExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (m MulExp) GetType() AstNodeType {
	return MulNode
}
func (m MulExp) String() string {
	return "MulExp(" + m.subNodes[0].String() + ", " + m.subNodes[1].String() + ")"
}

type DivExp struct {
	SExp
}

func NewDivExp(lhs AstNode, rhs AstNode) *DivExp {
	node := new(DivExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (d DivExp) GetType() AstNodeType {
	return DivNode
}
func (d DivExp) String() string {
	return "DivExp(" + d.subNodes[0].String() + ", " + d.subNodes[1].String() + ")"
}

type LtExp struct {
	SExp
}

func NewLtExp(lhs AstNode, rhs AstNode) *LtExp {
	node := new(LtExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (l LtExp) GetType() AstNodeType {
	return LtNode
}
func (l LtExp) String() string {
	return "LtExp(" + l.subNodes[0].String() + ", " + l.subNodes[1].String() + ")"
}

type LteExp struct {
	SExp
}

func NewLteExp(lhs AstNode, rhs AstNode) *LteExp {
	node := new(LteExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (l LteExp) GetType() AstNodeType {
	return LteNode
}
func (l LteExp) String() string {
	return "LteExp(" + l.subNodes[0].String() + ", " + l.subNodes[1].String() + ")"
}

type GtExp struct {
	SExp
}

func NewGtExp(lhs AstNode, rhs AstNode) *GtExp {
	node := new(GtExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (g GtExp) GetType() AstNodeType {
	return GtNode
}
func (g GtExp) String() string {
	return "GtExp(" + g.subNodes[0].String() + ", " + g.subNodes[1].String() + ")"
}

type GteExp struct {
	SExp
}

func NewGteExp(lhs AstNode, rhs AstNode) *GteExp {
	node := new(GteExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (g GteExp) GetType() AstNodeType {
	return LteNode
}
func (g GteExp) String() string {
	return "GteExp(" + g.subNodes[0].String() + ", " + g.subNodes[1].String() + ")"
}

type EqExp struct {
	SExp
}

func NewEqExp(lhs AstNode, rhs AstNode) *EqExp {
	node := new(EqExp)
	node.AddSubNode(lhs)
	node.AddSubNode(rhs)
	return node
}
func (e EqExp) GetType() AstNodeType {
	return EqNode
}
func (e EqExp) String() string {
	return "EqExp(" + e.subNodes[0].String() + ", " + e.subNodes[1].String() + ")"
}

type IfExp struct {
	SExp
}

func NewIfExp(cond AstNode, onTrue AstNode, onFalse AstNode) *IfExp {
	node := new(IfExp)
	node.AddSubNode(cond)
	node.AddSubNode(onTrue)
	node.AddSubNode(onFalse)
	return node
}
func (i IfExp) GetType() AstNodeType {
	return IfNode
}
func (i IfExp) String() string {
	return "IfExp(" + i.subNodes[0].String() + ", " + i.subNodes[1].String() + ", " + i.subNodes[2].String() + ")"
}

type DefExp struct {
	SExp
	Name string
}

func NewDefExp(name string, exp AstNode) *DefExp {
	node := new(DefExp)
	node.Name = name
	node.AddSubNode(exp)
	return node
}
func (d DefExp) GetType() AstNodeType {
	return DefNode
}
func (d DefExp) String() string {
	return "DefExp(" + d.Name + ", " + d.subNodes[0].String() + ")"
}

type DisplayExp struct {
	SExp
}

func NewDisplayExpr(whatToDisplay AstNode) *DisplayExp {
	node := new(DisplayExp)
	node.AddSubNode(whatToDisplay)
	return node
}

func (d DisplayExp) GetType() AstNodeType {
	return DisplayNode
}

func (d DisplayExp) String() string {
	return "Display"
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
	return "LambdaExp(" + strings.Trim(fmt.Sprintf("%v", l.Args), "[]") + ", " + l.subNodes[0].String() + ")"
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
