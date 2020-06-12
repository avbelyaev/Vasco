package main

import "wascho/go-generator/main/node"

func Check(e error) {
	if e != nil {
		panic(e)
	}
}

func IsLeaf(nNode node.AstNode) bool {
	return true
}

func IsComparison(nNode *node.AstNode) bool {
	nodeType := (*nNode).Type()
	switch nodeType {
	case
		node.LtNode,
		node.LteNode,
		node.GtNode,
		node.GteNode,
		node.EqNode:
		return true
	}
	return false
}

func IsArithmetic(nNode *node.AstNode) bool {
	nodeType := (*nNode).Type()
	switch nodeType {
	case
		node.AddNode,
		node.SubNode,
		node.MulNode,
		node.DivNode:
		return true
	}
	return false
}

func IsLiteral(nNode *node.AstNode) bool {
	nodeType := (*nNode).Type()
	switch nodeType {
	case
		node.IntNode,
		node.FloatNode,
		node.BoolNode,
		node.StringNode:
		return true
	}
	return false
}

// see https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-instr-numeric
func GetOperation(nNode *node.AstNode) string {
	nodeType := (*nNode).Type()
	switch nodeType {
	case node.LtNode:
		return "i32.lt_s"
	case node.LteNode:
		return "i32.le_s"
	case node.GtNode:
		return "i32.gt_s"
	case node.GteNode:
		return "i32.ge_s"
	case node.EqNode:
		return "i32.eq"

	case node.AddNode:
		return "i32.add"
	case node.SubNode:
		return "i32.sub"
	case node.MulNode:
		return "i32.mul"
	case node.DivNode:
		return "i32.div"
	}
	panic("unexpected type")
}
