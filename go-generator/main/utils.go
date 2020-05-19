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

func GetOperator(nNode *node.AstNode) string {
	nodeType := (*nNode).Type()
	switch nodeType {
	case node.LtNode:
		return "<"
	case node.LteNode:
		return "<="
	case node.GtNode:
		return ">"
	case node.GteNode:
		return ">="
	case node.EqNode:
		return "=="

	case node.AddNode:
		return "+"
	case node.SubNode:
		return "-"
	case node.MulNode:
		return "*"
	case node.DivNode:
		return "/"
	}
	panic("unexpected type")
}
