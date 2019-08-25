from typing import List, Union

from lexer import SAMPLE_SQUARE, Lexer, Token


class Ast:
    def __init__(self):
        self.ast = []

    def add_node(self, node):
        self.ast.append(node)


# NODE IS A LIST !!!!!
class Node:
    def __init__(self):
        self.items = []

    def add(self, item: Union[Token, 'Node']):
        self.items.append(item)

    @staticmethod
    def single(item: Token):
        node = Node()
        node.add(item)
        return node


class Parser:
    def __init__(self, tokens: 'List[Token]'):
        self.tokens = tokens
        self.curr_position = 0
        self.ast = Ast()

    def parse(self) -> Ast:
        self._parse_variable_definition(self.tokens)
        return self.ast

    def _parse_variable_definition(self, expr: list):
        nodes = []
        if 'define' == expr[0].value:
            if isinstance(expr[1], Token):
                v = self._parse_variable(expr[1])
                nodes.append(v)
                # e = self.

            elif isinstance(expr[1], list):
                var_nodes = []
                for item in expr[1]:
                    v = self._parse_variable(item)
                    var_nodes.append(v)
                nodes.append(var_nodes)

        else:
            raise LookupError(f'syntax error at {expr}')


    def _parse_variable(self, variable: Token) -> Node:
        return Node.single(variable)

    def _parse_identifier(self, identifier: Token) -> Node:
        pass

    def _parse_initial(self):
        pass

    def _parse_expression(self, expr: list) -> Node:
        node = Node()
        if 'if' == expr[0].value:
            if_cond = self._parse_expression(expr[1])
            node.add(if_cond)

            if_true = self._parse_expression(expr[2])
            node.add(if_true)

            if_flse = self._parse_expression(expr[3])
            node.add(if_flse)
            return node








def main():
    content = None
    with open(SAMPLE_SQUARE, 'r') as f:
        content = f.readlines()

    lexer = Lexer(content)
    tokens = lexer.scan()

    parser = Parser(tokens)
    ast = parser.parse()


if __name__ == '__main__':
    main()
