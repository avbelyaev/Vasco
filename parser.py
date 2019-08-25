from typing import List, Union

from lexer import SAMPLE_SQUARE, Lexer, Token


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


def parse(expr: list) -> Node:
    node = Node()
    variable_definition = _variable_definition(expr)
    node.add(variable_definition)
    return node


def _variable_definition(expr: list):
    node = Node()
    if 'define' == expr[0].value:
        if isinstance(expr[1], Token):
            v = _variable(expr[1])
            node.add(v)
            # e = self.

        elif isinstance(expr[1], list):
            var_nodes = Node()
            for item in expr[1]:
                v = _variable(item)
                var_nodes.add(v)
            node.add(var_nodes)

    else:
        raise LookupError(f'syntax error at {expr}')


def _variable(variable: Token) -> Node:
    return Node.single(variable)


def is_identifier(identifier: Token) -> bool:
    if '>' == identifier.value:
        return True
    return False



def _expression(expr: list) -> Node:
    node = Node()
    first = expr[0]
    if 'if' == first.value:
        if_cond = _expression(expr[1])
        node.add(if_cond)

        if_true = _expression(expr[2])
        node.add(if_true)

        if_flse = _expression(expr[3])
        node.add(if_flse)
        return node

    elif is_identifier(first):
        return # TODO still trying to parse `if (> x 0) () ()`





def main():
    content = None
    with open(SAMPLE_SQUARE, 'r') as f:
        content = f.readlines()

    lexer = Lexer(content)
    tokens = lexer.scan()

    ast = parse(tokens)


if __name__ == '__main__':
    main()
