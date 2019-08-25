from typing import List

from lexer import SAMPLE_SQUARE, Lexer, Token


class Ast:
    def __init__(self):
        self.ast = []

    def add_node(self, node):
        self.ast.append(node)


class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.curr_position = 0
        self.nodes = []

    def parse(self) -> Ast:
        self._parse_variable_definition()
        return None

    def _parse_variable_definition(self):
        self._parse_variable()

    def _parse_variable(self):
        pass


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
