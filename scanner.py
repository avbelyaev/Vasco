import re
from enum import Enum

SAMPLE_SQUARE = 'samples/square.ss'


class TokenType(Enum):
    ERROR = -1
    EOP = 0
    KEYWORD = 1
    BRACKET = 2
    OPERATOR = 3
    NUMBER = 4
    WHITESPACE = 5


class Token:
    def __init__(self, tok_type: TokenType, val: str):
        self.type = tok_type
        self.value = val


class Scanner:
    def __init__(self, program_lines: list):
        self.prog_header = None
        self.program = self._preprocess_program(program_lines)
        self.ast = self._eval_list_structure(self.program)

    def _eval_list_structure(self, program: str) -> list:
        program = ' '.join(program.split())
        program = program \
            .replace('(', ' ( ') \
            .replace(')', ' ) ')

        program = re.sub(r'\s+', ' ', program) \
            .split(' ')
        program = list(filter(lambda s: '' != s, program))
        # swutch quotes to be able to print them without escaping
        program = str(program)\
            .replace("'", '"') \
            .replace('"""', '"quot"')\
            .strip()

        # remove outermost brackets from `str(list)`
        program = program[1: len(program) - 1]
        program = program\
            .replace('"("', '[')\
            .replace('")"', ']')\
            .replace('[,', '[')\
            .replace(', ]', ']')
        # add some magic and AST is ready
        evaluated = eval(program)
        return evaluated

    def _preprocess_program(self, program_lines: list) -> str:
        lines = [s or not s.isspace() for s in program_lines]
        # remove header
        if lines[0].startswith('#lang'):
            self.prog_header = lines.pop(0).strip()

        program = ' '.join(lines)
        return program

    def determine_token(self, item: str) -> Token:
        pass

    def scan(self, program_items: list) -> list:
        tokens = []
        for item in program_items:
            if list == type(item):
                tokens.append(self.scan(item))
            else:
                tokens.append(self.determine_token(item))

        return tokens


def main():
    content = None
    with open(SAMPLE_SQUARE, 'r') as f:
        content = f.readlines()

    scanner = Scanner(content)
    # scanner.scan()


if __name__ == '__main__':
    main()
