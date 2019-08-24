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



class Lexer:
    def __init__(self, program_lines: list):
        self.header = None
        self.tokens = []
        self.program = program_lines
        self._preprocess_program()

    def _preprocess_program(self):
        lines = [s or not s.isspace() for s in self.program]
        if lines[0].startswith('#lang'):
            self.header = lines.pop(0).strip()

        program = ' '.join(lines)
        program = ' '.join(program.split())
        self.program = program

    # def scan(self) -> list:





def main():
    content = None
    with open(SAMPLE_SQUARE, 'r') as f:
        content = f.readlines()

    lexer = Lexer(content)
    # lexer
    print(lexer)



if __name__ == '__main__':
    main()
