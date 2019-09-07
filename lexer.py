import re
from enum import Enum

from ply import lex

FILE_SQUARE = 'samples/square.ss'


class TokenType(Enum):
    ERROR = -1
    EOP = 0
    VAR = 1
    KEYWD = 2
    OP = 3
    NUM = 4
    IDENT = 5


KEYWORDS = [
    'define', 'if', 'cond', 'else', 'lambda', 'car', 'cdr'
]

OPERATORS = [
    '+', '-', '*', '>', '>=', '<', '<='
]


class Token:
    def __init__(self, tok_type: TokenType, val: str):
        self.type = tok_type
        self.value = val

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return f'({self.type._name_}: {self.value})'


class KeywordToken(Token):
    def __init__(self, val: str):
        super().__init__(TokenType.KEYWD, val)


class OperatorToken(Token):
    def __init__(self, val: str):
        super().__init__(TokenType.OP, val)


class VarToken(Token):
    def __init__(self, val: str):
        super().__init__(TokenType.VAR, val)


class IdentToken(Token):
    def __init__(self, val: str):
        super().__init__(TokenType.IDENT, val)





class Lexer:
    def __init__(self, program_lines: list):
        self.prog_header = None
        self.program = self._preprocess_program(program_lines)
        self.structure = self._eval_list_structure(self.program)

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
        if item in KEYWORDS:
            return KeywordToken(item)

        elif item in OPERATORS:
            return OperatorToken(item)

        elif item.isdigit():
            return VarToken(item)

        else:
            return IdentToken(item)

    def scan(self) -> list:
        return self._scan(self.structure)

    def _scan(self, program_items: list) -> list:
        tokens = []
        for item in program_items:
            if list == type(item):
                tokens.append(self._scan(item))
            else:
                tokens.append(self.determine_token(item))

        return tokens


tokens = (
    'LEFTPAR', 'RIGHTPAR', 'NUMBER', 'EOL', 'WS', 'IDENT'
)

t_LEFTPAR = r'\('
t_RIGHTPAR = r'\)'
t_EOL = r'\n'
t_WS = r'\s+'


def t_IDENT(t):
    r'[a-z0-9!$%&*\-./:<=>?@^_~]{1}[+\-a-z0-9!$%&*+\-./:<=>?@^_~]*'
    return t


def t_NUMBER(t):
    r'[-]?\d+'
    t.value = int(t.value)
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


def main():
    # content = None
    # with open(SAMPLE_SQUARE, 'r') as f:
    #     content = f.readlines()

    lexer = lex.lex()
    with open(FILE_SQUARE, 'r') as f:
        content = f.read()
        lexer.input(content)

    # Tokenize
    while True:
        tok = lexer.token()
        if not tok:
            break  # No more input
        print(tok)
    # lexer = Lexer(content)
    # tokens = lexer.scan()
    # print(tokens)



if __name__ == '__main__':
    main()
