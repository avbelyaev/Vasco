import re
from enum import Enum

from ply import lex

FILE_SQUARE = 'samples/square.ss'

KEYWORDS = [
    'define', 'if', 'cond', 'else', 'lambda', 'car', 'cdr'
]


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
        program = str(program) \
            .replace("'", '"') \
            .replace('"""', '"quot"') \
            .strip()

        # remove outermost brackets from `str(list)`
        program = program[1: len(program) - 1]
        program = program \
            .replace('"("', '[') \
            .replace('")"', ']') \
            .replace('[,', '[') \
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


# Scheme grammar: https://www.scheme.com/tspl2d/grammar.html
tokens = (
    'LEFTPAR', 'RIGHTPAR', 'SPECIAL_IDENT', 'NUMBER', 'EOL', 'WS', 'IDENT',
)


def t_LEFTPAR(t):
    r'\('
    return t


def t_RIGHTPAR(t):
    r'\)'
    return t


def t_SPECIAL_IDENT(t):
    r'[+\-\.]{1}'
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_EOL(t):
    r'\n'
    return t


def t_WS(t):
    r'\s+'
    return t


# IDENTs denote {var, keyword, symbol} based on context
# They are formed from sequences of letters, digits, and special characters.
# With three exceptions, identifiers cannot begin with a character that can also begin a number,
# i.e., they cannot begin with ., +, -, or a digit. The three exceptions are the identifiers ..., +, and -.
def t_IDENT(t):
    r'[a-zA-Z!$%&*\-./:<=>?@^_~]{1}[+\-a-zA-Z0-9!$%&*+\-./:<=>?@^_~]*'
    # Case is insignificant in symbols
    t.value = t.value.lower()
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)



def main():
    lexer = lex.lex()
    with open(FILE_SQUARE, 'r') as f:
        content = f.read()
        lexer.input(content)

    # Tokenize
    tokenized = []
    while True:
        tok = lexer.token()
        if not tok:
            break  # No more input
        tokenized.append(tok)
        print(tok)

    print(tokenized)


if __name__ == '__main__':
    main()
