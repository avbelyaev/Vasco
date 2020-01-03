import re

from ply import lex

FILE_SQUARE = 'samples/square.rkt'

KEYWORDS = [
    'define', 'if', 'cond', 'else', 'lambda', 'car', 'cdr'
]


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



# Scheme grammar: https://www.scheme.com/tspl2d/grammar.html
tokens = (
    'LEFTPAR', 'RIGHTPAR', 'SPECIAL_IDENT', 'NUMBER', 'EOL', 'WS', 'IDENT',
)


# разбор идет сверху вниз по t_* функциям. при этом токены
# отсортированы в порядке уменьшения длины рег-ки
def t_LEFTPAR(t):
    r'\('
    return t


def t_RIGHTPAR(t):
    r'\)'
    return t


def t_NUMBER(t):
    r'[-]?\d+'
    t.value = int(t.value)
    return t


def t_SPECIAL_IDENT(t):
    r'[+\-\.]{1}'
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
    print(f'Illegal character {t.value[0]}')
    t.lexer.skip(1)


def main():
    lines = None
    with open(FILE_SQUARE, 'r') as f:
        lines = f.readlines()

    content = list(filter(lambda line: not line.isspace(), lines))
    header, *program = content
    program = '\n'.join(program)

    lexer = lex.lex()
    lexer.input(program)

    # Tokenize
    tokenized = []
    while True:
        tok = lexer.token()
        if not tok:
            break  # No more input
        tokenized.append(tok)
        print(tok)


if __name__ == '__main__':
    main()
