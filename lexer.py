
# lexer.py
# By Ben Anderson
# December 2018

from __future__ import annotations
from typing import Optional
import string

from error import Error


class Tokens:
    """
    Creates a list of tokens from some source code using the lexer. Keeps track
    of a 'current' token.
    """

    def __init__(self, file: str, source: str):
        """
        Create a new token list from some source code.

        :param file:   The path to the source code file.
        :param source: The contents of the file.
        """
        self.cursor = 0

        # Parse a list of tokens from a lexer
        lexer = Lexer(file, source)
        self.tokens = []
        while True:
            # Parse another token and add it to the list
            token = lexer.next()
            self.tokens.append(token)

            # Stop when we reach the end of the file
            if token.type == Token.EOF:
                break

    def cur(self) -> Token:
        """
        :return: The current token.
        """
        return self.peek(0)

    def prev(self) -> Token:
        """
        :return: The previous token, or None if the current token is the first
                 token in the file.
        """
        return self.peek(-1)

    def next(self):
        """
        Advances the cursor to the next token.
        """
        if self.cursor < len(self.tokens):
            self.cursor += 1

    def peek(self, amount: int) -> Token:
        """
        :return: The token 'amount' tokens in front of (or behind) the current
                 token.
        """
        if self.cursor + amount < 0 or self.cursor + amount >= len(self.tokens):
            return self.tokens[-1]  # The last token is EOF
        else:
            return self.tokens[self.cursor + amount]

    def expect(self, expected: Token):
        """
        Triggers an exception if the current token is not of the expected type.

        :param expected: The expected token type.
        """
        if self.cur().type != expected:
            desc = f"expected '{expected}', got '{self.cur().contents}'"
            raise Error(desc, self.cur())

    def save(self) -> int:
        """
        :return: Information necessary to restore the token sequence to its
                 current state.
        """
        return self.cursor

    def restore(self, saved: int):
        """
        Restores the cursor to an earlier saved location.

        :param saved: The information returned by 'save'.
        """
        self.cursor = saved


class Lexer:
    """
    The lexer converts a source code file into an array of tokens. The lexer
    assumes the pre-processor has already been run, so it doesn't need to deal
    with pre-processor directives, comments, or care about whitespace.
    """

    def __init__(self, file: str, source: str):
        """
        Create a new lexer for tokenizing the given source code.

        :param file:   The path to the file that we're lexing.
        :param source: The contents of the file.
        """
        self.scanner = Scanner(file, source)

    def next(self) -> Token:
        """
        Reads the next token at the current cursor position, advancing the
        cursor to the start of the next token.

        :return: The token at the current cursor position in the source code.
        """
        # End of file
        if self.scanner.is_eof():
            return self.token(Token.EOF, 0)

        # Whitespace
        if self.scanner.cur().isspace():
            self.scanner.consume_whitespace()
            return self.next()

        # Identifiers and keywords
        if is_ident_start(self.scanner.cur()):
            return self.lex_ident()

        # Numbers
        if is_decimal(self.scanner.cur()):
            return self.lex_num()
        if self.scanner.cur() == "." and is_decimal(self.scanner.peek(1)):
            return self.lex_float()

        # Syntax tokens
        syntax = self.lex_syntax()
        if syntax is not None:
            return syntax

        # Otherwise, trigger an error on the current character
        self.scanner.err(f"unknown character '{self.scanner.cur()}'", 0, 1)

    def lex_ident(self) -> Token:
        """
        Parses an identifier starting at the current cursor position.

        :return: An identifier token.
        """
        # Find the first non-alphabetic, non-numeric character
        token = self.token(Token.IDENT, 0)
        while not self.scanner.is_eof() and is_ident_continue(self.scanner.cur()):
            self.scanner.next()
        token.set_length(self.scanner.cursor - token.start)

        # Check to see if the identifier is a reserved keyword
        if token.contents in KEYWORDS:
            token.type = token.contents
        return token

    def is_hex_float(self) -> bool:
        """
        Checks to see if the number at the current cursor position is a
        hexadecimal float or not, after skipping the hexadecimal prefix '0x'.

        :return: True if the number at the cursor is a hexadecimal float, or
                 False otherwise.
        """
        # If we find a '.', 'e', 'E', 'p', or 'P' after a series of digits, then
        # its a float
        index = 0
        while is_hex(self.scanner.peek(index)):
            index += 1
        ch = self.scanner.peek(index)
        return ch == "." or ch == "e" or ch == "E" or ch == "p" or ch == "P"

    def lex_hex_float(self) -> Token:
        """
        Parses the hexadecimal floating point number at the current cursor
        position.

        :return: A number token.
        """
        token = self.token(Token.CONST_FLOAT, 0)
        token.start -= 2  # Include the hex prefix
        start = self.scanner.cursor

        # Sequence of digits with one optional dot
        found_dot = False
        while is_hex(self.scanner.cur()) or (self.scanner.cur() == "." and
                                             not found_dot):
            if self.scanner.cur() == ".":
                found_dot = True
            self.scanner.next()

        # Require an exponent
        ch = self.scanner.cur()
        if ch != "p" and ch != "P":
            token.set_length(self.scanner.cursor - token.start)
            raise Error("hexadecimal floats require an exponent", token)

        # Consume the exponent character
        self.scanner.next()

        # Check for an optional sign
        ch = self.scanner.cur()
        if ch == "+" or ch == "-":
            self.scanner.next()

        # Must have at least one digit following the exponent character
        if not is_decimal(self.scanner.cur()):
            self.scanner.err("missing digit after exponent", 0, 1)

        # Consume a sequence of DECIMAL digits that constitutes the exponent
        while is_decimal(self.scanner.cur()):
            self.scanner.next()

        # Parse the number
        contents = self.scanner.source[start:self.scanner.cursor]
        token.number = float.fromhex(contents)

        # Check for a float suffix
        ch = self.scanner.cur().lower()
        if ch in FLOAT_SUFFIXES:
            token.suffix = FLOAT_SUFFIXES[ch]
            self.scanner.consume(len(ch))
        token.set_length(self.scanner.cursor - start)

        # Check there's nothing after the suffix
        ch = self.scanner.cur()
        if is_ident_continue(ch):
            self.scanner.err(f"invalid character '{ch}' after float", 0, 1)
        return token

    def is_float(self) -> bool:
        """
        Checks to see if the number at the current cursor position is a
        float or not.

        :return: True if the number at the cursor is a float, or False
                 otherwise.
        """
        # If we find a '.', 'e', or 'E' after a series of digits, then its a
        # float
        index = 0
        while is_decimal(self.scanner.peek(index)):
            index += 1
        ch = self.scanner.peek(index)
        return ch == "." or ch == "e" or ch == "E"

    def lex_float(self) -> Token:
        """
        Parses the floating point number at the current cursor position.

        :return: A number token.
        """
        token = self.token(Token.CONST_FLOAT, 0)
        token.start -= 2  # Include the hex prefix
        start = self.scanner.cursor

        # Sequence of digits with one optional dot
        found_dot = False
        while is_decimal(self.scanner.cur()) or (self.scanner.cur() == "." and
                                                 not found_dot):
            if self.scanner.cur() == ".":
                found_dot = True
            self.scanner.next()

        # Check for an exponent
        ch = self.scanner.cur()
        if ch == "e" or ch == "E":
            # Skip the exponent character
            self.scanner.next()

            # Check for an optional sign
            ch = self.scanner.cur()
            if ch == "+" or ch == "-":
                self.scanner.next()

            # Must have at least one digit following the exponent character
            if not is_decimal(self.scanner.cur()):
                self.scanner.err("missing digit after exponent", 0, 1)

            # Consume a sequence of digits that constitutes the exponent
            while not self.scanner.is_eof() and is_decimal(self.scanner.cur()):
                self.scanner.next()

        # Parse the number
        token.number = float(self.scanner.source[start:self.scanner.cursor])

        # Check for a float suffix
        ch = self.scanner.cur()
        if ch in FLOAT_SUFFIXES.keys():
            token.suffix = FLOAT_SUFFIXES[ch]
            self.scanner.consume(len(ch))
        token.set_length(self.scanner.cursor - start)

        # Check there's nothing after the suffix
        ch = self.scanner.cur()
        if is_ident_continue(ch):
            self.scanner.err(f"invalid character '{ch}' in number", 0, 1)
        return token

    def lex_int(self, base=10) -> Token:
        """
        Parses an integer (in the given base) at the current cursor position.

        :param base: The base in which to parse the integer.
        :return:     A number token.
        """
        predicates = {8: is_octal, 10: is_decimal, 16: is_hex}
        predicate = predicates[base]

        # Consume a series of valid digits
        token = self.token(Token.CONST_INT, 0)
        while predicate(self.scanner.cur()):
            self.scanner.next()

        # Parse the number
        token.set_length(self.scanner.cursor - token.start)
        token.number = int(token.contents, base)

        # Check for an integer suffix. Sort the suffixes in descending order of
        # length, so longer more specific suffixes are matched first
        int_suffixes = list(INT_SUFFIXES.keys())
        int_suffixes.sort(key=len, reverse=True)
        for suffix in int_suffixes:
            if self.scanner.slice(len(suffix)) == suffix:
                token.suffix = INT_SUFFIXES[suffix]
                self.scanner.consume(len(suffix))
                break

        # Check there's nothing after the suffix
        ch = self.scanner.cur()
        if is_ident_continue(ch):
            self.scanner.err(f"invalid character '{ch}' in number", 0, 1)
        return token

    def lex_num(self) -> Token:
        """
        Parses an integer or floating point number starting at the current
        cursor position.

        :return: A number token.
        """
        # Check for a hexadecimal prefix
        prefix = self.scanner.slice(2)
        if prefix == "0x" or prefix == "0X":
            # Skip the hex prefix
            self.scanner.consume(2)

            # Check if we've got a hexadecimal float or int
            if self.is_hex_float():
                return self.lex_hex_float()
            else:
                return self.lex_int(16)

        # Check for a float
        if self.is_float():
            return self.lex_float()

        # Check for an octal constant
        if self.scanner.cur() == "0":
            return self.lex_int(8)

        # Otherwise, we've got a decimal integer
        return self.lex_int()

    def lex_syntax(self) -> Optional[Token]:
        """
        Checks to see if a syntax token starts at the current cursor position.
        Returns None if this isn't the case.

        :return: A syntax token (or None).
        """
        # Iterate over syntax tokens in order
        for syntax_token in SYNTAX_TOKENS:
            if self.scanner.slice(len(syntax_token)) == syntax_token:
                token = self.token(syntax_token, len(syntax_token))
                self.scanner.consume(len(syntax_token))
                return token
        return None

    def token(self, type: str, length: int) -> Token:
        """
        Creates a new token with the given type and arrow length. Copies across
        the file, line, and column information from the scanner.

        :param type:   The type of token to create.
        :param length: The length of the token.
        :return:       The new token.
        """
        token = Token(type)
        token.file = self.scanner.file
        token.start = self.scanner.cursor
        token.line_num = self.scanner.line_num
        token.column_num = self.scanner.column_num
        token.line = self.scanner.line
        token.source = self.scanner.source
        token.set_length(length)
        return token


class Scanner:
    """
    A scanner maintains a cursor position, line number, and column number
    within a source code string.
    """

    def __init__(self, file: str, source: str):
        """
        Creates a scanner over the given file, with the cursor starting at the
        first character in the file.

        :param file:   The path to the file that we're scanning.
        :param source: The contents of the file.
        """
        self.file = file
        self.source = source
        self.cursor = 0
        self.line = ""
        self.line_num = 0  # Incremented by the call to 'newline' below
        self.column_num = 1
        self.newline()     # Updates the 'line' property for the first time

    def cur(self) -> str:
        """
        :return: The character at the current cursor position.
        """
        return self.peek(0)

    def peek(self, amount: int) -> str:
        """
        Returns the character that's a certain number of bytes in front of (or
        behind) the current cursor position.

        :param amount: The number of characters to peek.
        :return:       The character 'amount' places in front of the cursor, or
                       an empty string if the character is out of bounds.
        """
        pos = self.cursor + amount
        if 0 <= pos < len(self.source):
            return self.source[pos]
        else:
            return ""

    def slice(self, length: int) -> str:
        """
        Returns a substring of the source code, starting at the current cursor
        position with the given length.

        This function doesn't do out of bounds checks, and will throw an
        exception.

        :param length: The length of the slice.
        :return:       A substring of the source code.
        """
        return self.source[self.cursor:self.cursor + length]

    def is_eof(self) -> bool:
        """
        Checks to see if the cursor has reached the end of the source code.

        :return: True if the cursor is at the end of the source code, or False
                 otherwise.
        """
        return self.cursor >= len(self.source)

    def next(self):
        """
        Moves the cursor one character forwards. Updates the current line and
        column number.
        """
        if self.cur() == "\r" and self.peek(1) == "\n":
            # Treat '\r\n' as a single newline (fuck Windows)
            self.cursor += 2
            self.newline()
        elif self.cur() == "\r" or self.cur() == "\n":
            # Single newline
            self.cursor += 1
            self.newline()
        else:
            # Not a newline character
            self.cursor += 1
            self.column_num += 1

    def newline(self):
        """
        Advances the cursor to the next line, updating the line number, column
        number, and line string.
        """
        self.line_num += 1
        self.column_num = 1

        # Find the end of the current line
        end = min(self.source.find("\n", self.cursor),
                  self.source.find("\r", self.cursor))

        # This happens if the cursor is on the last line in the file
        if end == -1:
            # Add 1 to the source code length, because we subtract 1 below to
            # exclude the newline character
            end = len(self.source) + 1

        # Subtract 1 to exclude the newline character
        self.line = self.slice(end).rstrip()

    def consume(self, amount: int):
        """
        Advances the cursor forward by 'amount' characters.

        :param amount: The number of bytes to advance the cursor by.
        """
        for _ in range(amount):
            self.next()

    def consume_whitespace(self):
        """
        Advances the cursor over all whitespace characters (as determined by the
        standard library function 'isspace').
        """
        while not self.is_eof() and self.cur().isspace():
            self.next()

    def err(self, description: str, offset: int, length: int):
        """
        Triggers a compiler error with the given description. The arrow starts
        at the current cursor position, plus the given start offset. The arrow
        has the given length.

        :param description:  The error message to display.
        :param offset: The offset from the cursor position to start the
                             arrow at.
        :param length:       The length of the arrow.
        """
        err = Error(description)
        err.set_file(self.file)
        err.set_location(self.line_num, self.column_num + offset, self.line)
        err.set_length(length)
        raise err


class Token:
    """
    A token is the smallest building block of the language, like a comma,
    semicolon, number, or identifier.
    """

    # 3 character tokens
    RSHIFT_ASSIGN = ">>="
    LSHIFT_ASSIGN = "<<="
    ELLIPSIS = "..."

    # 2 character tokens
    INC = "++"
    DEC = "--"
    ADD_ASSIGN = "+="
    SUB_ASSIGN = "-="
    MUL_ASSIGN = "*="
    DIV_ASSIGN = "/="
    MOD_ASSIGN = "%="
    AND_ASSIGN = "&="
    OR_ASSIGN = "|="
    XOR_ASSIGN = "^="
    RSHIFT = ">>"
    LSHIFT = "<<"
    ARROW = "->"
    LOGICAL_AND = "&&"
    LOGICAL_OR = "||"
    GE = ">="
    LE = "<="
    EQ = "=="
    NEQ = "!="

    # 1 character tokens
    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"
    MOD = "%"
    GT = ">"
    LT = "<"
    ASSIGN = "="
    SEMICOLON = ";"
    COMMA = ","
    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    OPEN_BRACKET = "["
    CLOSE_BRACKET = "]"
    OPEN_BRACE = "{"
    CLOSE_BRACE = "}"
    COLON = ":"
    DOT = "."
    BITWISE_AND = "&"
    BITWISE_OR = "|"
    BITWISE_XOR = "^"
    BITWISE_NOT = "~"
    LOGICAL_NOT = "!"
    TERNARY = "?"

    # Keywords
    AUTO = "auto"
    BREAK = "break"
    CASE = "case"
    CHAR = "char"
    CONST = "const"
    CONTINUE = "continue"
    DEFAULT = "default"
    DO = "do"
    DOUBLE = "double"
    ELSE = "else"
    ENUM = "enum"
    EXTERN = "extern"
    FLOAT = "float"
    FOR = "for"
    GOTO = "goto"
    IF = "if"
    INLINE = "inline"
    INT = "int"
    LONG = "long"
    REGISTER = "register"
    RESTRICT = "restrict"
    RETURN = "return"
    SHORT = "short"
    SIGNED = "signed"
    SIZEOF = "sizeof"
    STATIC = "static"
    STRUCT = "struct"
    SWITCH = "switch"
    TYPEDEF = "typedef"
    UNION = "union"
    UNSIGNED = "unsigned"
    VOID = "void"
    VOLTATILE = "volatile"
    WHILE = "while"

    # Other
    IDENT = "identifier"
    CONST_INT = "integer"
    CONST_FLOAT = "floating point number"
    CONST_CHAR = "character"
    CONST_STR = "string"

    # Markers
    COMBINED = "combined"
    EOF = "end of file"

    def __init__(self, type: str):
        """
        Create a new token with values initialised to some meaningless defaults.

        :param type: The token's type, equal to one of the strings above.
        """
        self.type = type
        self.file = ""  # Initialise with some default values
        self.start = 0
        self.line_num = 1
        self.column_num = 1
        self.line = ""
        self.length = 0
        self.source = ""
        self.contents = ""

        # For CONST_INT and CONST_FLOAT only
        self.suffix = None
        self.number = None

    def set_length(self, length: int):
        """
        Adjusts the length of a token after it's been created, updating its
        'length' and 'contents' properties.

        :param length: The new length of the token.
        """
        self.length = length
        self.contents = self.source[self.start:self.start + length]

    def combine(self, end: Token) -> Token:
        """
        Combine all tokens between the given starting and ending tokens into one
        large token, for use with errors.

        :param start: The starting token.
        :param end:   The ending token.
        :return:      A token formed by combining all tokens between the start
                      and end tokens.
        """
        # Ensure the start and end tokens are from the same file
        assert(self.file == end.file)
        assert(self.source == end.source)

        # Ensure the start token is before the end token
        if self.start > end.start:
            return end.combine(self)  # Swap the order

        # Create the combined token
        token = Token(Token.COMBINED)
        token.file = self.file
        token.start = self.start
        token.line_num = self.line_num
        token.column_num = self.column_num
        token.line = self.line
        token.source = self.source
        token.set_length(end.start + end.length - self.start)
        return token


class IntSuffix:
    """
    All possible suffixes for an integer.
    """
    LONG = "long"
    LLONG = "long long"
    UINT = "unsigned"
    ULONG = "unsigned long"
    ULLONG = "unsigned long long"


class FloatSuffix:
    """
    All possible suffixes for a floating point number.
    """
    FLOAT = "float"
    LDOUBLE = "long double"


def is_decimal(character: str) -> bool:
    """
    Checks if the given character is a decimal digit.

    :param character: The character to check.
    :return:          True if the character is a decimal digit, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.digits


def is_hex(character: str) -> bool:
    """
    Checks if the given character is a hexadecimal digit.

    :param character: The character to check.
    :return:          True if the character is a hexadecimal digit, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.hexdigits


def is_octal(character: str) -> bool:
    """
    Checks if the given character is an octal digit.

    :param character: The character to check.
    :return:          True if the character is an octal digit, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.octdigits


def is_ident_start(character: str) -> bool:
    """
    Checks if the given character can start an identifier.

    :param character: The character to check.
    :return:          True if the character can start an identifier, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.ascii_letters or character == "_"


def is_ident_continue(character: str) -> bool:
    """
    Checks if the given character can exist within an identifier.

    :param character: The character to check.
    :return:          True if the character can be part of an identifier, or
                      False otherwise.
    """
    if len(character) == 0:
        return False
    return is_ident_start(character) or is_decimal(character)


"""
A list of all reserved keywords.
"""
KEYWORDS = [
    Token.AUTO, Token.BREAK, Token.CASE, Token.CHAR, Token.CONST,
    Token.CONTINUE, Token.DEFAULT, Token.DO, Token.DOUBLE, Token.ELSE,
    Token.ENUM, Token.EXTERN, Token.FLOAT, Token.FOR, Token.GOTO, Token.IF,
    Token.INLINE, Token.INT, Token.LONG, Token.REGISTER, Token.RESTRICT,
    Token.RETURN, Token.SHORT, Token.SIGNED, Token.SIZEOF, Token.STATIC,
    Token.STRUCT, Token.SWITCH, Token.TYPEDEF, Token.UNION, Token.UNSIGNED,
    Token.VOID, Token.VOLTATILE, Token.WHILE,
]


"""
A list of all syntax tokens, in descending order of length.
"""
SYNTAX_TOKENS = [
    # 3 character tokens
    Token.RSHIFT_ASSIGN, Token.LSHIFT_ASSIGN, Token.ELLIPSIS,

    # 2 character tokens
    Token.INC, Token.DEC, Token.ADD_ASSIGN, Token.SUB_ASSIGN, Token.MUL_ASSIGN,
    Token.DIV_ASSIGN, Token.MOD_ASSIGN, Token.AND_ASSIGN, Token.OR_ASSIGN,
    Token.XOR_ASSIGN, Token.RSHIFT, Token.LSHIFT, Token.ARROW,
    Token.LOGICAL_AND, Token.LOGICAL_OR, Token.GE, Token.LE, Token.EQ,
    Token.NEQ,

    # 1 character tokens
    Token.ADD, Token.SUB, Token.MUL, Token.DIV, Token.MOD, Token.GT, Token.LT,
    Token.ASSIGN, Token.SEMICOLON, Token.COMMA, Token.OPEN_PAREN,
    Token.CLOSE_PAREN, Token.OPEN_BRACKET, Token.CLOSE_BRACKET,
    Token.OPEN_BRACE, Token.CLOSE_BRACE, Token.COLON, Token.DOT,
    Token.BITWISE_AND, Token.BITWISE_OR, Token.BITWISE_XOR, Token.BITWISE_NOT,
    Token.LOGICAL_NOT, Token.TERNARY,
]


"""
A mapping between characters and their corresponding float suffix.
"""
FLOAT_SUFFIXES = {
    "f": FloatSuffix.FLOAT, "F": FloatSuffix.FLOAT,
    "l": FloatSuffix.LDOUBLE, "L": FloatSuffix.LDOUBLE,
}


"""
A list of all integer suffixes.
"""
INT_SUFFIXES = {
    "l": IntSuffix.LONG, "L": IntSuffix.LONG,
    "ll": IntSuffix.LLONG, "LL": IntSuffix.LLONG,
    "u": IntSuffix.UINT, "U": IntSuffix.UINT,
    "ul": IntSuffix.ULONG, "uL": IntSuffix.ULONG,
    "Ul": IntSuffix.ULONG, "UL": IntSuffix.ULONG,
    "lu": IntSuffix.ULONG, "lU": IntSuffix.ULONG,
    "Lu": IntSuffix.ULONG, "LU": IntSuffix.ULONG,
    "ull": IntSuffix.ULLONG, "uLL": IntSuffix.ULLONG,
    "Ull": IntSuffix.ULLONG, "ULL": IntSuffix.ULLONG,
    "llu": IntSuffix.ULLONG, "llU": IntSuffix.ULLONG,
    "LLu": IntSuffix.ULLONG, "LLU": IntSuffix.ULLONG,
}
