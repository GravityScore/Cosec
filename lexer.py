
# lexer.py
# By Ben Anderson
# December 2018

from __future__ import annotations
from typing import Optional
from enum import Enum
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
            if token.type == Tk.EOF:
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

    def expect(self, expected: Tk):
        """
        Triggers an exception if the current token is not of the expected type.

        :param expected: The expected token type.
        """
        if self.cur().type != expected:
            expected_str = TOKEN_NAMES[expected]
            found_str = TOKEN_NAMES[self.cur().type]
            desc = f"expected {expected_str}, found {found_str}"
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
            return self.token(Tk.EOF, 0)

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
        token = self.token(Tk.IDENT, 0)
        while not self.scanner.is_eof() and \
                is_ident_continue(self.scanner.cur()):
            self.scanner.next()
        token.set_length(self.scanner.cursor - token.start)

        # Check to see if the identifier is a reserved keyword
        try:
            token.type = Tk(token.contents)
        except ValueError:
            pass
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
        token = self.token(Tk.CONST_FLOAT, 0)
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
            self.scanner.next()
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
        token = self.token(Tk.CONST_FLOAT, 0)
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
        token = self.token(Tk.CONST_INT, 0)
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
        for syntax in SYNTAX_TOKENS:
            if self.scanner.slice(len(syntax.value)) == syntax.value:
                token = self.token(syntax, len(syntax.value))
                self.scanner.consume(len(syntax.value))
                return token
        return None

    def token(self, type: Tk, length: int) -> Token:
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
        n_end = self.source.find("\n", self.cursor)
        r_end = self.source.find("\r", self.cursor)
        if n_end >= 0 and r_end >= 0:
            end = min(n_end, r_end)
        elif n_end >= 0:
            end = n_end
        elif r_end >= 0:
            end = r_end
        else:
            # Add 1 to the source code length, because we subtract 1 below to
            # exclude the newline character
            end = len(self.source) + 1

        # Subtract 1 to exclude the newline character
        self.line = self.slice(end - self.cursor).rstrip()

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


class Tk(Enum):
    """
    A list of all possible token types.
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
    VOLATILE = "volatile"
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


class Token:
    """
    A token is the smallest building block of the language, like a comma,
    semicolon, number, or identifier.
    """

    def __init__(self, type: Tk):
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

    def name(self) -> str:
        """
        :return: A printable name for the token.
        """
        if self.type == Tk.IDENT or self.type == Tk.CONST_INT or \
                self.type == Tk.CONST_FLOAT:
            return "'" + self.contents + "'"
        elif self.type == Tk.CONST_CHAR or self.type == Tk.CONST_STR:
            return self.contents
        else:
            return TOKEN_NAMES[self.type]

    def combine(self, end: Token) -> Token:
        """
        Combine all tokens between this token and the end token into one large
        token, for use with errors.

        :param end: The last token in the combined range
        :return:    A token formed by combining all tokens between this one and
                    'end'.
        """
        # Ensure the start and end tokens are from the same file
        assert(self.file == end.file)
        assert(self.source == end.source)

        # Ensure the start token is before the end token
        if self.start > end.start:
            return end.combine(self)  # Swap the order

        # Create the combined token
        token = Token(Tk.COMBINED)
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
    Tk.AUTO, Tk.BREAK, Tk.CASE, Tk.CHAR, Tk.CONST,
    Tk.CONTINUE, Tk.DEFAULT, Tk.DO, Tk.DOUBLE, Tk.ELSE,
    Tk.ENUM, Tk.EXTERN, Tk.FLOAT, Tk.FOR, Tk.GOTO, Tk.IF,
    Tk.INLINE, Tk.INT, Tk.LONG, Tk.REGISTER, Tk.RESTRICT,
    Tk.RETURN, Tk.SHORT, Tk.SIGNED, Tk.SIZEOF, Tk.STATIC,
    Tk.STRUCT, Tk.SWITCH, Tk.TYPEDEF, Tk.UNION, Tk.UNSIGNED,
    Tk.VOID, Tk.VOLATILE, Tk.WHILE,
]


"""
A list of all syntax tokens, in descending order of length.
"""
SYNTAX_TOKENS = [
    # 3 character tokens
    Tk.RSHIFT_ASSIGN, Tk.LSHIFT_ASSIGN, Tk.ELLIPSIS,

    # 2 character tokens
    Tk.INC, Tk.DEC, Tk.ADD_ASSIGN, Tk.SUB_ASSIGN, Tk.MUL_ASSIGN,
    Tk.DIV_ASSIGN, Tk.MOD_ASSIGN, Tk.AND_ASSIGN, Tk.OR_ASSIGN,
    Tk.XOR_ASSIGN, Tk.RSHIFT, Tk.LSHIFT, Tk.ARROW,
    Tk.LOGICAL_AND, Tk.LOGICAL_OR, Tk.GE, Tk.LE, Tk.EQ,
    Tk.NEQ,

    # 1 character tokens
    Tk.ADD, Tk.SUB, Tk.MUL, Tk.DIV, Tk.MOD, Tk.GT, Tk.LT,
    Tk.ASSIGN, Tk.SEMICOLON, Tk.COMMA, Tk.OPEN_PAREN,
    Tk.CLOSE_PAREN, Tk.OPEN_BRACKET, Tk.CLOSE_BRACKET,
    Tk.OPEN_BRACE, Tk.CLOSE_BRACE, Tk.COLON, Tk.DOT,
    Tk.BITWISE_AND, Tk.BITWISE_OR, Tk.BITWISE_XOR, Tk.BITWISE_NOT,
    Tk.LOGICAL_NOT, Tk.TERNARY,
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


"""
A mapping between token type and its printable name.
"""
TOKEN_NAMES = {x: "'" + x.value + "'" for x in Tk}
TOKEN_NAMES[Tk.IDENT] = "identifier"
TOKEN_NAMES[Tk.CONST_INT] = "number"
TOKEN_NAMES[Tk.CONST_FLOAT] = "number"
TOKEN_NAMES[Tk.CONST_CHAR] = "character"
TOKEN_NAMES[Tk.CONST_STR] = "string"
TOKEN_NAMES[Tk.COMBINED] = "<combined>"
TOKEN_NAMES[Tk.EOF] = "end of file"
