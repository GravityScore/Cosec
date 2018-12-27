
# lexer/__init__.py
# By Ben Anderson
# December 2018

from lexer.tokens import *
from lexer.scanner import *
from err import Error

import string


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

    def next(self):
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
        desc = f"unknown character '{self.scanner.cur()}'"
        self.scanner.trigger_err(desc, 0, 1)
        return None

    def lex_ident(self):
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

    def is_hex_float(self):
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

    def lex_hex_float(self):
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
            desc = "hexadecimal floats require an exponent"
            raise Error.from_token(desc, token)

        # Consume the exponent character
        self.scanner.next()

        # Check for an optional sign
        ch = self.scanner.cur()
        if ch == "+" or ch == "-":
            self.scanner.next()

        # Must have at least one digit following the exponent character
        if not is_decimal(self.scanner.cur()):
            self.scanner.trigger_err("missing digit after exponent", 0, 1)

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
            desc = f"invalid character '{ch}' after float"
            self.scanner.trigger_err(desc, 0, 1)
        return token

    def is_float(self):
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

    def lex_float(self):
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
                self.scanner.trigger_err("missing digit after exponent", 0, 1)

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
            desc = f"invalid character '{ch}' in number"
            self.scanner.trigger_err(desc, 0, 1)
        return token

    def lex_int(self, base=10):
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
            desc = f"invalid character '{ch}' in number"
            self.scanner.trigger_err(desc, 0, 1)
        return token

    def lex_num(self):
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

    def lex_syntax(self):
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

    def token(self, type: str, length: int):
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
        self.current = 0

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

    def cur(self):
        """
        :return: The current token.
        """
        return self.peek(0)

    def prev(self):
        """
        :return: The previous token, or None if the current token is the first
                 token in the file.
        """
        return self.peek(-1)

    def next(self):
        """
        Advances to the next token.
        """
        if self.current < len(self.tokens):
            self.current += 1

    def peek(self, amount: int):
        """
        :return: The token 'amount' tokens in front of (or behind) the current
                 token.
        """
        if self.current + amount < 0:
            return None
        elif self.current + amount >= len(self.tokens):
            return self.tokens[-1]  # The last token is EOF
        else:
            return self.tokens[self.current + amount]

    def expect(self, expected: Token):
        """
        Triggers an exception if the current token is not of the expected type.

        :param expected: The expected token type.
        """
        if self.cur().type != expected:
            desc = f"expected '{expected}', got '{self.cur().contents}'"
            raise Error.from_token(desc, self.cur())

    def save(self):
        """
        :return: Information necessary to restore the token sequence to its
                 current state.
        """
        return self.current

    def restore(self, saved):
        """
        Restores the cursor to an earlier saved location.

        :param saved: The information returned by 'save'.
        """
        self.current = saved

    def combine(self, start_tk: Token, end_tk: Token):
        """
        Combine all tokens between the given starting and ending tokens into one
        large token, for use with errors.

        :param start_tk: The starting token.
        :param end_tk:   The ending token.
        :return:         A token formed by combining all tokens between the
                         start and end tokens.
        """
        # Ensure the start and end tokens are from the same file
        assert(start_tk.file == end_tk.file)
        assert(start_tk.source == end_tk.source)

        # Ensure the start token is before the end token
        if start_tk.start > end_tk.start:
            return self.combine(end_tk, start_tk)  # Swap the order

        # Create the combined token
        token = Token(Token.COMBINED)
        token.file = start_tk.file
        token.start = start_tk.start
        token.line_num = start_tk.line_num
        token.column_num = start_tk.column_num
        token.line = start_tk.line
        token.source = start_tk.source
        token.set_length(end_tk.start + end_tk.length - start_tk.start)
        return token


def is_decimal(character: str):
    """
    Checks if the given character is a decimal digit.

    :param character: The character to check.
    :return:          True if the character is a decimal digit, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.digits


def is_hex(character: str):
    """
    Checks if the given character is a hexadecimal digit.

    :param character: The character to check.
    :return:          True if the character is a hexadecimal digit, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.hexdigits


def is_octal(character: str):
    """
    Checks if the given character is an octal digit.

    :param character: The character to check.
    :return:          True if the character is an octal digit, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.octdigits


def is_ident_start(character: str):
    """
    Checks if the given character can start an identifier.

    :param character: The character to check.
    :return:          True if the character can start an identifier, or False
                      otherwise.
    """
    if len(character) == 0:
        return False
    return character in string.ascii_letters or character == "_"


def is_ident_continue(character: str):
    """
    Checks if the given character can exist within an identifier.

    :param character: The character to check.
    :return:          True if the character can be part of an identifier, or
                      False otherwise.
    """
    if len(character) == 0:
        return False
    return is_ident_start(character) or is_decimal(character)

