
# lexer.py
# By Ben Anderson
# December 2018

from err import CompilerErrorBuilder


class Lexer:
    """
    The lexer converts a source code file into an array of tokens. The lexer
    assumes the pre-processor has already been run, so it doesn't need to deal
    with pre-processor directives, comments, or care about whitespace.
    """

    def __init__(self, file, source_code):
        """
        Create a new lexer for tokenizing the given source code.
        :param file: The path to the file the source code was taken from, for
        use when an error occurs.
        :param source_code: The source code to tokenize.
        """
        self.file = file
        self.source_code = source_code
        self.cursor = 0
        self.line_num = 0
        self.column_num = 1
        self.line = ""
        self._new_line()

    def tokenize(self):
        """
        Creates an array of tokens from the source code.
        :return: The array of tokens.
        """
        tokens = []
        while not self._is_eof():
            token = self._next_token()
            if token is not None:
                tokens.append(token)
        return tokens

    def _next_token(self):
        """
        Reads the next token underneath the current cursor position, advancing
        the cursor to the start of the next token.
        :return: The token at the current cursor position in the source code.
        """
        if self._is_eof():
            return None

        ch = self._peek(0)

        # Whitespace
        if ch.isspace():
            self._consume_whitespace()
            token = self._next_token()

        # Arithmetic
        elif ch == "+":
            token = Token(TokenType.PLUS, 1, self)
            self._consume()
        elif ch == "-":
            token = Token(TokenType.MINUS, 1, self)
            self._consume()
        elif ch == "*":
            token = Token(TokenType.ASTERISK, 1, self)
            self._consume()
        elif ch == "/":
            token = Token(TokenType.FSLASH, 1, self)
            self._consume()

        # Values
        elif ch.isalpha() or ch == "_":
            token = self._ident()
        elif ch.isdigit():
            token = self._num()

        # Unrecognised character error
        else:
            raise CompilerErrorBuilder(f"unrecognised character '{ch}'") \
                .file(self.file)                                         \
                .location(self.line_num, self.column_num, self.line)     \
                .arrow(1)                                                \
                .build()

        return token

    def _ident(self):
        """
        Assumes that an identifier starts at the current cursor position. Parses
        and returns the identifier token.
        :return: An identifier token.
        """
        # Find the first non-alphabetic, non-numeric character
        token = Token(TokenType.IDENT, 0, self)
        while not self._is_eof() and (self._peek(0).isalpha() or
                                      self._peek(0).isdigit() or
                                      self._peek(0) == "_"):
            self._consume()
        token.set_length(self.cursor - token.start)

        # Check to see if the identifier is a reserved keyword
        types = ["void", "char", "short", "int", "long", "float", "double"]
        if token.contents in types:
            token.type = TokenType.TYPE

        return token

    def _num(self):
        """
        Assumes that a number starts at the current cursor position. Parses and
        returns the number token.
        :return: A number token.
        """
        # TODO: spec compliance
        # Find the first non-numeric character
        token = Token(TokenType.NUM, 0, self)
        while not self._is_eof() and self._peek(0).isdigit():
            self._consume()
        token.set_length(self.cursor - token.start)
        return token

    def _consume_whitespace(self):
        """
        Moves the cursor past any whitespace.
        """
        while not self._is_eof() and self._peek(0).isspace():
            self._consume()

    def _consume(self):
        """
        Moves the cursor one character forwards. Updates the current line and
        column number.
        """
        if self._peek(0) == "\r" and self._peek(1) == "\n":
            # Treat \r\n as a single newline (fuck Windows)
            self.cursor += 2
            self._new_line()
        elif self._peek(0) == "\r" or self._peek(0) == "\n":
            # Single newline
            self.cursor += 1
            self._new_line()
        else:
            # Some other character
            self.cursor += 1
            self.column_num += 1

    def _new_line(self):
        """
        Advances the cursor to the next line, updating the line number, column
        number, and line string.
        """
        self.line_num += 1
        self.column_num = 1

        # Extract the contents of the line as a string
        end = min(self.source_code.find("\n", self.cursor),
                  self.source_code.find("\r", self.cursor))
        if end == -1:
            end = len(self.source_code)
        self.line = self.source_code[self.cursor:end - 1]

    def _peek(self, amount):
        """
        Returns the character 'amount' places in front of the current cursor
        position.
        :param amount: The number of characters to peek forwards.
        :return: The character 'amount' places in front of the cursor, or None
        if the character is out of bounds.
        """
        if self.cursor + amount < len(self.source_code):
            return self.source_code[self.cursor + amount]
        else:
            return None

    def _is_eof(self):
        """
        Checks to see if the cursor has reached the end of the source code that
        we're tokenizing.
        :return: True if the cursor is at the end of the source code, and False
        otherwise.
        """
        return self.cursor >= len(self.source_code)


class Token:
    """
    A token is a syntactical building block for a language; for example, a
    comma, number, or identifier.
    """

    def __init__(self, type, length, lexer):
        """
        Creates a new token with the given length, copying across information
        from the lexer.
        :param type: The type of token to create.
        :param length: The length of the token, in bytes.
        :param lexer: The lexer to copy information from.
        """
        self.type = type
        self.start = lexer.cursor
        self.line_num = lexer.line_num
        self.column_num = lexer.column_num
        self.line = lexer.line
        self.length = length
        self.source_code = lexer.source_code
        self.contents = lexer.source_code[lexer.cursor:lexer.cursor + length]

    def set_length(self, length):
        """
        Changes the length of the token after initialisation, updating both the
        token's length and its contents.
        :param length: The new length of the token.
        """
        self.length = length
        self.contents = self.source_code[self.start:self.start + length]


class TokenType:
    """
    A list of all possible token types.
    """
    # Arithmetic
    PLUS = 0      # Addition
    MINUS = 1     # Subtraction
    ASTERISK = 2  # Multiplication
    FSLASH = 3    # Division (forward slash)

    # Keywords
    TYPE = 4  # Built-in types (void, char, short, int, long, float, double)

    # Values
    IDENT = 5  # Identifiers
    NUM = 6    # Numbers
