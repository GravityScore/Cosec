
# lexer.py
# By Ben Anderson
# December 2018

from err import CompilerError, CompilerErrorBuilder


class TokenSequence:
    """
    Stores a list of tokens and keeps track of the "current" token, allowing us
    to progressively advance through the token list.
    """

    def __init__(self, file, source):
        self._source = source
        self._file = file
        self._current = 0

        lexer = Lexer(file, source)
        self._tokens = lexer.tokenize()

    def cur(self):
        """
        Returns the current token, or None if we've reached the end of the
        tokens list.
        """
        return self.peek(0)

    def prev(self):
        """
        Returns the previous token, or None if there was no previous token.
        """
        return self.peek(-1)

    def next(self):
        """
        Advances to the next token.
        """
        if self._current < len(self._tokens):
            self._current += 1

    def peek(self, amount):
        """
        Returns the token 'amount' tokens in front of (or behind) the current
        token, or None if the requested token is out of bounds.
        """
        if self._current + amount < 0 or \
                self._current + amount >= len(self._tokens):
            return None
        else:
            return self._tokens[self._current + amount]

    def expect(self, expected):
        """
        Triggers an exception if the current token is not of the expected type.
        :param expected: The expected token type.
        """
        if self.cur().type != expected:
            self.expected(expected)
        else:
            return self.cur()

    def expected(self, expected):
        """
        Triggers an excepted token exception.
        :param expected: The token we expected.
        """
        msg = f"expected '{expected}', got '{self.cur().contents}'"
        raise CompilerError.from_tk(msg, self.cur())

    def combine(self, start_tk, end_tk):
        """
        Combine all tokens between the given starting and ending tokens into one
        large token, for use with errors.
        :param start_tk: The starting token.
        :param end_tk: The ending token.
        :return: A token formed by combining the tokens between the start and
        end tokens.
        """
        # Ensure the starting and ending tokens are from the same file
        assert(start_tk.file == end_tk.file)
        assert(start_tk.source == end_tk.source)

        # Ensure the start token is before the end token
        if start_tk.start > end_tk.start:
            return self.combine(end_tk, start_tk)  # Swap the order

        # Create the combined token
        token = Token("combined")
        token.file = start_tk.file
        token.start = start_tk.start
        token.line_num = start_tk.line_num
        token.column_num = start_tk.column_num
        token.line = start_tk.line
        token.source = start_tk.source
        token.set_length(end_tk.start + end_tk.length - start_tk.start)
        return token


class Lexer:
    """
    The lexer converts a source code file into an array of tokens. The lexer
    assumes the pre-processor has already been run, so it doesn't need to deal
    with pre-processor directives, comments, or care about whitespace.
    """

    def __init__(self, file, source):
        """
        Create a new lexer for tokenizing the given source code.
        :param file: The path to the file the source code was taken from, for
        use when an error occurs.
        :param source: The source code to tokenize.
        """
        self.file = file
        self.source = source
        self.cursor = 0
        self.line_num = 0  # Incremented by the call to _new_line() below
        self.column_num = 1
        self.line = ""
        self._new_line()

    def tokenize(self):
        """
        Creates an array of tokens from the source code.
        :return: The array of tokens.
        """
        tokens = []
        while True:
            token = self._next_token()
            if token is None:
                break
            tokens.append(token)
        return tokens

    def _next_token(self):
        """
        Reads the next token underneath the current cursor position, advancing
        the cursor to the start of the next token.
        :return: The token at the current cursor position in the source code.
        """
        # End of file
        if self._is_eof():
            return None

        # Whitespace
        if self._peek(0).isspace():
            while not self._is_eof() and self._peek(0).isspace():
                self._consume()
            return self._next_token()

        # Values
        if self._peek(0).isalpha() or self._peek(0) == "_":
            return self._identifier()
        if self._peek(0).isdigit():
            return self._number()

        # Syntax token
        token = self._syntax()
        if token is not None:
            return token

        # Unrecognised character error
        msg = f"unrecognised token '{self._peek(0)}'"
        raise CompilerErrorBuilder(msg)                           \
            .file(self.file)                                      \
            .location(self.line_num, self.column_num, self.line)  \
            .arrow(1)                                             \
            .build()

    def _identifier(self):
        """
        Assumes that an identifier starts at the current cursor position. Parses
        and returns the identifier token.
        :return: An identifier token.
        """
        # Find the first non-alphabetic, non-numeric character
        token = Token.from_lexer("identifier", 0, self)
        while not self._is_eof() and (self._peek(0).isalpha() or
                                      self._peek(0).isdigit() or
                                      self._peek(0) == "_"):
            self._consume()
        token.set_length(self.cursor - token.start)

        # Check to see if the identifier is a reserved keyword
        keywords = ["auto", "break", "case", "char", "const", "continue",
                    "default", "do", "double", "else", "enum", "extern",
                    "float", "for", "goto", "if", "inline", "int", "long",
                    "register", "restrict", "return", "short", "signed",
                    "sizeof", "static", "struct", "switch", "typedef", "union",
                    "unsigned", "void", "volatile", "while"]
        if token.contents in keywords:
            token.type = token.contents
        return token

    def _number(self):
        """
        Assumes that a number starts at the current cursor position. Parses and
        returns the number token.
        :return: A number token.
        """
        # Find the first non-numeric character
        token = Token.from_lexer("number", 0, self)
        while not self._is_eof() and self._peek(0).isdigit():
            self._consume()
        token.set_length(self.cursor - token.start)
        return token

    def _syntax(self):
        """
        Checks to see if a syntax token starts at the current cursor position.
        Returns None if this isn't the case.
        :return: A syntax token, or None if no syntax token starts at the
        current cursor position.
        """
        # All language tokens, in descending order of length
        syntax_tokens = [
            # 3 character tokens
            ">>=", "<<=", "...",

            # 2 character tokens
            "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", ">>",
            "<<", "->", "&&", "||", ">=", "<=", "==", "!=",

            # 1 character tokens
            "+", "-", "*", "/", "%", ">", "<", "=", ";", ",", "(", ")", "[",
            "]", "{", "}", ":", ".", "&", "|", "^", "~", "!", "?"
        ]

        # Iterate over syntax tokens in order
        token = None
        for syntax_token in syntax_tokens:
            if self._slice(len(syntax_token)) == syntax_token:
                token = Token.from_lexer(syntax_token, len(syntax_token), self)

                # Consume the characters that make up the token
                for _ in range(0, len(syntax_token)):
                    self._consume()
                break
        return token

    def _peek(self, amount):
        """
        Returns the character 'amount' places in front of the current cursor
        position.
        :param amount: The number of characters to peek forwards.
        :return: The character 'amount' places in front of the cursor, or None
        if the character is out of bounds.
        """
        if self.cursor + amount < len(self.source):
            return self.source[self.cursor + amount]
        else:
            return None

    def _slice(self, length):
        """
        Returns a slice of the source code starting at the current cursor
        position and extending for 'length' characters.
        :param length: The length of the slice.
        :return: The source code slice.
        """
        return self.source[self.cursor:self.cursor + length]

    def _is_eof(self):
        """
        Checks to see if the cursor has reached the end of the source code that
        we're tokenizing.
        :return: True if the cursor is at the end of the source code, and False
        otherwise.
        """
        return self.cursor >= len(self.source)

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
        end = min(self.source.find("\n", self.cursor),
                  self.source.find("\r", self.cursor))
        if end == -1:  # On the last line of the file
            end = len(self.source)

        # Subtract 1 to exclude the newline character
        self.line = self.source[self.cursor:end - 1]


class Token:
    """
    A token is a syntactical building block for a language. For example, a
    comma, number, or identifier.
    """

    def __init__(self, type):
        self.type = type
        self.file = None
        self.start = 0
        self.line_num = 1
        self.column_num = 1
        self.line = ""
        self.length = 0
        self.source = ""
        self.contents = ""

    @staticmethod
    def from_lexer(type, length, lexer):
        """
        Creates a new token with the given length, copying across information
        from the lexer.
        :param type: The type of token to create.
        :param length: The length of the token, in bytes.
        :param lexer: The lexer to copy information from.
        :return: The new token.
        """
        token = Token(type)
        token.type = type
        token.file = lexer.file
        token.start = lexer.cursor
        token.line_num = lexer.line_num
        token.column_num = lexer.column_num
        token.line = lexer.line
        token.source = lexer.source
        token.set_length(length)
        return token

    def set_length(self, length):
        """
        Adjusts the length of a token after it's been created, updating its
        contents string.
        :param length: The new length of the token.
        """
        self.length = length
        self.contents = self.source[self.start:self.start + length]
