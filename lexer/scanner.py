
# lexer/scanner.py
# By Ben Anderson
# December 2018

from err import Error


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

    def cur(self):
        """
        :return: The character at the current cursor position.
        """
        return self.peek(0)

    def peek(self, amount: int):
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

    def slice(self, length: int):
        """
        Returns a substring of the source code, starting at the current cursor
        position with the given length.

        This function doesn't do out of bounds checks, and will throw an
        exception.

        :param length: The length of the slice.
        :return:       A substring of the source code.
        """
        return self.source[self.cursor:self.cursor + length]

    def is_eof(self):
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

    def trigger_err(self, description: str, offset: int, length: int):
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
