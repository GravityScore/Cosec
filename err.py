
# err.py
# By Ben Anderson
# December 2018

from colorama import Fore, Style


class CompilerErrorBuilder:
    """
    Construct a compiler error using a chain of functions.
    """

    def __init__(self, message):
        self.error = CompilerError(message)

    def file(self, file):
        """
        Associates a file with the error.
        :param file: The path to the file in which the error occurred.
        """
        self.error.file = file
        return self

    def location(self, line_num, column_num, line):
        """
        Associates a line and column number with the error. A file must be
        associated with the error before calling this function.
        :param line_num: The line on which the error occurred.
        :param column_num: The column number at which the error occurred.
        :param line: The line of source code on which the error occurred, as a
        string.
        """
        assert(self.error.file is not None)
        self.error.line = line
        self.error.line_num = line_num
        self.error.column_num = column_num
        return self

    def arrow(self, length):
        """
        Will draw an arrow under the line of code when printing the error. The
        arrow starts at the column number given in the 'location' function, and
        will be 'length' characters long.
        :param length: The length of the arrow to draw under the line of code.
        """
        assert(self.error.file is not None)
        assert(self.error.line is not None)
        assert(self.error.line_num is not None)
        assert(self.error.column_num is not None)
        self.error.length = length
        return self

    def build(self):
        """
        Returns the constructed compiler error.
        """
        return self.error


class CompilerError(Exception):
    """
    A generalised custom exception used throughout the compiler. Cosec's error
    philosophy is pretty simple. When something goes wrong, raise an exception
    immediately and terminate compilation. We don't try to recover from an
    error and accumulate things that go wrong.
    """

    def __init__(self, message, file=None, line=None, line_num=None,
                 column_num=None, length=None):
        """
        Creates a compiler error.
        :param message: The error message.
        :param file: The file in which the error occurred (or None).
        :param line: The line of the file on which the error occurred (as a
        string), or None.
        :param line_num: The line number at which the error occurred (or None).
        :param column_num: The column number at which the error occurred (or
        None).
        :param length: The length of the token(s) causing the error, so we can
        draw an arrow underneath them indicating the exact cause of the error
        (or None for no arrow).
        """
        super().__init__()
        self.message = message
        self.file = file
        self.line = line
        self.line_num = line_num
        self.column_num = column_num
        self.length = length

    def pretty_print(self):
        """
        Pretty prints the compiler error to the standard output, using colors
        and proper formatting.

        Any tabs within the line associated with the error are converted to 4
        spaces, so it's displayed consistently across various terminals. The
        printed column number is not changed, but the position at which the
        arrow is drawn is updated.
        """
        # TODO
        print(Fore.RED + Style.BRIGHT + "error: " + Style.RESET_ALL + self.message)
