
# err.py
# By Ben Anderson
# December 2018

from lexer import Token

import sys
import os


class CompilerError(Exception):
    """
    A generalised custom exception used throughout the compiler.

    Our error handling philosophy is simple: when something goes wrong, raise an
    exception immediately and terminate compilation. We don't try to recover
    from errors.
    """

    def __init__(self, description: str):
        """
        Create a compiler error.

        :param description: The error message to display.
        """
        super().__init__()
        self.description = description
        self.file = None
        self.line = None
        self.line_num = -1
        self.column_num = -1
        self.length = -1

    @staticmethod
    def from_token(description: str, token: Token):
        """
        Create a compiler error centred on a token.

        :param description: The error message to display.
        :param token:       The token to center the error on.
        :return:            A compiler error.
        """
        err = CompilerError(description)
        err.set_file(token.file)
        err.set_location(token.line_num, token.column_num, token.line)
        err.set_length(token.length)
        return err

    def set_file(self, file: str):
        """
        Associates a file with the error.

        :param file: The path to the file in which the error occurred.
        """
        self.file = file

    def set_location(self, line_num: int, column_num: int, line: str):
        """
        Associates a line and column number with the error. A file must be
        associated with the error before calling this function.

        :param line_num:   The line number the error occurred on.
        :param column_num: The column number the error occurred at.
        :param line:       The line the error occurred on (as a string).
        string.
        """
        assert(self.file is not None)
        self.line = line
        self.line_num = line_num
        self.column_num = column_num

    def set_length(self, length: int):
        """
        Draw an arrow under a portion of the line of code that's printed when
        displaying the error. The arrow will start at the error's assigned
        column number, and will be 'length' bytes long.

        :param length: The length of the arrow to draw under the line of code.
        """
        assert(self.file is not None)
        assert(self.line is not None)
        assert(self.line_num is not None)
        assert(self.column_num is not None)
        self.length = length

    def print(self):
        """
        Pretty prints the error to the standard error output, using colors
        and proper formatting.
        """
        # Error message
        print_color(Color.RED)
        print_color(Color.BOLD)
        print("error: ", end="")
        print_color(Color.WHITE)
        print(self.description, end="")
        print_color(Color.RESET)
        print("")  # End the line

        # File
        if self.file is None or len(self.file) == 0:
            return
        print_color(Color.BLUE)
        print_color(Color.BOLD)
        print("  --> ", end="")
        print_color(Color.RESET)
        print(self.file, end="")

        # Line number
        if self.line_num is None or self.line_num < 1:
            print("")  # Finish the previous line
            return
        print(":" + str(self.line_num), end="")

        # Column number
        if self.column_num is None or self.column_num < 1:
            print("")  # Finish the previous line
            return
        print(":" + str(self.column_num))

        # Source code line. We replace all tabs with 4 spaces to ensure
        # consistent spacing across various terminals
        if self.line is None:
            return
        spaces_per_tab = 4
        prefix = "  " + str(self.line_num) + " | "
        print_color(Color.BLUE)
        print_color(Color.BOLD)
        print(prefix, end="")
        print_color(Color.RESET)
        print(self.line.replace("\t", " " * spaces_per_tab))

        # Arrow. We need to account for the number of tabs we've replaced before
        # the start of the arrow
        if self.length is None or self.length <= 0:
            return
        num_tabs = self.line.count("\t", 0, self.column_num - 1)
        padding = self.column_num - 1 + num_tabs * (spaces_per_tab - 1)
        print(" " * (len(prefix) + padding), end="")
        print_color(Color.YELLOW)
        print_color(Color.BOLD)
        print("^" + "~" * (self.length - 1), end="")
        print_color(Color.RESET)
        print("")  # End the line


class Color:
    """
    ASCII color codes that change the style of the text printed to the standard
    output.
    """
    RESET = "\x1B[0m"
    RED = "\x1B[31m"
    GREEN = "\x1B[32m"
    YELLOW = "\x1B[33m"
    BLUE = "\x1B[34m"
    WHITE = "\x1B[37m"
    BOLD = "\x1B[1m"


def print_color(color: str):
    """
    Prints a color code to the standard output if colors are supported.

    :param color: The color code to print.
    """
    if supports_color():
        print(color, end="")


def supports_color():
    """
    Checks if the running terminal supports ASCII color codes. This method is
    modified from the Django source code.

    :return: True if the terminal supports colors, or False otherwise.
    """
    supported_platform = sys.platform != "win32" or "ANSICON" in os.environ
    is_a_tty = hasattr(sys.stdout, "isatty") and sys.stdout.isatty()
    return supported_platform and is_a_tty
