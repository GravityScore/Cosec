
# error.py
# By Ben Anderson
# December 2018

import sys
import os
from enum import Enum


class Severity(Enum):
    """
    The severity of an error message, used to color code it when printing.
    """
    INFO = 0     # Grey (usually used to provide additional information)
    WARNING = 1  # Orange
    FATAL = 2    # Red


class Message:
    """
    A single message within an error. Consists of a description and optionally
    associated location.

    Multiple messages within one error can be used to provide more information
    about the error.
    """

    def __init__(self, severity: Severity, description: str, token=None):
        """
        Create an error message with an associated description and optional
        token.

        :param description: A message to display alongside the error.
        :param token:       An optional token to center the error on.
        """
        self.severity = severity
        self.description = description
        self.token = token

    def pretty_print(self):
        """
        Pretty prints the message to the standard error output, using colors
        and proper formatting.
        """
        # Description
        print_color(Color.BOLD)
        if self.severity == Severity.INFO:
            print_color(Color.BLUE)
            print("info: ", end="")
        elif self.severity == Severity.WARNING:
            print_color(Color.YELLOW)
            print("warning: ", end="")
        elif self.severity == Severity.FATAL:
            print_color(Color.RED)
            print("error: ", end="")
        print_color(Color.WHITE)
        print(self.description, end="")
        print_color(Color.RESET)
        print("")  # End the line

        # File
        if self.token is None or self.token.file is None or \
                len(self.token.file) == 0:
            return
        print_color(Color.BLUE)
        print_color(Color.BOLD)
        print("  --> ", end="")
        print_color(Color.RESET)
        print(self.token.file, end="")

        # Line number
        if self.token.line_num < 1:
            print("")  # Finish the previous line
            return
        print(":" + str(self.token.line_num), end="")

        # Column number
        if self.token.column_num < 1:
            print("")  # Finish the previous line
            return
        print(":" + str(self.token.column_num))

        # Source code line. We replace all tabs with 4 spaces to ensure
        # consistent spacing across various terminals
        if self.token.line is None:
            return
        spaces_per_tab = 4
        prefix = "  " + str(self.token.line_num) + " | "
        print_color(Color.BLUE)
        print_color(Color.BOLD)
        print(prefix, end="")
        print_color(Color.RESET)
        print(self.token.line.replace("\t", " " * spaces_per_tab))

        # Arrow. We need to account for the number of tabs we've replaced before
        # the start of the arrow
        if self.token.length is None or self.token.length < 0:
            return
        num_tabs = self.token.line.count("\t", 0, self.token.column_num - 1)
        padding = self.token.column_num - 1 + num_tabs * (spaces_per_tab - 1)
        print(" " * (len(prefix) + padding), end="")
        print_color(Color.YELLOW)
        print_color(Color.BOLD)
        print("^" + "~" * (self.token.length - 1), end="")
        print_color(Color.RESET)
        print("")  # End the line


class Error(Exception):
    """
    A generalised custom exception used throughout the compiler.

    The error handling philosophy is simple: when something goes wrong, raise an
    exception immediately and terminate compilation. We don't try to recover
    from errors.
    """

    def __init__(self, description: str, token=None, *more):
        """
        Create a compiler error with one or more messages. The first message in
        the list defaults to fatal severity, with the remaining defaulting to
        info severity.

        :param description: The error message to display.
        :param token:       An optional token to center the error on.
        :param more:        Ability to include additional messages.
        """
        super().__init__()
        self.messages = [Message(Severity.FATAL, description, token)]

        # Add any more messages
        assert(len(more) % 2 == 0)
        for i in range(0, len(more), 2):
            # Parse the arguments in description/token pairs
            description = more[i]
            token = more[i + 1]
            self.messages.append(Message(Severity.INFO, description, token))

    def pretty_print(self):
        """
        Pretty prints the error to the standard error output, using colors
        and proper formatting.
        """
        for message in self.messages:
            message.pretty_print()

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
