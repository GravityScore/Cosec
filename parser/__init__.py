
# parser/__init__.py
# By Ben Anderson
# December 2018

from parser.declarations import *
from err import Error


class Parser:
    """
    The parser builds an abstract syntax tree (AST) from a list of tokens. An
    AST is a high-level representation of a C program, structured as a tree.
    """

    def __init__(self, tokens):
        """
        Create a parser from the given list of tokens.

        :param tokens: The tokens to build the AST from.
        """
        self.tk = tokens

    def gen(self):
        """
        Generate an AST from a list of tokens.

        :return: An array of root AST nodes.
        """
        return None
