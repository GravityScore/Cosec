
# ast.py
# By Ben Anderson
# December 2018


class AstGenerator:
    """
    The abstract syntax tree (AST) is a high level representation of C source
    code as a graph.
    """

    def __init__(self, tokens):
        """
        Create an AST generator that can build an AST from a list of tokens.
        :param tokens: The tokens to build the AST from.
        """
        self.tokens = tokens

    def gen(self):
        """
        Build an AST from the list of tokens.
        :return: The root of the built AST.
        """
        return None
