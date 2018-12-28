
# compiler.py
# By Ben Anderson
# December 2018


class Compiler:
    """
    The intermediate representation (IR) stores a list of IR instructions
    organised in basic blocks. It's a lower-level representation of the C
    source code, and is where optimisation happens.
    """

    def __init__(self, ast_root):
        """
        Create a compiler that can build an IR graph from an AST.

        :param ast_root: The root node of the AST to build the IR graph from.
        """
        self.ast = ast_root

    def gen(self):
        """
        Build an IR graph from the AST.

        :return: The root basic block of the IR graph.
        """
        return None
