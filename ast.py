
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


class BuiltInType:
    """
    A list of all built in types. Throughout the compiler code, we make the
    assumption that a char is 8 bits, short is 16, int is 32, long is 32, long
    long is 64, float is 32, and double is 64.
    """

    # Base types
    VOID = 0
    CHAR = 1     # Signed 8 bit integer
    SHORT = 2    # Signed 16 bit integer
    INT = 3      # Signed 32 bit integer
    LLONG = 5    # Signed 64 bit integer
    FLOAT = 5    # 32 bit floating point number
    DOUBLE = 6   # 64 bit floating point number

    # Unsigned types
    UCHAR = 7    # Unsigned 8 bit integer
    USHORT = 8   # Unsigned 16 bit integer
    UINT = 9     # Unsigned 32 bit integer
    ULLONG = 10  # Unsigned 64 bit integer
