
# ast.py
# By Ben Anderson
# December 2018

from err import CompilerError
from lexer import TokenSequence


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
        self.tk = TokenSequence(tokens)

    def gen(self):
        """
        Build an AST from the list of tokens.
        :return: The root of the built AST.
        """
        return self.parse_top_level_nodes()

    def parse_top_level_nodes(self):
        """
        Parse a sequence of declarations or function definitions at the top
        level of a source code file.
        :return: An array of top level nodes.
        """
        top_levels = []
        while self.tk.cur() is not None:
            top_levels.append(self.parse_top_level_node())
        return top_levels

    def parse_top_level_node(self):
        """
        Parse a single top level node (a function definition or a declaration),
        at the top level of a source code file.
        :return: A top level node.
        """
        # Only function definitions are allowed for now
        return FuncDef(self.tk)


class FuncDef:
    """
    Information about a top level function definition.
    """

    def __init__(self, tk):
        """
        Parse a top level function definition from a token sequence.
        :param tk: The token sequence.
        :return: The top level function definition.
        """
        start = tk.cur()

        # Return type and name
        self.return_type = BuiltInType(tk)
        self.name = tk.expect("identifier")
        tk.next()

        # Arguments list
        self.args = []
        self.parse_args(tk)

        # Function body
        self.body = []
        self.parse_body(tk)

        # The tokens that make up this function definition
        self.range = tk.combine(start, tk.prev())

    def parse_args(self, tk):
        """
        Parse an argument definition list.
        :param tk: The token sequence.
        """
        tk.expect("(")
        tk.next()
        while tk.cur().type != ")":
            self.args.append(FuncDefArg(tk))

            # Expect a comma after the argument
            if tk.cur().type == ",":
                tk.next()
            else:
                break
        tk.expect(")")
        tk.next()

    def parse_body(self, tk):
        """
        Parse the body of a function definition.
        :param tk: The token sequence.
        """
        tk.expect("{")
        tk.next()
        while tk.cur().type != "}":
            self.body.append(Statement(tk))
        tk.expect("}")
        tk.next()


class FuncDefArg:
    """
    Information about an argument to a function definition.
    """

    def __init__(self, tk):
        """
        Parse an argument to a function definition.
        :param tk: The token sequence.
        """
        self.type = BuiltInType(tk)
        self.name = tk.expect("identifier")
        tk.next()
        self.range = tk.combine(self.type.range, self.name)


class Statement:
    """
    A statement is something like an assignment, variable declaration, if
    statement, etc.
    """

    def __init__(self, tk):
        """
        Parses a statement.
        :param tk: The token sequence.
        """
        pass


class BuiltInType:
    """
    A list of all built in types. Throughout the compiler code, we make the
    assumption that a char is 8 bits, short is 16, int is 32, long is 32, long
    long is 64, float is 32, and double is 64.
    """
    # Base types
    VOID = "void"
    CHAR = "char"      # Signed 8 bit integer
    SHORT = "short"    # Signed 16 bit integer
    INT = "int"        # Signed 32 bit integer
    LLONG = "llong"    # Signed 64 bit integer
    FLOAT = "float"    # 32 bit floating point number
    DOUBLE = "double"  # 64 bit floating point number

    # Unsigned types
    UCHAR = "uchar"    # Unsigned 8 bit integer
    USHORT = "ushort"  # Unsigned 16 bit integer
    UINT = "uint"      # Unsigned 32 bit integer
    ULLONG = "ullong"  # Unsigned 64 bit integer

    def __init__(self, tk):
        """
        Parse a built-in type specifier from a token sequence.
        :param tk: The token sequence.
        """
        # Keep track of the first token that makes up this type identifier
        start = tk.cur()

        # Check if we've got a sign qualifier
        ok = False
        self.type = "int"  # Default to an int
        signed = True      # Default to a signed int
        if tk.cur().type in ["signed", "unsigned"]:
            ok = True
            if tk.cur().type == "unsigned":
                signed = False
            tk.next()

        # Check if we've got a type specifier
        if tk.cur().type in ["void", "char", "short", "int", "long", "float",
                             "double"]:
            ok = True
            self.type = tk.cur().type
            tk.next()

        # Check we actually saw a type specifier
        if not ok:
            msg = f"expected type specifier, got '{tk.cur().contents}'"
            raise CompilerError.from_tk(msg, tk.cur())

        # Sometimes we can add more type specifiers. Specifically, "short int",
        # "long int", "long long", "long long int", "long double"
        if self.type == "short":
            if tk.cur().type == "int":       # short int
                tk.next()
        elif self.type == "long":
            if tk.cur().type == "long":      # long long
                self.type = "llong"
                tk.next()
                if tk.cur().type == "int":   # long long int
                    tk.next()
            elif tk.cur().type == "int":     # long int
                tk.next()
            elif tk.cur().type == "double":  # long double
                self.type = "double"  # long double is the same as double
                tk.next()

        # Longs are the same as ints (both are 32 bits)
        if self.type == "long":
            self.type = "int"

        # Prepend 'u' if unsigned
        if not signed:
            self.type = "u" + self.type

        # Store which tokens make up the type specifier, for error printing
        # purposes
        self.range = tk.combine(start, tk.prev())
