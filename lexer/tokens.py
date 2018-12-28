
# lexer/tokens.py
# By Ben Anderson
# December 2018


class Token:
    """
    A token is the smallest building block of the language, like a comma,
    semicolon, number, or identifier.
    """

    # 3 character tokens
    RSHIFT_ASSIGN = ">>="
    LSHIFT_ASSIGN = "<<="
    ELLIPSIS = "..."

    # 2 character tokens
    INC = "++"
    DEC = "--"
    ADD_ASSIGN = "+="
    SUB_ASSIGN = "-="
    MUL_ASSIGN = "*="
    DIV_ASSIGN = "/="
    MOD_ASSIGN = "%="
    AND_ASSIGN = "&="
    OR_ASSIGN = "|="
    XOR_ASSIGN = "^="
    RSHIFT = ">>"
    LSHIFT = "<<"
    ARROW = "->"
    LOGICAL_AND = "&&"
    LOGICAL_OR = "||"
    GE = ">="
    LE = "<="
    EQ = "=="
    NEQ = "!="

    # 1 character tokens
    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"
    MOD = "%"
    GT = ">"
    LT = "<"
    ASSIGN = "="
    SEMICOLON = ";"
    COMMA = ","
    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    OPEN_BRACKET = "["
    CLOSE_BRACKET = "]"
    OPEN_BRACE = "{"
    CLOSE_BRACE = "}"
    COLON = ":"
    DOT = "."
    BITWISE_AND = "&"
    BITWISE_OR = "|"
    BITWISE_XOR = "^"
    BITWISE_NOT = "~"
    LOGICAL_NOT = "!"
    TERNARY = "?"

    # Keywords
    AUTO = "auto"
    BREAK = "break"
    CASE = "case"
    CHAR = "char"
    CONST = "const"
    CONTINUE = "continue"
    DEFAULT = "default"
    DO = "do"
    DOUBLE = "double"
    ELSE = "else"
    ENUM = "enum"
    EXTERN = "extern"
    FLOAT = "float"
    FOR = "for"
    GOTO = "goto"
    IF = "if"
    INLINE = "inline"
    INT = "int"
    LONG = "long"
    REGISTER = "register"
    RESTRICT = "restrict"
    RETURN = "return"
    SHORT = "short"
    SIGNED = "signed"
    SIZEOF = "sizeof"
    STATIC = "static"
    STRUCT = "struct"
    SWITCH = "switch"
    TYPEDEF = "typedef"
    UNION = "union"
    UNSIGNED = "unsigned"
    VOID = "void"
    VOLTATILE = "volatile"
    WHILE = "while"

    # Other
    IDENT = "identifier"
    CONST_INT = "integer"
    CONST_FLOAT = "floating point number"
    CONST_CHAR = "character"
    CONST_STR = "string"

    # Markers
    COMBINED = "combined"
    EOF = "end of file"

    def __init__(self, type: str):
        """
        Create a new token with values initialised to some meaningless defaults.

        :param type: The token's type, equal to one of the strings above.
        """
        self.type = type
        self.file = ""  # Initialise with some default values
        self.start = 0
        self.line_num = 1
        self.column_num = 1
        self.line = ""
        self.length = 0
        self.source = ""
        self.contents = ""

        # For CONST_INT and CONST_FLOAT only
        self.suffix = None
        self.number = None

    def set_length(self, length: int):
        """
        Adjusts the length of a token after it's been created, updating its
        'length' and 'contents' properties.

        :param length: The new length of the token.
        """
        self.length = length
        self.contents = self.source[self.start:self.start + length]


class IntSuffix:
    """
    All possible suffixes for an integer.
    """
    LONG = "long"
    LLONG = "long long"
    UINT = "unsigned"
    ULONG = "unsigned long"
    ULLONG = "unsigned long long"


class FloatSuffix:
    """
    All possible suffixes for a floating point number.
    """
    FLOAT = "float"
    LDOUBLE = "long double"


"""
A list of all reserved keywords.
"""
KEYWORDS = [
    Token.AUTO, Token.BREAK, Token.CASE, Token.CHAR, Token.CONST,
    Token.CONTINUE, Token.DEFAULT, Token.DO, Token.DOUBLE, Token.ELSE,
    Token.ENUM, Token.EXTERN, Token.FLOAT, Token.FOR, Token.GOTO, Token.IF,
    Token.INLINE, Token.INT, Token.LONG, Token.REGISTER, Token.RESTRICT,
    Token.RETURN, Token.SHORT, Token.SIGNED, Token.SIZEOF, Token.STATIC,
    Token.STRUCT, Token.SWITCH, Token.TYPEDEF, Token.UNION, Token.UNSIGNED,
    Token.VOID, Token.VOLTATILE, Token.WHILE,
]


"""
A list of all syntax tokens, in descending order of length.
"""
SYNTAX_TOKENS = [
    # 3 character tokens
    Token.RSHIFT_ASSIGN, Token.LSHIFT_ASSIGN, Token.ELLIPSIS,

    # 2 character tokens
    Token.INC, Token.DEC, Token.ADD_ASSIGN, Token.SUB_ASSIGN, Token.MUL_ASSIGN,
    Token.DIV_ASSIGN, Token.MOD_ASSIGN, Token.AND_ASSIGN, Token.OR_ASSIGN,
    Token.XOR_ASSIGN, Token.RSHIFT, Token.LSHIFT, Token.ARROW,
    Token.LOGICAL_AND, Token.LOGICAL_OR, Token.GE, Token.LE, Token.EQ,
    Token.NEQ,

    # 1 character tokens
    Token.ADD, Token.SUB, Token.MUL, Token.DIV, Token.MOD, Token.GT, Token.LT,
    Token.ASSIGN, Token.SEMICOLON, Token.COMMA, Token.OPEN_PAREN,
    Token.CLOSE_PAREN, Token.OPEN_BRACKET, Token.CLOSE_BRACKET,
    Token.OPEN_BRACE, Token.CLOSE_BRACE, Token.COLON, Token.DOT,
    Token.BITWISE_AND, Token.BITWISE_OR, Token.BITWISE_XOR, Token.BITWISE_NOT,
    Token.LOGICAL_NOT, Token.TERNARY,
]


"""
A mapping between characters and their corresponding float suffix.
"""
FLOAT_SUFFIXES = {
    "f": FloatSuffix.FLOAT, "F": FloatSuffix.FLOAT,
    "l": FloatSuffix.LDOUBLE, "L": FloatSuffix.LDOUBLE,
}


"""
A list of all integer suffixes.
"""
INT_SUFFIXES = {
    "l": IntSuffix.LONG, "L": IntSuffix.LONG,
    "ll": IntSuffix.LLONG, "LL": IntSuffix.LLONG,
    "u": IntSuffix.UINT, "U": IntSuffix.UINT,
    "ul": IntSuffix.ULONG, "uL": IntSuffix.ULONG,
    "Ul": IntSuffix.ULONG, "UL": IntSuffix.ULONG,
    "lu": IntSuffix.ULONG, "lU": IntSuffix.ULONG,
    "Lu": IntSuffix.ULONG, "LU": IntSuffix.ULONG,
    "ull": IntSuffix.ULLONG, "uLL": IntSuffix.ULLONG,
    "Ull": IntSuffix.ULLONG, "ULL": IntSuffix.ULLONG,
    "llu": IntSuffix.ULLONG, "llU": IntSuffix.ULLONG,
    "LLu": IntSuffix.ULLONG, "LLU": IntSuffix.ULLONG,
}
