
# lexer/tokens.py
# By Ben Anderson
# December 2018


class TokenType:
    """
    A list of all possible tokens that exist in the C language.
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


class Token:
    """
    A token is the smallest building block of the C language, like a comma,
    semicolon, number, or identifier.
    """

    def __init__(self, type: TokenType):
        """
        Create a new token with values initialised to some meaningless defaults.

        :param type: The token's type.
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

        # For TokenType.CONST_INT and TokenType.CONST_FLOAT only
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
    All possible suffixes for an integer (in lowercase).
    """
    LONG = 0
    LLONG = 1
    UNSIGNED = 2
    UNSIGNED_LONG = 3
    UNSIGNED_LLONG = 4


class FloatSuffix:
    """
    All possible suffixes for a floating point number (in lowercase).
    """
    FLOAT = 0
    LONG_DOUBLE = 1


"""
A list of all reserved keywords.
"""
KEYWORDS = [
    TokenType.AUTO, TokenType.BREAK, TokenType.CASE, TokenType.CHAR,
    TokenType.CONST, TokenType.CONTINUE, TokenType.DEFAULT, TokenType.DO,
    TokenType.DOUBLE, TokenType.ELSE, TokenType.ENUM, TokenType.EXTERN,
    TokenType.FLOAT, TokenType.FOR, TokenType.GOTO, TokenType.IF,
    TokenType.INLINE, TokenType.INT, TokenType.LONG, TokenType.REGISTER,
    TokenType.RESTRICT, TokenType.RETURN, TokenType.SHORT, TokenType.SIGNED,
    TokenType.SIZEOF, TokenType.STATIC, TokenType.STRUCT, TokenType.SWITCH,
    TokenType.TYPEDEF, TokenType.UNION, TokenType.UNSIGNED, TokenType.VOID,
    TokenType.VOLTATILE, TokenType.WHILE,
]


"""
A list of all syntax tokens, in descending order of length.
"""
SYNTAX_TOKENS = [
    # 3 character tokens
    TokenType.RSHIFT_ASSIGN, TokenType.LSHIFT_ASSIGN, TokenType.ELLIPSIS,

    # 2 character tokens
    TokenType.INC, TokenType.DEC, TokenType.ADD_ASSIGN, TokenType.SUB_ASSIGN,
    TokenType.MUL_ASSIGN, TokenType.DIV_ASSIGN, TokenType.MOD_ASSIGN,
    TokenType.AND_ASSIGN, TokenType.OR_ASSIGN, TokenType.XOR_ASSIGN,
    TokenType.RSHIFT, TokenType.LSHIFT, TokenType.ARROW, TokenType.LOGICAL_AND,
    TokenType.LOGICAL_OR, TokenType.GE, TokenType.LE, TokenType.EQ,
    TokenType.NEQ,

    # 1 character tokens
    TokenType.ADD, TokenType.SUB, TokenType.MUL, TokenType.DIV, TokenType.MOD,
    TokenType.GT, TokenType.LT, TokenType.ASSIGN, TokenType.SEMICOLON,
    TokenType.COMMA, TokenType.OPEN_PAREN, TokenType.CLOSE_PAREN,
    TokenType.OPEN_BRACKET, TokenType.CLOSE_BRACKET, TokenType.OPEN_BRACE,
    TokenType.CLOSE_BRACE, TokenType.COLON, TokenType.DOT,
    TokenType.BITWISE_AND, TokenType.BITWISE_OR, TokenType.BITWISE_XOR,
    TokenType.BITWISE_NOT, TokenType.LOGICAL_NOT, TokenType.TERNARY,
]


"""
A mapping between characters and their corresponding float suffix.
"""
FLOAT_SUFFIXES = {
    "f": FloatSuffix.FLOAT, "F": FloatSuffix.FLOAT,
    "l": FloatSuffix.LONG_DOUBLE, "L": FloatSuffix.LONG_DOUBLE,
}


"""
A list of all integer suffixes.
"""
INT_SUFFIXES = {
    "l": IntSuffix.LONG, "L": IntSuffix.LONG,
    "ll": IntSuffix.LLONG, "LL": IntSuffix.LLONG,
    "u": IntSuffix.UNSIGNED, "U": IntSuffix.UNSIGNED,
    "ul": IntSuffix.UNSIGNED_LONG, "uL": IntSuffix.UNSIGNED_LONG,
    "Ul": IntSuffix.UNSIGNED_LONG, "UL": IntSuffix.UNSIGNED_LONG,
    "lu": IntSuffix.UNSIGNED_LONG, "lU": IntSuffix.UNSIGNED_LONG,
    "Lu": IntSuffix.UNSIGNED_LONG, "LU": IntSuffix.UNSIGNED_LONG,
    "ull": IntSuffix.UNSIGNED_LLONG, "uLL": IntSuffix.UNSIGNED_LLONG,
    "Ull": IntSuffix.UNSIGNED_LLONG, "ULL": IntSuffix.UNSIGNED_LLONG,
    "llu": IntSuffix.UNSIGNED_LLONG, "llU": IntSuffix.UNSIGNED_LLONG,
    "LLu": IntSuffix.UNSIGNED_LLONG, "LLU": IntSuffix.UNSIGNED_LLONG,
}
