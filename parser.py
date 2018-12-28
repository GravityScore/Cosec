
# parser.py
# By Ben Anderson
# December 2018

from __future__ import annotations

from lexer import Tokens, Token
from error import Error, Warning


class Node:
    """
    The base class for any AST node.
    """

    def __init__(self):
        """
        Every AST node has an associated token range, for use when printing
        errors.
        """
        self.range = None


# ******************************************************************************
#     Declarators
# ******************************************************************************


class Declaration(Node):
    """
    A declaration defines a new symbol. It consists of a set of declaration
    specifiers followed by a declarator and an optional initialization
    expression.
    """

    def __init__(self, specifiers, declarator, initializer=None):
        super().__init__()
        self.specifiers = specifiers
        self.declarator = declarator
        self.initializer = initializer


class Type(Node):
    """
    A type consists of a set of declaration specifiers and a declarator. Only
    in certain cases does this declarator have to be abstract.
    """

    def __init__(self, specifiers, declarator=None):
        super().__init__()
        self.specifiers = specifiers
        self.declarator = declarator


class DeclarationSpecifiers(Node):
    """
    There are 4 parts to a declaration specifier:
      * Storage class (typedef, extern, static, auto, or register)
      * Type specifiers (built in type, struct, union, enum, or typedef)
      * Any type qualifiers (const, restrict, volatile)
      * Any function specifiers (inline)
    """

    def __init__(self):
        super().__init__()
        self.storage_class = None
        self.type_specifier = None
        self.type_qualifiers = set()
        self.function_specifiers = set()


class StorageClass:
    """
    All storage classes that can occur in a set of declaration specifiers. These
    are only legal in a root declaration at the top level of a source code file.
    """
    TYPEDEF = Token.TYPEDEF
    EXTERN = Token.EXTERN
    STATIC = Token.STATIC
    AUTO = Token.AUTO
    REGISTER = Token.REGISTER


class TypeSpecifier:
    """
    All type specifiers that can occur in a set of declaration specifiers.
    """
    VOID = "void"

    # Signed types
    CHAR = "char"      # Signed 8 bit integer
    SHORT = "short"    # Signed 16 bit integer
    INT = "int"        # Signed 32 bit integer
    LONG = "long"      # Signed 32 bit integer
    LLONG = "llong"    # Signed 64 bit integer
    FLOAT = "float"    # 32 bit floating point number
    DOUBLE = "double"  # 64 bit floating point number

    # Unsigned types
    UCHAR = "uchar"    # Unsigned 8 bit integer
    USHORT = "ushort"  # Unsigned 16 bit integer
    UINT = "uint"      # Unsigned 32 bit integer
    ULONG = "ulong"    # Unsigned 32 bit integer
    ULLONG = "ullong"  # Unsigned 64 bit integer


class TypeQualifier:
    """
    All type qualifiers that can occur in a set of declaration specifiers.
    """
    CONST = Token.CONST
    RESTRICT = Token.RESTRICT
    VOLATILE = Token.VOLTATILE


class FunctionSpecifier:
    """
    All function specifiers that can occur in a set of declaration specifiers.
    These are only legal in function declarations and definitions at the top
    level of a source code file.
    """
    INLINE = Token.INLINE


class Declarator(Node):
    """
    A declarator specifies the name of a variable being declared and some
    additional information about its type.
    """

    def __init__(self):
        super().__init__()
        self.parts = []   # First element is inner-most part closest to name
        self.name = None  # None for abstract declarators


class DeclaratorArrayPart(Node):
    """
    Information about an array part of a declarator. This includes an expression
    specifying the size of the array, as well information for implicit
    conversion to a pointer when used as a function argument.
    """

    def __init__(self):
        super().__init__()
        self.size = None
        self.is_static = False
        self.is_vla = False
        self.type_qualifiers = set()


class DeclaratorFunctionPart(Node):
    """
    Information about a function part of a declarator. This includes its list
    of arguments and their names and types.
    """

    def __init__(self):
        super().__init__()
        self.args = []


class DeclaratorPointerPart(Node):
    """
    Information about a pointer part of a declarator. This includes any type
    qualifiers specified alongside the pointer.
    """

    def __init__(self):
        super().__init__()
        self.type_qualifiers = set()


# ******************************************************************************
#     Expressions
# ******************************************************************************


class Expression(Node):
    """
    This is the base class for an expression tree node, which forms one part
    of an expression.
    """
    pass


class TernaryExpression(Expression):
    """
    A ternary operation consists of a condition and a true and false expression.
    """

    def __init__(self, condition: Expression, true: Expression,
                 false: Expression):
        super().__init__()
        self.condition = condition
        self.true = true
        self.false = false


class BinaryExpression(Expression):
    """
    A binary operation consists of an operator and two operands.
    """

    def __init__(self, operator: BinaryOperator, left: Expression,
                 right: Expression):
        super().__init__()
        self.operator = operator
        self.left = left
        self.right = right


class CastExpression(Expression):
    """
    A cast expression explicitly changes the type of its operand.
    """

    def __init__(self, type: Type, operand: Expression):
        super().__init__()
        self.type = type
        self.operand = operand


class SizeofExpression(Expression):
    """
    A sizeof expression measures the size of a type (consisting of a set of
    declaration specifiers and an abstract declarator).

    There are two uses for sizeof, one measures the size of an expression, and
    the other measures the size of a fixed type. The first case is handled as a
    unary operator. The second case is handled by this special expression tree
    node.
    """

    def __init__(self, type: Type):
        super().__init__()
        self.type = type


class UnaryExpression(Expression):
    """
    A unary operation consists of an operator and a single operand.
    """

    def __init__(self, operator: UnaryOperator, operand: Expression):
        super().__init__()
        self.operator = operator
        self.operand = operand


class PostfixExpression(Expression):
    """
    There are 2 uses of the increment and decrement operators ('++' and '--'),
    as a prefix or a postfix operation ('++a' and 'a++' respectively). Use as a
    prefix operator is represented as a unary operation. Use as a postfix
    operator is represented using this special expression tree node.
    """

    def __init__(self, operator: UnaryOperator, operand: Expression):
        super().__init__()
        self.operator = operator
        self.operand = operand


class ArrayAccessExpression(Expression):
    """
    An array access consists of the array we're accessing, and the index to
    access.
    """

    def __init__(self, array: Expression, index: Expression):
        super().__init__()
        self.array = array
        self.index = index


class FunctionCallExpression(Expression):
    """
    A function call consists of the function we're calling, and a list of
    arguments.
    """

    def __init__(self, function: Expression, args: list):
        super().__init__()
        self.function = function
        self.args = args


class FieldAccessExpression(Expression):
    """
    A field access on a struct consists of the struct to access, and the name
    of the field (an identifier).
    """

    def __init__(self, struct: Expression, name: Token):
        super().__init__()
        self.struct = struct
        self.name = name


class SymbolExpression(Expression):
    """
    A symbol consists of an identifier that references some previously declared
    object.
    """

    def __init__(self, name: Token):
        super().__init__()
        self.name = name


class ConstantExpression(Expression):
    """
    A constant can be an integer, float, character, or string literal.
    """

    def __init__(self, value: Token):
        super().__init__()
        self.value = value


class Precedence:
    """
    A list of all possible precedences.
    """
    NONE = 0
    ASSIGN = 1
    TERNARY = 2
    LOGICAL_OR = 3
    LOGICAL_AND = 4
    BITWISE_OR = 5
    BITWISE_XOR = 6
    BITWISE_AND = 7
    EQ = 8
    ORD = 9
    SHIFT = 10
    ADD = 11
    MUL = 12


class BinaryOperator:
    """
    A list of all binary operators, and their corresponding tokens.
    """
    ASSIGN = Token.ASSIGN
    ADD_ASSIGN = Token.ADD_ASSIGN
    SUB_ASSIGN = Token.SUB_ASSIGN
    MUL_ASSIGN = Token.MUL_ASSIGN
    DIV_ASSIGN = Token.DIV_ASSIGN
    MOD_ASSIGN = Token.MOD_ASSIGN
    LSHIFT_ASSIGN = Token.LSHIFT_ASSIGN
    RSHIFT_ASSIGN = Token.RSHIFT_ASSIGN
    AND_ASSIGN = Token.AND_ASSIGN
    OR_ASSIGN = Token.OR_ASSIGN
    XOR_ASSIGN = Token.XOR_ASSIGN
    TERNARY = Token.TERNARY
    LOGICAL_OR = Token.LOGICAL_OR
    LOGICAL_AND = Token.LOGICAL_AND
    BITWISE_OR = Token.BITWISE_OR
    BITWISE_XOR = Token.BITWISE_XOR
    BITWISE_AND = Token.BITWISE_AND
    EQ = Token.EQ
    NEQ = Token.NEQ
    LT = Token.LT
    GT = Token.GT
    LE = Token.LE
    GE = Token.GE
    LSHIFT = Token.LSHIFT
    RSHIFT = Token.RSHIFT
    ADD = Token.ADD
    SUB = Token.SUB
    MUL = Token.MUL
    DIV = Token.DIV
    MOD = Token.MOD


class UnaryOperator:
    """
    A list of all possible unary operators, and their corresponding tokens.
    """
    DEREF = Token.MUL
    ADDR = Token.BITWISE_AND
    ADD = Token.ADD
    NEG = Token.SUB
    BITWISE_NOT = Token.BITWISE_NOT
    LOGICAL_NOT = Token.LOGICAL_NOT
    INC = Token.INC
    DEC = Token.DEC
    SIZEOF = Token.SIZEOF


# ******************************************************************************
#     Parser
# ******************************************************************************


class Parser:
    """
    A parser generates an AST from a list of tokens produced by the lexer.
    """

    def __init__(self, tokens: Tokens):
        """
        Create a new AST parser from a tokens list.

        :param tokens: The tokens list to construct an AST from.
        """
        self.t = tokens

    def parse_declarations(self) -> list:
        """
        Parse a list of declarations. The relevant grammar is:

          declaration
            : declaration_specifiers ';'
            | declaration_specifiers init_declarator_list ';'

          init_declarator_list
            : init_declarator
            | init_declarator_list ',' init_declarator

          init_declarator
            : declarator '=' initializer
            | declarator

        :return: An array of declarations.
        """
        # Parse a set of declaration specifiers
        specifiers = self.parse_declaration_specifiers()

        # Parse a list of declarators and initializers
        declarations = []
        while self.t.cur().type != Token.SEMICOLON:
            start = self.t.cur()

            # Parse a declarator
            declarator = self.parse_declarator()

            # Check for an optional initializer
            initializer = None
            if self.t.cur().type == Token.ASSIGN:
                self.t.next()
                # initializer = self.parse_expression()

            # Add a declaration
            declaration = Declaration(specifiers, declarator, initializer)
            declaration.range = start.combine(self.t.prev())
            declarations.append(declaration)

            # Check for a comma
            if self.t.cur().type == Token.COMMA:
                self.t.next()
            else:
                break

        # Expect a semicolon
        self.t.expect(Token.SEMICOLON)
        self.t.next()

        # Emit a warning if we don't have any declarators (the statement is
        # useless)
        if len(declarations) == 0:
            err = Warning("useless declaration", specifiers.range)
            err.print()
        return declarations

    def parse_type(self) -> Type:
        """
        Parse a type name, consisting of a set of declaration specifiers
        followed by an optional abstract declarator.

        :return: The parsed type.
        """
        # Parse a set of declaration specifiers
        specifiers = self.parse_declaration_specifiers()
        type = Type(specifiers)

        # Parse an optional declarator
        if self.t.cur().type != Token.CLOSE_PAREN and \
                self.t.cur().type != Token.COMMA:
            type.declarator = self.parse_declarator()
        return type

    def parse_declaration_specifiers(self) -> DeclarationSpecifiers:
        """
        Parse a set of declaration specifiers. The relevant grammar is:

            declaration_specifiers
                : storage_class_specifier declaration_specifiers
                | storage_class_specifier
                | type_specifier declaration_specifiers
                | type_specifier
                | type_qualifier declaration_specifiers
                | type_qualifier
                | function_specifier declaration_specifiers
                | function_specifier

            storage_class_specifier: TYPEDEF | EXTERN | STATIC | AUTO | REGISTER

            type_specifier: VOID | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE
                | SIGNED | UNSIGNED | BOOL | COMPLEX | IMAGINARY
                | struct_or_union_specifier | enum_specifier | TYPEDEF_NAME

            type_qualifier: CONST | RESTRICT | VOLATILE

            function_specifier: INLINE

        Note THREAD_LOCAL, AUTO, REGISTER, BOOL, COMPLEX, IMAGINARY, RESTRICT,
        and VOLATILE are unsupported. AUTO, REGISTER, RESTRICT, and VOLATILE are
        all no-ops, and THREAD_LOCAL, BOOL, COMPLEX, IMAGINARY all trigger
        compiler errors.

        :return: The set of declaration specifiers.
        """
        start = self.t.cur()
        specifiers = DeclarationSpecifiers()

        # Keep track of which type names we've seen
        type_names = []

        # Keep parsing tokens until we reach one that isn't valid
        while self.t.cur().type != Token.EOF:
            type = self.t.cur().type
            if type in STORAGE_CLASSES:
                # Check we haven't already got a storage class
                if specifiers.storage_class is not None:
                    raise Error(f"can't have two storage classes", self.t.cur())
                specifiers.storage_class = type
            elif type in BUILT_IN_TYPE_NAMES:
                # Collect the type name
                type_names.append(type)

                # Try match the accumulated type names to a built in type
                type_specifier = None
                for (candidate, specifications) in BUILT_IN_TYPES.items():
                    if sorted(type_names) in specifications:
                        type_specifier = candidate
                        break

                # Check we could find a matching type specifier
                if type_specifier is None:
                    raise Error("unknown type", self.t.cur())
                specifiers.type_specifier = type_specifier
            elif type in TYPE_QUALIFIERS:
                # Add another type qualifier
                specifiers.type_qualifiers.add(self.t.cur().type)
            elif type in FUNCTION_SPECIFIERS:
                # Add another function specifier
                specifiers.function_specifiers.add(type)
            else:
                break
            self.t.next()

        # Check we actually parsed anything at all
        if specifiers.type_specifier is None:
            raise Error("expected type", self.t.cur())

        specifiers.range = start.combine(self.t.prev())
        return specifiers

    def parse_declarator(self) -> Declarator:
        """
        Parse a declarator. A great article on declarator parsing describing the
        approach used here can be found at:

          http://blog.robertelder.org/building-a-c-compiler-type-system-the-
          formidable-declarator/

        The relevant grammar is:

          declarator: pointer direct_declarator | direct_declarator

          direct_declarator
            : IDENTIFIER
            | '(' declarator ')'
            | direct_declarator '[' ']'
            | direct_declarator '[' type_qualifiers ']'
            | direct_declarator '[' '*' ']'
            | direct_declarator '[' type_qualifiers '*' ']'
            | direct_declarator '[' expression ']'
            | direct_declarator '[' type_qualifiers expression ']'
            | direct_declarator '[' STATIC expression ']'
            | direct_declarator '[' STATIC type_qualifiers expression ']'
            | direct_declarator '[' type_qualifiers STATIC expression ']'
            | direct_declarator '(' parameter_types ')'
            | direct_declarator '(' identifiers ')'      NOT SUPPORTED
            | direct_declarator '(' ')'

          pointer
            : '*' type_qualifiers pointer
            | '*' type_qualifiers
            | '*' pointer
            | '*'

        This function parses both named and abstract declarators. Note the old
        K&R style function definitions are not supported, as indicated by the
        'NOT SUPPORTED' tag next to the relevant line in the above grammar.
        """
        start = self.t.cur()

        # Parse a declarator recursively
        declarator = Declarator()
        self.parse_declarator_recursive(declarator)
        declarator.range = start.combine(self.t.cur())

        # Check we actually parsed something
        if len(declarator.parts) == 0 and declarator.name is None:
            raise Error("expected declarator", self.t.cur())
        return declarator

    def parse_declarator_recursive(self, declarator: Declarator):
        """
        Parse a single part of a declarator (either an array, pointer, or
        function part).
        """
        # Parse a list of pointer parts
        pointers = []
        while self.t.cur().type == Token.MUL:
            pointer_part = self.parse_declarator_pointer_part()
            pointers.append(pointer_part)

        # Either an identifier or an open parenthesis can follow the pointer
        # parts
        if self.t.cur().type == Token.OPEN_PAREN:
            # Parse another declarator part recursively
            self.t.next()
            self.parse_declarator_recursive(declarator)

            # Expect a closing parenthesis
            self.t.expect(Token.CLOSE_PAREN)
            self.t.next()
        elif self.t.cur().type == Token.IDENT:
            # Found the symbol name that the declarator is declaring
            declarator.name = self.t.cur()
            self.t.next()

        # Parse a list of postfix array or function parts
        while True:
            if self.t.cur().type == Token.OPEN_PAREN:
                function_part = self.parse_declarator_function_part()
                declarator.parts.append(function_part)
            elif self.t.cur().type == Token.OPEN_BRACKET:
                array_part = self.parse_declarator_array_part()
                declarator.parts.append(array_part)
            else:
                break

        # Append the pointer parts we parsed earlier, in reverse order
        pointers.reverse()
        declarator.parts += pointers

    def parse_declarator_pointer_part(self) -> DeclaratorPointerPart:
        """
        Parse a pointer declarator part from a tokens list. The relevant grammar
        is:

          pointer
            : '*' type_qualifiers pointer
            | '*' type_qualifiers
            | '*' pointer
            | '*'

          type_qualifiers
            : type_qualifier
            | type_qualifiers type_qualifier

        You can find information on using 'static' and type qualifiers in array
        declarators here:

          https://stackoverflow.com/questions/17559631/what-are-those-strange-
          array-sizes-and-static-in-c99

        :return: A pointer declarator part.
        """
        # Expect an asterisk
        self.t.expect(Token.MUL)
        self.t.next()

        # Parse a list of type qualifiers
        pointer = DeclaratorPointerPart()
        while self.t.cur().type in TYPE_QUALIFIERS:
            pointer.type_qualifiers.add(self.t.cur().type)
            self.t.next()
        return pointer

    def parse_declarator_function_part(self) -> DeclaratorFunctionPart:
        """
        Parse a function declarator part from a tokens list. The relevant
        grammar is:

          direct_declarator
            : ...
            | direct_declarator '(' parameter_types ')'
            | direct_declarator '(' identifiers ')'      NOT SUPPORTED
            | direct_declarator '(' ')'

          parameter_types
            : parameter_list ',' '...'
            | parameter_list

          parameter_list
            : parameter_declaration
            | parameter_list ',' parameter_declaration

          parameter_declaration
            : declaration_specifiers declarator
            | declaration_specifiers abstract_declarator
            | declaration_specifiers

        Note we don't support the old K&R style function definitions, as
        indicated by the 'NOT SUPPORTED' tag next to the relevant line in the
        above grammar.

        :return: A function declarator part.
        """
        # Expect an opening parenthesis
        self.t.expect(Token.OPEN_PAREN)
        self.t.next()

        # No arguments if the only thing in the parentheses is 'void'
        if self.t.cur().type == Token.VOID and \
                self.t.peek(1).type == Token.CLOSE_PAREN:
            self.t.next()  # Skip 'void' and stop on the close parenthesis

        # Parse a list of argument declarations
        function = DeclaratorFunctionPart()
        while self.t.cur().type != Token.CLOSE_PAREN:
            arg = self.parse_type()
            function.args.append(arg)

            # Parse a comma
            if self.t.cur().type == Token.COMMA:
                self.t.next()
            else:
                break

        # Expect a close parenthesis
        self.t.expect(Token.CLOSE_PAREN)
        self.t.next()
        return function

    def parse_declarator_array_part(self) -> DeclaratorArrayPart:
        """
        Parses an array part of a declarator. The relevant grammar is:

            direct_declarator
                : ...
                | direct_declarator '[' ']'
                | direct_declarator '[' type_qualifiers ']'
                | direct_declarator '[' '*' ']'
                | direct_declarator '[' type_qualifiers '*' ']'
                | direct_declarator '[' expression ']'
                | direct_declarator '[' type_qualifiers expression ']'
                | direct_declarator '[' STATIC expression ']'
                | direct_declarator '[' STATIC type_qualifiers expression ']'
                | direct_declarator '[' type_qualifiers STATIC expression ']'
                | ...

            type_qualifiers
                : type_qualifier
                | type_qualifiers type_qualifier

        :return: An array declarator part.
        """
        array = DeclaratorArrayPart()
        start = self.t.cur()

        # Expect an opening bracket
        self.t.expect(Token.OPEN_BRACKET)
        self.t.next()

        # Check for static
        if self.t.cur().type == Token.STATIC:
            array.is_static = True
            self.t.next()

        # Check for type qualifiers
        while self.t.cur().type in TYPE_QUALIFIERS:
            array.type_qualifiers.add(self.t.cur().type)
            self.t.next()

        # Check for static again
        if self.t.cur().type == Token.STATIC:
            array.is_static = True
            self.t.next()

        # Check for a variable length array, or parse an expression
        if self.t.cur().type == Token.MUL and \
                self.t.peek(1).type == Token.CLOSE_BRACKET:
            array.is_vla = True
            self.t.next()  # The close bracket is consumed below
        elif self.t.cur().type != Token.CLOSE_BRACKET:
            # array.size = self.parse_expression()
            pass

        # Expect a closing bracket
        self.t.expect(Token.CLOSE_BRACKET)
        self.t.next()
        array.range = start.combine(self.t.prev())
        return array




"""
A list of all storage class specifiers.
"""
STORAGE_CLASSES = {
    StorageClass.TYPEDEF, StorageClass.EXTERN, StorageClass.STATIC,
    StorageClass.AUTO, StorageClass.REGISTER,
}


"""
A list of all built-in type names.
"""
BUILT_IN_TYPE_NAMES = {
    Token.VOID, Token.CHAR, Token.SHORT, Token.INT,
    Token.LONG, Token.SIGNED, Token.UNSIGNED, Token.FLOAT,
    Token.DOUBLE,
}


"""
A mapping between a built-in type specifier, and a corresponding list of
tokens (sorted in alphabetical order) that can be used to specify that built-in
type.
"""
BUILT_IN_TYPES = {
    TypeSpecifier.VOID: [[Token.VOID]],
    TypeSpecifier.CHAR: [[Token.CHAR], [Token.CHAR, Token.SIGNED]],
    TypeSpecifier.UCHAR: [[Token.CHAR, Token.UNSIGNED]],
    TypeSpecifier.SHORT: [[Token.SHORT], [Token.SHORT, Token.SIGNED],
                          [Token.INT, Token.SHORT],
                          [Token.INT, Token.SHORT, Token.SIGNED]],
    TypeSpecifier.USHORT: [[Token.SHORT, Token.UNSIGNED],
                           [Token.INT, Token.SHORT, Token.UNSIGNED]],
    TypeSpecifier.INT: [[Token.INT], [Token.SIGNED], [Token.INT, Token.SIGNED]],
    TypeSpecifier.UINT: [[Token.UNSIGNED], [Token.INT, Token.UNSIGNED]],
    TypeSpecifier.LONG: [[Token.LONG], [Token.LONG, Token.SIGNED],
                         [Token.INT, Token.LONG],
                         [Token.INT, Token.LONG, Token.SIGNED]],
    TypeSpecifier.ULONG: [[Token.LONG, Token.UNSIGNED],
                          [Token.INT, Token.LONG, Token.UNSIGNED]],
    TypeSpecifier.LLONG: [[Token.LONG, Token.LONG],
                          [Token.LONG, Token.LONG, Token.SIGNED],
                          [Token.INT, Token.LONG, Token.LONG],
                          [Token.INT, Token.LONG, Token.LONG, Token.SIGNED]],
    TypeSpecifier.ULLONG: [[Token.LONG, Token.LONG, Token.UNSIGNED],
                           [Token.INT, Token.LONG, Token.LONG, Token.UNSIGNED]],
    TypeSpecifier.FLOAT: [[Token.FLOAT]],
    TypeSpecifier.DOUBLE: [[Token.DOUBLE], [Token.DOUBLE, Token.LONG]],
}


"""
A list of all type qualifiers.
"""
TYPE_QUALIFIERS = {
    TypeQualifier.CONST, TypeQualifier.RESTRICT, TypeQualifier.VOLATILE,
}


"""
A list of all function specifiers.
"""
FUNCTION_SPECIFIERS = {
    FunctionSpecifier.INLINE,
}


"""
Mapping between binary operators and their corresponding precedence.
"""
PRECEDENCES = {
    BinaryOperator.ASSIGN: Precedence.ASSIGN,
    BinaryOperator.ADD_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.SUB_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.MUL_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.DIV_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.MOD_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.LSHIFT_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.RSHIFT_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.AND_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.OR_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.XOR_ASSIGN: Precedence.ASSIGN,
    BinaryOperator.TERNARY: Precedence.TERNARY,
    BinaryOperator.LOGICAL_OR: Precedence.LOGICAL_OR,
    BinaryOperator.LOGICAL_AND: Precedence.LOGICAL_AND,
    BinaryOperator.BITWISE_OR: Precedence.BITWISE_OR,
    BinaryOperator.BITWISE_XOR: Precedence.BITWISE_XOR,
    BinaryOperator.BITWISE_AND: Precedence.BITWISE_AND,
    BinaryOperator.EQ: Precedence.EQ,
    BinaryOperator.NEQ: Precedence.EQ,
    BinaryOperator.LT: Precedence.ORD,
    BinaryOperator.GT: Precedence.ORD,
    BinaryOperator.LE: Precedence.ORD,
    BinaryOperator.GE: Precedence.ORD,
    BinaryOperator.LSHIFT: Precedence.SHIFT,
    BinaryOperator.RSHIFT: Precedence.SHIFT,
    BinaryOperator.ADD: Precedence.ADD,
    BinaryOperator.SUB: Precedence.ADD,
    BinaryOperator.MUL: Precedence.MUL,
    BinaryOperator.DIV: Precedence.MUL,
    BinaryOperator.MOD: Precedence.MUL,
}


"""
A list of all binary operators.
"""
BINARY_OPERATORS = set(PRECEDENCES.keys())


"""
A list of all unary operators.
"""
UNARY_OPERATORS = {
    UnaryOperator.DEREF, UnaryOperator.ADDR, UnaryOperator.ADD,
    UnaryOperator.NEG, UnaryOperator.BITWISE_NOT, UnaryOperator.LOGICAL_NOT,
    UnaryOperator.INC, UnaryOperator.DEC, UnaryOperator.SIZEOF,
}
