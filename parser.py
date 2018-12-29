
# parser.py
# By Ben Anderson
# December 2018

from __future__ import annotations
from typing import Optional
from enum import Enum

from lexer import Tokens, Token, Tk
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


class TypeName(Node):
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


class StorageClass(Enum):
    """
    All storage classes that can occur in a set of declaration specifiers. These
    are only legal in a root declaration at the top level of a source code file.
    """
    TYPEDEF = Tk.TYPEDEF
    EXTERN = Tk.EXTERN
    STATIC = Tk.STATIC
    AUTO = Tk.AUTO
    REGISTER = Tk.REGISTER


class TypeSpecifier(Enum):
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


class TypeQualifier(Enum):
    """
    All type qualifiers that can occur in a set of declaration specifiers.
    """
    CONST = Tk.CONST
    RESTRICT = Tk.RESTRICT
    VOLATILE = Tk.VOLATILE


class FunctionSpecifier(Enum):
    """
    All function specifiers that can occur in a set of declaration specifiers.
    These are only legal in function declarations and definitions at the top
    level of a source code file.
    """
    INLINE = Tk.INLINE


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


class ExpressionList(Expression):
    """
    An expression can contain multiple subexpressions (called roots) separated
    by commas. This object keeps a list of these subexpressions.
    """

    def __init__(self, roots):
        super().__init__()
        self.roots = roots


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

    def __init__(self, type: TypeName, operand: Expression):
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

    def __init__(self, type: TypeName):
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


class Precedence(Enum):
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


class BinaryOperator(Enum):
    """
    A list of all binary operators, and their corresponding tokens.
    """
    ASSIGN = Tk.ASSIGN
    ADD_ASSIGN = Tk.ADD_ASSIGN
    SUB_ASSIGN = Tk.SUB_ASSIGN
    MUL_ASSIGN = Tk.MUL_ASSIGN
    DIV_ASSIGN = Tk.DIV_ASSIGN
    MOD_ASSIGN = Tk.MOD_ASSIGN
    LSHIFT_ASSIGN = Tk.LSHIFT_ASSIGN
    RSHIFT_ASSIGN = Tk.RSHIFT_ASSIGN
    AND_ASSIGN = Tk.AND_ASSIGN
    OR_ASSIGN = Tk.OR_ASSIGN
    XOR_ASSIGN = Tk.XOR_ASSIGN
    TERNARY = Tk.TERNARY
    LOGICAL_OR = Tk.LOGICAL_OR
    LOGICAL_AND = Tk.LOGICAL_AND
    BITWISE_OR = Tk.BITWISE_OR
    BITWISE_XOR = Tk.BITWISE_XOR
    BITWISE_AND = Tk.BITWISE_AND
    EQ = Tk.EQ
    NEQ = Tk.NEQ
    LT = Tk.LT
    GT = Tk.GT
    LE = Tk.LE
    GE = Tk.GE
    LSHIFT = Tk.LSHIFT
    RSHIFT = Tk.RSHIFT
    ADD = Tk.ADD
    SUB = Tk.SUB
    MUL = Tk.MUL
    DIV = Tk.DIV
    MOD = Tk.MOD


class UnaryOperator(Enum):
    """
    A list of all possible unary operators, and their corresponding tokens.
    """
    DEREF = Tk.MUL
    ADDR = Tk.BITWISE_AND
    ADD = Tk.ADD
    NEG = Tk.SUB
    BITWISE_NOT = Tk.BITWISE_NOT
    LOGICAL_NOT = Tk.LOGICAL_NOT
    INC = Tk.INC
    DEC = Tk.DEC
    SIZEOF = Tk.SIZEOF


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

    # **************************************************************************
    #     Declaration Parsing
    # **************************************************************************

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
        while self.t.cur().type != Tk.EOF and \
                self.t.cur().type != Tk.SEMICOLON:
            start = self.t.cur()

            # Parse a declarator
            declarator = self.parse_declarator()

            # Check for an optional initializer
            initializer = None
            if self.t.cur().type == Tk.ASSIGN:
                self.t.next()
                initializer = self.parse_expression()

            # Add a declaration
            declaration = Declaration(specifiers, declarator, initializer)
            declaration.range = start.combine(self.t.prev())
            declarations.append(declaration)

            # Check for a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()

        # Emit a warning if we don't have any declarators (the statement is
        # useless)
        if len(declarations) == 0:
            err = Warning("useless declaration", specifiers.range)
            err.print()
        return declarations

    def parse_type(self) -> TypeName:
        """
        Parse a type name, consisting of a set of declaration specifiers
        followed by an optional abstract declarator.

        :return: The parsed type.
        """
        # Parse a set of declaration specifiers
        specifiers = self.parse_declaration_specifiers()
        type = TypeName(specifiers)

        # Parse an optional declarator
        if self.t.cur().type != Tk.CLOSE_PAREN and \
                self.t.cur().type != Tk.COMMA:
            type.declarator = self.parse_declarator()
        return type

    def try_parse_type(self) -> Optional[TypeName]:
        """
        Try parse a type surrounded in parentheses. If no valid type exists,
        then return None.

        :return: A type, or None.
        """
        saved = self.t.save()

        # Check for an opening parenthesis
        if self.t.cur().type != Tk.OPEN_PAREN:
            return None
        self.t.next()

        # Try parsing a type
        try:
            type = self.parse_type()
        except Error:
            type = None
            self.t.restore(saved)

        # If we found a type, expect a closing parenthesis
        if type is not None:
            self.t.expect(Tk.CLOSE_PAREN)
            self.t.next()
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
        while self.t.cur().type != Tk.EOF:
            type = self.t.cur().type
            if type in STORAGE_CLASSES:
                # Check we haven't already got a storage class
                if specifiers.storage_class is not None:
                    raise Error(f"can't have two storage classes", self.t.cur())
                specifiers.storage_class = StorageClass(type)
            elif type in BUILT_IN_TYPE_TOKENS:
                # Collect the type name
                type_names.append(type)

                # Try match the accumulated type names to a built in type
                type_specifier = None
                for (candidate, options) in BUILT_IN_TYPES.items():
                    # Convert all the tokens to strings
                    if sorted(type_names, key=lambda x: x.value) in options:
                        type_specifier = candidate
                        break

                # Check we could find a matching type specifier
                if type_specifier is None:
                    raise Error("unknown type", self.t.cur())
                specifiers.type_specifier = type_specifier
            elif type in TYPE_QUALIFIERS:
                # Add another type qualifier
                qualifier = TypeQualifier(type)
                specifiers.type_qualifiers.add(qualifier)
            elif type in FUNCTION_SPECIFIERS:
                # Add another function specifier
                specifier = FunctionSpecifier(type)
                specifiers.function_specifiers.add(specifier)
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
        while self.t.cur().type == Tk.MUL:
            pointer_part = self.parse_declarator_pointer_part()
            pointers.append(pointer_part)

        # Either an identifier or an open parenthesis can follow the pointer
        # parts
        if self.t.cur().type == Tk.OPEN_PAREN:
            # Parse another declarator part recursively
            self.t.next()
            self.parse_declarator_recursive(declarator)

            # Expect a closing parenthesis
            self.t.expect(Tk.CLOSE_PAREN)
            self.t.next()
        elif self.t.cur().type == Tk.IDENT:
            # Found the symbol name that the declarator is declaring
            declarator.name = self.t.cur()
            self.t.next()

        # Parse a list of postfix array or function parts
        while True:
            if self.t.cur().type == Tk.OPEN_PAREN:
                function_part = self.parse_declarator_function_part()
                declarator.parts.append(function_part)
            elif self.t.cur().type == Tk.OPEN_BRACKET:
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
        self.t.expect(Tk.MUL)
        self.t.next()

        # Parse a list of type qualifiers
        pointer = DeclaratorPointerPart()
        while self.t.cur().type in TYPE_QUALIFIERS:
            qualifier = TypeQualifier(self.t.cur().type)
            pointer.type_qualifiers.add(qualifier)
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
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()

        # No arguments if the only thing in the parentheses is 'void'
        if self.t.cur().type == Tk.VOID and \
                self.t.peek(1).type == Tk.CLOSE_PAREN:
            self.t.next()  # Skip 'void' and stop on the close parenthesis

        # Parse a list of argument declarations
        function = DeclaratorFunctionPart()
        while self.t.cur().type != Tk.EOF and \
                self.t.cur().type != Tk.CLOSE_PAREN:
            arg = self.parse_type()
            function.args.append(arg)

            # Parse a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a close parenthesis
        self.t.expect(Tk.CLOSE_PAREN)
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
        self.t.expect(Tk.OPEN_BRACKET)
        self.t.next()

        # Check for static
        if self.t.cur().type == Tk.STATIC:
            array.is_static = True
            self.t.next()

        # Check for type qualifiers
        while self.t.cur().type in TYPE_QUALIFIERS:
            qualifier = TypeQualifier(self.t.cur().type)
            array.type_qualifiers.add(qualifier)
            self.t.next()

        # Check for static again
        if self.t.cur().type == Tk.STATIC:
            array.is_static = True
            self.t.next()

        # Check for a variable length array, or parse an expression
        if self.t.cur().type == Tk.MUL and \
                self.t.peek(1).type == Tk.CLOSE_BRACKET:
            array.is_vla = True
            self.t.next()  # The close bracket is consumed below
        elif self.t.cur().type != Tk.CLOSE_BRACKET:
            array.size = self.parse_expression()
            pass

        # Expect a closing bracket
        self.t.expect(Tk.CLOSE_BRACKET)
        self.t.next()
        array.range = start.combine(self.t.prev())
        return array

    # **************************************************************************
    #     Expression Parsing
    # **************************************************************************

    def parse_expression(self) -> Expression:
        """
        Parse an expression from a list of tokens. The relevant grammar is:

          expression
            : assignment_expression
            | expression ',' assignment_expression

        :return: An expression list.
        """
        # Parse expressions separated by commas
        expressions = []
        while True:
            root = self.parse_expression_root()
            expressions.append(root)

            # Check for a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Return the expression directly if there's only one of them
        if len(expressions) == 1:
            return expressions[0]
        else:
            return ExpressionList(expressions)

    def parse_expression_root(self) -> Expression:
        """
        Parse a single expression root (an 'assignment_expression' in the
        grammar).

        :return: The root node of an expression tree.
        """
        return self.parse_expression_recursive(Precedence.NONE)

    def parse_expression_recursive(self, min_prec: Precedence) -> Expression:
        """
        Parse binary operations as long as the operator has a precedence greater
        than the minimum.

        :param min_prec: The minimum precedence for binary operators.
        :return:               An expression tree.
        """
        # Parse the left operand
        left = self.parse_cast_expression()

        # Keep parsing binary operators while their precedence is greater than
        # the minimum
        while self.t.cur().type != Tk.EOF:
            # Check we've got a binary operator
            try:
                operator = BinaryOperator(self.t.cur().type)
            except ValueError:
                break

            # Check the precedence is greater than the minimum
            precedence = PRECEDENCES[operator]
            if precedence.value <= min_prec.value:
                break
            self.t.next()

            # Parse the right operand
            right = self.parse_expression_recursive(precedence)

            # Check for a ternary operator
            if operator == BinaryOperator.TERNARY:
                # Expect a colon
                self.t.expect(Tk.COLON)
                self.t.next()

                # Parse another expression
                after = self.parse_expression_recursive(precedence)

                # Construct a ternary operation
                left = TernaryExpression(left, right, after)
            else:
                # Construct a binary operation
                left = BinaryExpression(operator, left, right)
        return left

    def parse_cast_expression(self) -> Expression:
        """
        Parse an optional cast of an expression to another type. The relevant
        grammar is:

          cast_expression
            : unary_expression
            | '(' type_name ')' cast_expression

        :return: A cast expression.
        """
        # Check for an open parenthesis
        if self.t.cur().type != Tk.OPEN_PAREN:
            return self.parse_sizeof_expression()

        # An open parenthesis here could indicate either a cast or a
        # subexpression; try parsing a type name
        type = self.try_parse_type()

        if type is None:
            # Failed to parse a type name, try a subexpression
            return self.parse_sizeof_expression()
        else:
            # Check the declarator is abstract
            if type.declarator is not None and type.declarator.name is not None:
                desc = "expected abstract declarator"
                raise Error(desc, type.declarator.name)

            # Parse the unary expression that follows
            operand = self.parse_sizeof_expression()
            return CastExpression(type, operand)

    def parse_sizeof_expression(self) -> Expression:
        """
        Parse a sizeof expression. There are 2 uses of sizeof, either to measure
        the size of an expression, or the size of an explicit type name. The
        first sort is represented as a unary operator on an expression, and the
        second as this special expression tree node.

        :return: A sizeof expression.
        """
        # Check we've got a sizeof operator
        if self.t.cur().type != Tk.SIZEOF:
            return self.parse_unary_expression()
        self.t.next()

        # Try parsing a type in parentheses
        if self.t.cur().type == Tk.OPEN_PAREN:
            type = self.try_parse_type()
            if type is not None:
                return SizeofExpression(type)

        # Otherwise, expect an expression (don't allow casting of the operand)
        operand = self.parse_unary_expression()
        return UnaryExpression(UnaryOperator.SIZEOF, operand)

    def parse_unary_expression(self) -> Expression:
        """
        Parse a unary or prefix operation. The relevant grammar is:

          unary_expression
            : postfix_expression
            | INC_OP unary_expression
            | DEC_OP unary_expression
            | unary_operator cast_expression
            | SIZEOF unary_expression
            | SIZEOF '(' type_name ')'

          unary_operator: '&' | '*' | '+' | '-' | '~' | '!'

        :return:  A unary expression.
        """
        # Check for a unary operator
        try:
            operator = UnaryOperator(self.t.cur().type)
        except ValueError:
            return self.parse_postfix_expression()
        self.t.next()

        # Parse the operand
        if operator == UnaryOperator.INC or operator == UnaryOperator.DEC:
            # Don't allow casting for prefix operators
            operand = self.parse_unary_expression()
        else:
            operand = self.parse_cast_expression()
        return UnaryExpression(operator, operand)

    def parse_postfix_expression(self) -> Expression:
        """
        Parse a postfix operation, including postfix operators, array accesses,
        function calls, or struct field accesses.

          postfix_expression
            : primary_expression
            | postfix_expression '[' expression ']'
            | postfix_expression '(' ')'
            | postfix_expression '(' argument_expression_list ')'
            | postfix_expression '.' IDENTIFIER
            | postfix_expression PTR_OP IDENTIFIER
            | postfix_expression INC_OP
            | postfix_expression DEC_OP
            | '(' type_name ')' '{' initializer_list '}'      TODO
            | '(' type_name ')' '{' initializer_list ',' '}'  TODO

          argument_expression_list
            : assignment_expression
            | argument_expression_list ',' assignment_expression

        :return: A postfix expression.
        """
        # Parse a primary expression
        result = self.parse_primary_expression()

        # Continually parse postfix expressions
        while True:
            if self.t.cur().type == Tk.OPEN_BRACKET:
                result = self.parse_array_access_expression(result)
            elif self.t.cur().type == Tk.OPEN_PAREN:
                result = self.parse_function_call_expression(result)
            elif self.t.cur().type == Tk.DOT:
                result = self.parse_field_access_expression(result)
            elif self.t.cur().type == Tk.ARROW:
                result = self.parse_deref_access_expression(result)
            elif self.t.cur().type == Tk.INC or self.t.cur().type == Tk.DEC:
                operator = UnaryOperator(self.t.cur().type)
                self.t.next()
                result = PostfixExpression(operator, result)
            else:
                # No more postfix expressions
                break
        return result

    def parse_array_access_expression(self, array: Expression) -> Expression:
        """
        Parse an array access after a primary expression.

        :param array: The array that we're accessing.
        :return:      An array access expression node.
        """
        # Expect an opening bracket
        self.t.expect(Tk.OPEN_BRACKET)
        self.t.next()

        # Parse the index within the brackets
        index = self.parse_expression()

        # Expect a closing bracket
        self.t.expect(Tk.CLOSE_BRACKET)
        self.t.next()
        return ArrayAccessExpression(array, index)

    def parse_function_call_expression(self, func: Expression) -> Expression:
        """
        Parse a function call after a primary expression.

        :param func: The function that we're calling.
        :return:     A function call expression node.
        """
        # Parse opening parenthesis
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()

        # Parse arguments
        args = []
        while self.t.cur().type != Tk.CLOSE_PAREN:
            # Parse an argument
            args.append(self.parse_expression_root())

            # Expect a comma if we haven't reached the end of the arguments
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a closing parenthesis
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()
        return FunctionCallExpression(func, args)

    def parse_field_access_expression(self, struct: Expression) -> Expression:
        """
        Parse a struct field access through a direct access (using '.').

        :param struct: The struct to access a field on.
        :return:       A field access expression node.
        """
        # Parse the dot
        self.t.expect(Tk.DOT)
        self.t.next()

        # Parse the field to access
        self.t.expect(Tk.IDENT)
        access = FieldAccessExpression(struct, self.t.cur())
        self.t.next()
        return access

    def parse_deref_access_expression(self, struct: Expression) -> Expression:
        """
        Parse a struct field access through a dereference access (using '->').

        :param struct: The struct to access a field on.
        :return:       A field access expression node.
        """
        # Parse the arrow
        self.t.expect(Tk.ARROW)
        self.t.next()

        # Parse the field to access
        self.t.expect(Tk.IDENT)
        field = self.t.cur()
        self.t.next()

        # Dereference the struct first, then access a field on it
        deref = UnaryExpression(UnaryOperator.DEREF, struct)
        access = FieldAccessExpression(deref, field)
        return access

    def parse_primary_expression(self) -> Expression:
        """
        Parse a primary expression, including symbols, constants, and
        subexpressions. The relevant grammar is:

          primary_expression
            : IDENTIFIER
            | constant
            | string
            | '(' expression ')'

          constant
            : INTEGER_CONSTANT
            | FLOAT_CONSTANT
            | ENUMERATION_CONSTANT

        :return: A primary expression.
        """
        type = self.t.cur().type
        if type == Tk.IDENT:
            # Parse a symbol from an identifier
            result = SymbolExpression(self.t.cur())
            self.t.next()
        elif type == Tk.CONST_INT or type == Tk.CONST_FLOAT or \
                type == Tk.CONST_CHAR or type == Tk.CONST_STR:
            # Parse a constant
            result = ConstantExpression(self.t.cur())
            self.t.next()
        elif type == Tk.OPEN_PAREN:
            # Skip the open parenthesis
            self.t.next()

            # Parse the expression contained in the parentheses
            result = self.parse_expression()

            # Expect a closing parenthesis
            self.t.expect(Tk.CLOSE_PAREN)
            self.t.next()
        else:
            # Expected expression error
            raise Error("expected expression", self.t.cur())
        return result


"""
A list of all built-in type names.
"""
BUILT_IN_TYPE_TOKENS = {
    Tk.VOID, Tk.CHAR, Tk.SHORT, Tk.INT, Tk.LONG, Tk.SIGNED, Tk.UNSIGNED,
    Tk.FLOAT, Tk.DOUBLE,
}


"""
A mapping between a built-in type specifier, and a corresponding list of
tokens (sorted in alphabetical order) that can be used to specify that built-in
type.
"""
BUILT_IN_TYPES = {
    TypeSpecifier.VOID: [[Tk.VOID]],
    TypeSpecifier.CHAR: [[Tk.CHAR], [Tk.CHAR, Tk.SIGNED]],
    TypeSpecifier.UCHAR: [[Tk.CHAR, Tk.UNSIGNED]],
    TypeSpecifier.SHORT: [[Tk.SHORT], [Tk.SHORT, Tk.SIGNED], [Tk.INT, Tk.SHORT],
                          [Tk.INT, Tk.SHORT, Tk.SIGNED]],
    TypeSpecifier.USHORT: [[Tk.SHORT, Tk.UNSIGNED],
                           [Tk.INT, Tk.SHORT, Tk.UNSIGNED]],
    TypeSpecifier.INT: [[Tk.INT], [Tk.SIGNED], [Tk.INT, Tk.SIGNED]],
    TypeSpecifier.UINT: [[Tk.UNSIGNED], [Tk.INT, Tk.UNSIGNED]],
    TypeSpecifier.LONG: [[Tk.LONG], [Tk.LONG, Tk.SIGNED], [Tk.INT, Tk.LONG],
                         [Tk.INT, Tk.LONG, Tk.SIGNED]],
    TypeSpecifier.ULONG: [[Tk.LONG, Tk.UNSIGNED],
                          [Tk.INT, Tk.LONG, Tk.UNSIGNED]],
    TypeSpecifier.LLONG: [[Tk.LONG, Tk.LONG], [Tk.LONG, Tk.LONG, Tk.SIGNED],
                          [Tk.INT, Tk.LONG, Tk.LONG],
                          [Tk.INT, Tk.LONG, Tk.LONG, Tk.SIGNED]],
    TypeSpecifier.ULLONG: [[Tk.LONG, Tk.LONG, Tk.UNSIGNED],
                           [Tk.INT, Tk.LONG, Tk.LONG, Tk.UNSIGNED]],
    TypeSpecifier.FLOAT: [[Tk.FLOAT]],
    TypeSpecifier.DOUBLE: [[Tk.DOUBLE], [Tk.DOUBLE, Tk.LONG]],
}


"""
Lists of tokens that constitute each part of a declaration specifier.
"""
STORAGE_CLASSES = [Tk(x.value) for x in StorageClass]
TYPE_QUALIFIERS = [Tk(x.value) for x in TypeQualifier]
FUNCTION_SPECIFIERS = [Tk(x.value) for x in FunctionSpecifier]


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
