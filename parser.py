
# parser.py
# By Ben Anderson
# December 2018

from __future__ import annotations
from typing import Optional, Union
from enum import Enum

from lexer import Tokens, Token, Tk
from error import Error


class Node:
    """
    Base class for any AST node.
    """
    pass


# ******************************************************************************
#     Declarators
# ******************************************************************************


class DeclarationList(Node):
    """
    Multiple declarations can occur after one set of declaration specifiers.
    This node stores a list of declarations.
    """

    def __init__(self, declarations):
        super().__init__()
        self.declarations = declarations


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

    # Typedefs
    TYPEDEF = "typedef"

    # Structs, unions, and enums
    STRUCT = "struct"
    UNION = "union"
    ENUM = "enum"

    def __init__(self, _):
        """
        Creates a new type specifier, with potential to include information
        about a typedef name, struct, union, or enum.
        """
        self.typedef_name = None
        self.struct = None
        self.union = None
        self.enum = None


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


class StructSpecifier(Node):
    """
    A struct stores a series of fields as a list of declarations (with optional
    declarators).
    """

    def __init__(self, name=None, fields=None):
        super().__init__()
        self.name = name      # Identifier token
        self.fields = fields  # List of declarations (None for incomplete type)


class UnionSpecifier(Node):
    """
    A union stores a series of fields as a list of declarations (with optional
    declarators).
    """

    def __init__(self, name=None, fields=None):
        super().__init__()
        self.name = name      # Identifier token
        self.fields = fields  # List of declarations (None for incomplete type)


class EnumSpecifier(Node):
    """
    An enum stores a list of enumerator constants that can be used within a
    scope.
    """

    def __init__(self, name=None, consts=None):
        super().__init__()
        self.name = name      # Identifier token
        self.consts = consts  # EnumConst list (None for incomplete type)


class EnumConst(Node):
    """
    An enumerator constant is an identifier with an optional constant
    expression.
    """

    def __init__(self, name, expr=None):
        super().__init__()
        self.name = name  # Identifier token
        self.expr = expr  # Constant expression


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
        self.size = None        # Constant expression
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
        self.args = []  # List of declarations


class DeclaratorPointerPart(Node):
    """
    Information about a pointer part of a declarator. This includes any type
    qualifiers specified alongside the pointer.
    """

    def __init__(self):
        super().__init__()
        self.type_qualifiers = set()


class InitializerList(Node):
    """
    An initializer in a declaration is either an expression or an initializer
    list, implicitly creating a struct.
    """

    def __init__(self, fields: list):
        super().__init__()
        self.fields = fields  # List of initializers


class Initializer(Node):
    """
    An element in an initializer list. Consists of a designator list (possibly
    empty), and either an expression or another initializer list.
    """

    def __init__(self, designators: list,
                 initializer: Union[Expression, InitializerList]):
        super().__init__()
        self.designators = designators  # List of struct or array designators
        self.initializer = initializer  # An expression or initializer list


class StructDesignator(Node):
    """
    A designator in an initializer list either specifies a struct field or an
    array index to assign to.
    """

    def __init__(self, field: Token):
        super().__init__()
        self.field = field  # Identifier token specifying the struct field


class ArrayDesignator(Node):
    """
    A designator in an initializer list either specifies a struct field or an
    array index to assign to.
    """

    def __init__(self, index: Expression):
        super().__init__()
        self.index = index  # Constant expression


# ******************************************************************************
#     Expressions
# ******************************************************************************


class Expression(Node):
    """
    This is the base class for any component of an expression.
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

    def __init__(self, specifiers: DeclarationSpecifiers,
                 declarator: Optional[Declarator], operand: Expression):
        super().__init__()
        self.specifiers = specifiers
        self.declarator = declarator
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

    def __init__(self, specifiers: DeclarationSpecifiers,
                 declarator: Declarator):
        super().__init__()
        self.specifiers = specifiers
        self.declarator = declarator


class InitializerExpression(Expression):
    """
    An initializer expression creates a struct (with an explicit type name) from
    an initializer list, within an expression.
    """

    def __init__(self, type: TypeName, initializer_list: InitializerList):
        super().__init__()
        self.type = type
        self.initializer_list = initializer_list


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
#     Statements
# ******************************************************************************


class FunctionDefinition(Node):
    """
    A function definition contains a set of declaration specifiers and a
    declarator that tell us the function signature and name of the function,
    and a block of statements.
    """

    def __init__(self, specifiers: DeclarationSpecifiers,
                 declarator: Declarator, body: CompoundStatement):
        super().__init__()
        self.specifiers = specifiers
        self.declarator = declarator
        self.body = body


class Statement(Node):
    """
    Base class for a statement.
    """
    pass


class CompoundStatement(Statement):
    """
    A compound statement consists of a list of other statements contained in
    braces.
    """

    def __init__(self, statements: list):
        super().__init__()
        self.statements = statements


class DeclarationStatement(Statement):
    """
    A declaration statement may contain multiple declarations under a single set
    of declaration specifiers.
    """

    def __init__(self, declarations: DeclarationList):
        super().__init__()
        self.declarations = declarations


class ExpressionStatement(Statement):
    """
    An expression statement contains an expression (e.g. an assignment or
    function call). This is only useful if the expression has side effects,
    otherwise the result is unused.
    """

    def __init__(self, expression: Expression):
        super().__init__()
        self.expression = expression


class IfStatementChain(Statement):
    """
    An if statement chain stores a list of if statements that represent an
    initial if, followed optionally by a list of else statements, followed by
    an optional else statement.
    """

    def __init__(self, chain: list):
        super().__init__()
        self.chain = chain


class IfStatement(Statement):
    """
    An if statement conditionally executes a block based on the result of an
    expression.

    The condition is None for an else statement.
    """

    def __init__(self, condition: Expression, body: Statement):
        super().__init__()
        self.condition = condition
        self.body = body


class SwitchStatement(Statement):
    """
    A switch statement jumps to a case or default statement contained in its
    block based on a condition.
    """

    def __init__(self, condition: Expression, body: Statement):
        super().__init__()
        self.condition = condition
        self.body = body


class CaseStatement(Statement):
    """
    A case statement can only occur inside the block that follows a switch
    statement.
    """

    def __init__(self, condition: Expression):
        super().__init__()
        self.condition = condition


class DefaultStatement(Statement):
    """
    A default statement can only occur inside the block that follows a switch
    statement.
    """
    pass


class WhileStatement(Statement):
    """
    A while statement repeats its block as long as the condition is true.
    """

    def __init__(self, condition: Expression, body: Statement):
        super().__init__()
        self.condition = condition
        self.body = body


class DoWhileStatement(Statement):
    """
    A do while statement repeats its block as long as the condition is true,
    executing its block at least once.
    """

    def __init__(self, condition: Expression, body: Statement):
        super().__init__()
        self.condition = condition
        self.body = body


class ForStatement(Statement):
    """
    A for statement includes an initializer, condition, and increment
    expression, and a block. The initializer can be either a declaration or
    expression.
    """

    def __init__(self, initializer: Union[DeclarationList, Expression],
                 condition: Expression, increment: Expression, body: Statement):
        super().__init__()
        self.initializer = initializer
        self.condition = condition
        self.increment = increment
        self.body = body


class ContinueStatement(Statement):
    """
    A continue statement jumps back to the start of the loop its contained in.
    """
    pass


class BreakStatement(Statement):
    """
    A break statement jumps to after the loop its contained in.
    """
    pass


class ReturnStatement(Statement):
    """
    A return statement breaks out of the function its contained in, optionally
    with an expression.
    """

    def __init__(self, result: Expression):
        super().__init__()
        self.result = result


class GotoStatement(Statement):
    """
    A goto statement jumps to a label within the same function.
    """

    def __init__(self, name: Token):
        super().__init__()
        self.name = name


class LabelStatement(Statement):
    """
    A label statement specifies a point that a goto statement can jump to.
    """

    def __init__(self, name: Token):
        super().__init__()
        self.name = name


# ******************************************************************************
#     Parser
# ******************************************************************************


class Scope:
    """
    A scope stores various lists of identifiers so we can disambiguate between
    usages.
    """

    def __init__(self):
        self.typedefs = []


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
        self.scopes = []  # Inner-most scope is the last element in the list

    def push_scope(self):
        """
        Adds a new scope to the top of the scopes stack.
        """
        self.scopes.append(Scope())

    def pop_scope(self):
        """
        Removes the top scope off the scopes stack.
        """
        self.scopes.pop()

    def find_typedef(self, name: str) -> bool:
        """
        Checks to see if the given name is a typedef symbol from some parent
        scope.

        :param name: The name (as a string) to check.
        :return:     True if the name was used in a typedef in a parent scope.
        """
        # Iterate over the scopes in reverse order, so we search the inner-most
        # scope first
        for scope in reversed(self.scopes):
            if name in scope.typedefs:
                return True
        return False

    # **************************************************************************
    #     Root AST Node Parsing
    # **************************************************************************

    def gen(self) -> list:
        """
        Generate the AST. The entry point for the grammar is:

          translation_unit
            : external_declaration
            | translation_unit external_declaration

        :return: A list of root AST nodes.
        """
        # Create a file-level scope
        self.push_scope()

        # Keep parsing root nodes until we reach the end of the file
        nodes = []
        while self.t.cur().type != Tk.EOF:
            node = self.parse_root_node()
            nodes.append(node)

        # Destroy the file-level scope
        self.pop_scope()
        return nodes

    def parse_root_node(self):
        """
        Parse a root AST node, which occurs at the top level of the source code.
        This is either a function definition or declaration.

          external_declaration
              : function_definition
              | declaration

        :return: A root AST node.
        """
        # The scoping rules for a declaration and a function definition are
        # different. For a function definition, any enum constants defined in
        # its arguments can be used in the function's body, so we need to create
        # a new scope BEFORE we parse the function declarator. For a
        # declaration, any enum constants need to be included in the file-level
        # scope (i.e. we don't create a new scope at all).
        #
        # Because of the different scoping rules, we need to know prior to
        # parsing whether we're dealing with a function definition or a
        # declaration. This is determined by the token AFTER the declaration
        # specifiers and declarator
        saved = self.t.save()
        self.parse_declaration_specifiers()
        self.parse_declarator()
        discriminator = self.t.cur().type
        self.t.restore(saved)

        # Depending on the discriminating token
        if discriminator == Tk.OPEN_BRACE:
            return self.parse_function_definition()
        else:
            return self.parse_declaration_list()

    def parse_function_definition(self):
        """
        Parse a function definition. The relevant grammar is:

          function_definition
              : declaration_specifiers declarator declaration_list
                  compound_statement                 NOT SUPPORTED
              | declaration_specifiers declarator compound_statement

        Note we don't support the old K&R style function definitions, indicated
        by the 'NOT SUPPORTED' tag next to the relevant line in the grammar.

        :return: A function definition root AST node.
        """
        # Parse a set of declaration specifiers
        specifiers = self.parse_declaration_specifiers()

        # The declarator tells us the function's name and its type signature
        declarator = self.parse_declarator()

        # Check we actually declared a function
        if len(declarator.parts) == 0 or \
                not isinstance(declarator.parts[-1], DeclaratorFunctionPart):
            # TODO: associate a token with this error
            raise Error("expected function declaration")

        # Parse a block of statement
        block = self.parse_compound_statement()

        # Construct a function definition
        definition = FunctionDefinition(specifiers, declarator, block)
        return definition

    # **************************************************************************
    #     Declaration Parsing
    # **************************************************************************

    def parse_declaration_list(self) -> DeclarationList:
        """
        Parse a list of declarations. The relevant grammar is:

            declaration
                : declaration_specifiers ';'
                | declaration_specifiers init_declarator_list ';'
                | static_assert_declaration     NOT SUPPORTED

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
            # Parse a declarator
            declarator = self.parse_declarator()

            # Check for an optional initializer
            initializer = None
            if self.t.cur().type == Tk.ASSIGN:
                self.t.next()
                initializer = self.parse_initializer()

            # Add a declaration
            declaration = Declaration(specifiers, declarator, initializer)
            declarations.append(declaration)

            # Check if this declaration is a typedef
            if specifiers.storage_class == StorageClass.TYPEDEF and \
                    declarator.name is not None:
                # Add the typedef name to the current scope
                scope = self.scopes[-1]
                scope.typedefs.append(declarator.name.contents)

            # Check for a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return DeclarationList(declarations)

    def parse_initializer(self) -> Union[Expression, InitializerList]:
        """
        Parse an initializer, which can either be an expression or an
        initializer list. The relevant grammar is:

            initializer
                : '{' initializer_list '}'
                | '{' initializer_list ',' '}'
                | assignment_expression

        :return: An expression or an initializer list
        """
        # Check for an initializer list
        if self.t.cur().type == Tk.OPEN_BRACE:
            return self.parse_initializer_list()
        else:
            return self.parse_expression_root()

    def parse_initializer_list(self) -> InitializerList:
        """
        Parse an initializer list, used in declarations and in expressions (when
        preceded by a type name). The relevant grammar is:

            initializer_list
                : designation initializer
                | initializer
                | initializer_list ',' designation initializer
                | initializer_list ',' initializer

            designation
                : designator_list '='

            designator_list
                : designator
                | designator_list designator

            designator
                : '[' constant_expression ']'
                | '.' IDENTIFIER

        :return: An initializer list.
        """
        # Skip the opening brace
        self.t.expect(Tk.OPEN_BRACE)
        self.t.next()

        # Keep parsing initializers
        initializers = []
        while self.t.cur().type != Tk.CLOSE_BRACE:
            # Parse a designator list
            designators = self.parse_designators()

            # Parse an initializer
            initializer = self.parse_initializer()
            initializers.append(Initializer(designators, initializer))

            # Parse a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a closing brace
        self.t.expect(Tk.CLOSE_BRACE)
        self.t.next()
        return InitializerList(initializers)

    def parse_designators(self) -> list:
        """
        Parse a (possibly empty) list of designators, followed by an assignment
        token.

        :return: A list of designators
        """
        # Parse a designator list
        designators = []
        while self.t.cur().type == Tk.OPEN_BRACKET or \
                self.t.cur().type == Tk.DOT:
            # Skip the opening token
            type = self.t.cur().type
            self.t.next()

            # Depending on the type of the designator
            if type == Tk.OPEN_BRACKET:
                # Parse a constant expression
                expr = self.parse_expression_root()
                designator = ArrayDesignator(expr)

                # Expect a closing bracket
                self.t.expect(Tk.CLOSE_BRACKET)
                self.t.next()
            else:
                # Expect an identifier
                self.t.expect(Tk.IDENT)
                field = self.t.cur()
                designator = StructDesignator(field)
                self.t.next()
            designators.append(designator)

        # If the designator list is non-empty, expect an assignment token
        if len(designators) > 0:
            self.t.expect(Tk.ASSIGN)
            self.t.next()
        return designators

    def parse_type_name(self) -> TypeName:
        """
        Parse a type name, consisting of a set of declaration specifiers
        followed by an optional declarator. The relevant grammar is:

            type_name
                : specifier_qualifier_list abstract_declarator
                | specifier_qualifier_list

            specifier_qualifier_list
                : type_specifier specifier_qualifier_list
                | type_specifier
                | type_qualifier specifier_qualifier_list
                | type_qualifier

        This function differs from the above grammar, in that it doesn't
        insist that the declarator be abstract. It's up to the calling function
        to check if the declarator is abstract or not (depending on what it
        wants).

        :return: The parsed type.
        """
        # Parse a set of declaration specifiers
        specifiers = self.parse_declaration_specifiers()

        # Parse an optional declarator
        declarator = None
        type = self.t.cur().type
        if type != Tk.CLOSE_PAREN and type != Tk.COMMA:
            declarator = self.parse_declarator()
        return TypeName(specifiers, declarator)

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
                | alignment_specifier declaration_specifiers   NOT SUPPORTED
                | alignment_specifier                          NOT SUPPORTED

            storage_class_specifier
                : TYPEDEF
                | EXTERN
                | STATIC
                | AUTO
                | THREAD_LOCAL               NOT SUPPORTED
                | REGISTER

            type_specifier
                : VOID
                | CHAR
                | SHORT
                | INT
                | LONG
                | FLOAT
                | DOUBLE
                | SIGNED
                | UNSIGNED
                | BOOL                       NOT SUPPORTED
                | COMPLEX                    NOT SUPPORTED
                | IMAGINARY                  NOT SUPPORTED
                | struct_or_union_specifier
                | enum_specifier
                | TYPEDEF_NAME

            type_qualifier
                : CONST
                | RESTRICT
                | VOLATILE

            function_specifier
                : INLINE

        :return: The set of declaration specifiers.
        """
        type_tokens = []

        # Keep parsing tokens until we reach one that isn't valid
        specifiers = DeclarationSpecifiers()
        while self.t.cur().type != Tk.EOF:
            type = self.t.cur().type
            if type in STORAGE_CLASSES:
                # Check we haven't already got a storage class
                if specifiers.storage_class is not None:
                    raise Error(f"can't have two storage classes", self.t.cur())
                specifiers.storage_class = StorageClass(type)
                self.t.next()
            elif type in BUILT_IN_TYPE_TOKENS:
                # Try parse a type specifier
                type_tokens.append(type)
                type_specifier = self.parse_type_specifier(type_tokens)
                specifiers.type_specifier = type_specifier
                self.t.next()
            elif type in TYPE_QUALIFIERS:
                # Add another type qualifier
                qualifier = TypeQualifier(type)
                specifiers.type_qualifiers.add(qualifier)
                self.t.next()
            elif type in FUNCTION_SPECIFIERS:
                # Add another function specifier
                specifier = FunctionSpecifier(type)
                specifiers.function_specifiers.add(specifier)
                self.t.next()
            elif type == Tk.IDENT and self.find_typedef(self.t.cur().contents):
                # Check we don't have any other type specifiers
                if len(type_tokens) > 0:
                    raise Error("can't combine type", self.t.cur())
                specifiers.type_specifier = TypeSpecifier.TYPEDEF
                specifiers.type_specifier.typedef_name = self.t.cur().contents
                self.t.next()
            elif type == Tk.STRUCT:
                # Check we don't have any other type specifiers
                if len(type_tokens) > 0:
                    raise Error("can't combine type", self.t.cur())
                specifiers.type_specifier = TypeSpecifier.STRUCT
                specifiers.type_specifier.struct = self.parse_struct_specifier()
            elif type == Tk.UNION:
                # Check we don't have any other type specifiers
                if len(type_tokens) > 0:
                    raise Error("can't combine type", self.t.cur())
                specifiers.type_specifier = TypeSpecifier.UNION
                specifiers.type_specifier.union = self.parse_union_specifier()
            elif type == Tk.ENUM:
                # Check we don't have any other type specifiers
                if len(type_tokens) > 0:
                    raise Error("can't combine type", self.t.cur())
                specifiers.type_specifier = TypeSpecifier.ENUM
                specifiers.type_specifier.enum = self.parse_enum_specifier()
            else:
                # Token isn't a valid declaration specifier
                break

        # Check we had a type specifier
        if specifiers.type_specifier is None:
            raise Error("expected type name", self.t.cur())
        return specifiers

    def parse_type_specifier(self, type_tokens: list) -> TypeSpecifier:
        """
        Takes a list of type tokens (e.g. int, long, unsigned, etc.) and tries
        to find a type specifier that corresponds to these tokens. Triggers an
        error if it can't.

        :param type_tokens: The list of type tokens to parse.
        :return:            A type specifier corresponding to the tokens.
        """
        # Try match the accumulated type tokens to a built in type
        type_specifier = None
        for (candidate, options) in BUILT_IN_TYPES.items():
            # Convert all the tokens to strings
            if sorted(type_tokens, key=lambda x: x.value) in options:
                type_specifier = candidate
                break

        # Check we could find a matching type specifier
        if type_specifier is None:
            raise Error("cannot combine type", self.t.cur())
        return type_specifier

    def parse_struct_specifier(self) -> StructSpecifier:
        """
        Parses a struct type specifier in a declaration. See
        `parse_struct_or_union` for the relevant grammar.

        :return: Information about the struct declaration.
        """
        # Skip the `struct` keyword
        self.t.expect(Tk.STRUCT)
        self.t.next()

        # Parse the declaration
        name, fields = self.parse_struct_or_union()
        struct = StructSpecifier(name, fields)
        return struct

    def parse_union_specifier(self) -> UnionSpecifier:
        """
        Parses a union type specifier in a declaration. See
        `parse_struct_or_union` for the relevant grammar.

        :return: Information about the union declaration.
        """
        # Skip the `union` keyword
        self.t.expect(Tk.UNION)
        self.t.next()

        # Parse the declaration
        name, fields = self.parse_struct_or_union()
        union = UnionSpecifier(name, fields)
        return union

    def parse_struct_or_union(self) -> (Optional[Token], Optional[list]):
        """
        Parses a struct or union declaration, returning the optional name of the
        struct/union, and an optional list of fields (None for an incomplete
        declaration). The relevant grammar is:

            struct_or_union_specifier
                : struct_or_union '{' struct_declaration_list '}'
                | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
                | struct_or_union IDENTIFIER

            struct_or_union: STRUCT | UNION

            struct_declaration_list
                : struct_declaration
                | struct_declaration_list struct_declaration

            struct_declaration
                : specifier_qualifier_list ';'
                | specifier_qualifier_list struct_declarator_list ';'
                | static_assert_declaration                 NOT SUPPORTED

            struct_declarator_list
                : struct_declarator
                | struct_declarator_list ',' struct_declarator

            struct_declarator
                : ':' constant_expression                   NOT SUPPORTED
                | declarator ':' constant_expression        NOT SUPPORTED
                | declarator

        :return: An optional name and fields list.
        """
        # We've already parsed the `struct` or `union` keyword
        # Check for an identifier
        name = None
        if self.t.cur().type == Tk.IDENT:
            name = self.t.cur()
            self.t.next()

        # If there's no open parenthesis, then this is an incomplete declaration
        # (and we MUST have a name)
        if self.t.cur().type != Tk.OPEN_BRACE:
            if name is None:
                raise Error("expected struct name", self.t.cur())
            return name, None
        self.t.next()

        # Keep parsing declarations until we reach the terminating brace
        fields = []
        while self.t.cur().type != Tk.CLOSE_BRACE:
            # Parse a declaration
            specifiers = self.parse_declaration_specifiers()

            # Parse a list of declarators separated by commas
            while self.t.cur().type != Tk.SEMICOLON:
                # Parse a declarator and add a declaration
                declarator = self.parse_declarator()
                declaration = Declaration(specifiers, declarator)
                fields.append(declaration)

                # Check for another declarator
                if self.t.cur().type == Tk.COMMA:
                    self.t.next()
                else:
                    break

            # Ensure a semicolon terminates the declaration list
            if self.t.cur().type == Tk.SEMICOLON:
                self.t.next()
            else:
                break

        # Expect a closing brace
        self.t.expect(Tk.CLOSE_BRACE)
        self.t.next()
        return name, fields

    def parse_enum_specifier(self) -> EnumSpecifier:
        """
        Parses an enum type specifier in a declaration. The relevant grammar is:

            enum_specifier
                : ENUM '{' enumerator_list '}'
                | ENUM '{' enumerator_list ',' '}'
                | ENUM IDENTIFIER '{' enumerator_list '}'
                | ENUM IDENTIFIER '{' enumerator_list ',' '}'
                | ENUM IDENTIFIER

            enumerator_list
                : enumerator
                | enumerator_list ',' enumerator

            enumerator
                : enumeration_constant '=' constant_expression
                | enumeration_constant

        :return: Information about the enum declaration.
        """
        # Skip the enum token
        self.t.expect(Tk.ENUM)
        self.t.next()

        # Check for an identifier
        name = None
        if self.t.cur().type == Tk.IDENT:
            name = self.t.cur()
            self.t.next()

        # If we don't have an opening brace, then we need a name
        if self.t.cur().type != Tk.OPEN_BRACE:
            if name is None:
                raise Error("expected enum name", self.t.cur())
            return EnumSpecifier(name)
        self.t.next()

        # Parse a series of enumerator constants separated by commas
        consts = []
        while self.t.cur().type != Tk.CLOSE_BRACE:
            # Expect an identifier
            self.t.expect(Tk.IDENT)
            const_name = self.t.cur()
            self.t.next()

            # Check for a constant expression
            expr = None
            if self.t.cur().type == Tk.ASSIGN:
                self.t.next()
                expr = self.parse_expression_root()

            # Add a constant
            const = EnumConst(const_name, expr)
            consts.append(const)

            # Expect a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a close brace
        self.t.expect(Tk.CLOSE_BRACE)
        self.t.next()
        return EnumSpecifier(name, consts)

    def parse_declarator(self) -> Declarator:
        """
        Parse a declarator. A great article on declarator parsing describing the
        approach used here can be found at:

            http://blog.robertelder.org/building-a-c-compiler-type-system-the-
            formidable-declarator/

        The relevant grammar is:

            declarator
                : pointer direct_declarator
                | direct_declarator

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

        This function parses both named and abstract declarators. Note the old
        K&R style function definitions are not supported, as indicated by the
        'NOT SUPPORTED' tag next to the relevant line in the above grammar.

        :return: The parsed declarator.
        """
        # Parse a declarator recursively
        declarator = Declarator()
        self.parse_declarator_recursive(declarator)

        # Check we actually parsed something
        if len(declarator.parts) == 0 and declarator.name is None:
            raise Error("expected declarator", self.t.cur())
        return declarator

    def parse_declarator_recursive(self, declarator: Declarator):
        """
        Parse a single part of a declarator (either an array, pointer, or
        function part).

        :param declarator: The resulting declarator that we're parsing.
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

        # Everything defined in the function declarator is in its own scope
        # (e.g. enums constants defined in the function prototype are not
        # accessible outside the declarator)
        self.push_scope()

        # Parse a list of argument declarations
        function = DeclaratorFunctionPart()
        while self.t.cur().type != Tk.EOF and \
                self.t.cur().type != Tk.CLOSE_PAREN:
            arg = self.parse_type_name()
            function.args.append(arg)

            # Parse a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a close parenthesis
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()

        # Destroy the scope we just created
        self.pop_scope()
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
        return array

    # **************************************************************************
    #     Statement Parsing
    # **************************************************************************

    def parse_compound_statement(self) -> CompoundStatement:
        """
        Parse a list of statements surrounded by braces. The relevant grammar
        is:

            compound_statement
                : '{' '}'
                | '{'  block_item_list '}'

            block_item_list
                : block_item
                | block_item_list block_item

        :return: A list of statements.
        """
        # Opening brace
        self.t.expect(Tk.OPEN_BRACE)
        self.t.next()

        # Create a new scope for the block
        self.push_scope()

        # Parse a list of statements
        statements = []
        while self.t.cur().type != Tk.EOF and \
                self.t.cur().type != Tk.CLOSE_BRACE:
            statement = self.parse_statement()
            statements.append(statement)

        # Destroy the scope we just created
        self.pop_scope()

        # Closing brace
        self.t.expect(Tk.CLOSE_BRACE)
        self.t.next()
        return CompoundStatement(statements)

    def parse_statement(self) -> Statement:
        """
        Parse a single statement (called a 'block_item' in the grammar). The
        relevant grammar is:

            block_item
                : declaration
                | statement

            statement
                : labeled_statement
                | compound_statement
                | expression_statement
                | selection_statement
                | iteration_statement
                | jump_statement

            labeled_statement
                : IDENTIFIER ':' statement
                | CASE constant_expression ':' statement
                | DEFAULT ':' statement

            expression_statement
                : ';'
                | expression ';'

            selection_statement
                : IF '(' expression ')' statement ELSE statement
                | IF '(' expression ')' statement
                | SWITCH '(' expression ')' statement

            iteration_statement
                : WHILE '(' expression ')' statement
                | DO statement WHILE '(' expression ')' ';'
                | FOR '(' expression_statement expression_statement ')'
                    statement
                | FOR '(' expression_statement expression_statement expression
                    ')' statement
                | FOR '(' declaration expression_statement ')' statement
                | FOR '(' declaration expression_statement expression ')'
                    statement

            jump_statement
                : GOTO IDENTIFIER ';'
                | CONTINUE ';'
                | BREAK ';'
                | RETURN ';'
                | RETURN expression ';'

        :return: A statement object.
        """
        if self.t.cur().type == Tk.IDENT and self.t.peek(1).type == Tk.COLON:
            return self.parse_labelled_statement()
        elif self.t.cur().type == Tk.CASE:
            return self.parse_case_statement()
        elif self.t.cur().type == Tk.DEFAULT:
            return self.parse_default_statement()
        elif self.t.cur().type == Tk.OPEN_BRACE:
            return self.parse_compound_statement()
        elif self.t.cur().type == Tk.IF:
            return self.parse_if_statement()
        elif self.t.cur().type == Tk.SWITCH:
            return self.parse_switch_statement()
        elif self.t.cur().type == Tk.WHILE:
            return self.parse_while_statement()
        elif self.t.cur().type == Tk.DO:
            return self.parse_do_while_statement()
        elif self.t.cur().type == Tk.FOR:
            return self.parse_for_statement()
        elif self.t.cur().type == Tk.GOTO:
            return self.parse_goto_statement()
        elif self.t.cur().type == Tk.CONTINUE:
            return self.parse_continue_statement()
        elif self.t.cur().type == Tk.BREAK:
            return self.parse_break_statement()
        elif self.t.cur().type == Tk.RETURN:
            return self.parse_return_statement()
        elif self.t.cur().type in BUILT_IN_TYPE_TOKENS:
            return self.parse_declaration_statement()
        elif self.t.cur().type == Tk.SEMICOLON:  # Ignore random semicolons
            self.t.next()  # Skip the semicolon
            return self.parse_statement()
        else:
            return self.parse_expression_statement()

    def parse_labelled_statement(self) -> LabelStatement:
        """
        Parse a statement preceded by a label. The relevant grammar is:

            labeled_statement
                : IDENTIFIER ':' statement
                ...

        :return: A labelled statement.
        """
        # Expect an identifier
        self.t.expect(Tk.IDENT)
        name = self.t.cur()
        self.t.next()

        # Expect a colon
        self.t.expect(Tk.COLON)
        self.t.next()
        return LabelStatement(name)

    def parse_case_statement(self) -> CaseStatement:
        """
        Parse a case statement. The relevant grammar is:

            labeled_statement
                ...
                | CASE constant_expression ':' statement
                ...

        :return: A case statement.
        """
        # Expect a case keyword
        self.t.expect(Tk.CASE)
        self.t.next()

        # Parse the following expression
        condition = self.parse_expression_root()

        # Expect a colon
        self.t.expect(Tk.COLON)
        self.t.next()
        return CaseStatement(condition)

    def parse_default_statement(self) -> DefaultStatement:
        """
        Parse a default statement. The relevant grammar is:

            labeled_statement
                ...
                | DEFAULT ':' statement

        :return: A case statement.
        """
        # Expect a default keyword
        self.t.expect(Tk.DEFAULT)
        self.t.next()

        # Expect a colon
        self.t.expect(Tk.COLON)
        self.t.next()
        return DefaultStatement()

    def parse_if_statement(self) -> IfStatementChain:
        """
        Parse an if statement. The relevant grammar is:

            selection_statement
                : IF '(' expression ')' statement ELSE statement
                | IF '(' expression ')' statement
                ...

        :return: An if statement.
        """
        # We represent an if statement and any following else if or else
        # clauses as an 'IfStatementChain'
        chain = []

        # Parse the initial 'if' clause
        self.t.expect(Tk.IF)
        statement = self.parse_if_clause()
        chain.append(statement)

        # Check for else ifs
        while self.t.cur().type == Tk.ELSE:
            # Check for an else if
            if self.t.peek(1).type == Tk.IF:
                # Skip the 'else' token to land on the 'if'
                self.t.next()
            type = self.t.cur().type

            # Add a new element to the chain
            statement = self.parse_if_clause()
            chain.append(statement)

            # Stop if we've reached an 'else' statement
            if type == Tk.ELSE:
                break
        return IfStatementChain(chain)

    def parse_if_clause(self) -> IfStatement:
        """
        Parses a single if, else if, or else clause within an if statement
        chain.

        :return: An if, else if, or else clause.
        """
        # Skip the 'if' or 'else' token
        type = self.t.cur().type
        self.t.next()

        condition = None
        if type == Tk.IF:
            # Expect an expression surrounded by parentheses
            self.t.expect(Tk.OPEN_PAREN)
            self.t.next()
            condition = self.parse_expression()
            self.t.expect(Tk.CLOSE_PAREN)
            self.t.next()

        # Expect a statement
        body = self.parse_statement()
        return IfStatement(condition, body)

    def parse_switch_statement(self) -> SwitchStatement:
        """
        Parse a switch statement. The relevant grammar is:

            selection_statement
                ...
                | SWITCH '(' expression ')' statement

        :return: A switch statement.
        """
        # Skip the switch token
        self.t.expect(Tk.SWITCH)
        self.t.next()

        # Expect an expression surrounded by parentheses
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()
        condition = self.parse_expression()
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()

        # Expect a statement
        body = self.parse_statement()
        return SwitchStatement(condition, body)

    def parse_while_statement(self) -> WhileStatement:
        """
        Parse a while statement. The relevant grammar is:

            iteration_statement
                : WHILE '(' expression ')' statement
                ...

        :return: A while statement.
        """
        # Skip the while token
        self.t.expect(Tk.WHILE)
        self.t.next()

        # Expect an expression surrounded by parentheses
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()
        condition = self.parse_expression()
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()

        # Expect a statement
        body = self.parse_statement()
        return WhileStatement(condition, body)

    def parse_do_while_statement(self) -> DoWhileStatement:
        """
        Parse a do while statement. The relevant grammar is:

            iteration_statement
                ...
                | DO statement WHILE '(' expression ')' ';'
                ...

        :return: A do while statement.
        """
        # Skip the do token
        self.t.expect(Tk.DO)
        self.t.next()

        # Expect a statement
        body = self.parse_statement()

        # Expect a while token
        self.t.expect(Tk.WHILE)
        self.t.next()

        # Expect an expression in parentheses, concluded by a semicolon
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()
        condition = self.parse_expression()
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return DoWhileStatement(condition, body)

    def parse_for_statement(self) -> ForStatement:
        """
        Parse a for statement. The relevant grammar is:

            iteration_statement
                ...
                | FOR '(' expression_statement expression_statement ')'
                    statement
                | FOR '(' expression_statement expression_statement expression
                    ')' statement
                | FOR '(' declaration expression_statement ')' statement
                | FOR '(' declaration expression_statement expression ')'
                    statement

        :return: A for statement.
        """
        # Skip the for token
        self.t.expect(Tk.FOR)
        self.t.next()

        # Expect an open parenthesis
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()

        # Check if we've got a declaration or an expression
        initializer = None
        if self.t.cur().type in BUILT_IN_TYPE_TOKENS:
            # Parses the terminating semicolon for us
            initializer = self.parse_declaration_list()
        elif self.t.cur().type != Tk.SEMICOLON:
            # Parse an expression
            initializer = self.parse_expression()

            # Expect a semicolon
            self.t.expect(Tk.SEMICOLON)
            self.t.next()
        else:
            # Have just a semicolon
            self.t.next()

        # Check for a condition
        condition = None
        if self.t.cur().type != Tk.SEMICOLON:
            condition = self.parse_expression()
        self.t.expect(Tk.SEMICOLON)
        self.t.next()

        # Check for an increment
        increment = None
        if self.t.cur().type != Tk.CLOSE_PAREN:
            increment = self.parse_expression()

        # Expect a close parenthesis
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()

        # Expect a statement
        body = self.parse_statement()
        return ForStatement(initializer, condition, increment, body)

    def parse_goto_statement(self) -> GotoStatement:
        """
        Parse a goto statement. The relevant grammar is:

            jump_statement
                : GOTO IDENTIFIER ';'
                ...

        :return: A goto statement.
        """
        # Expect a goto token
        self.t.expect(Tk.GOTO)
        self.t.next()

        # Expect an identifier
        self.t.expect(Tk.IDENT)
        name = self.t.cur()
        self.t.next()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return GotoStatement(name)

    def parse_continue_statement(self) -> ContinueStatement:
        """
        Parse a continue statement. The relevant grammar is:

            jump_statement
                ...
                | CONTINUE ';'
                ...

        :return: A continue statement.
        """
        # Expect a continue token
        self.t.expect(Tk.CONTINUE)
        self.t.next()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return ContinueStatement()

    def parse_break_statement(self) -> BreakStatement:
        """
        Parse a break statement. The relevant grammar is:

            jump_statement
                ...
                | BREAK ';'
                ...

        :return: A break statement.
        """
        # Expect a break token
        self.t.expect(Tk.BREAK)
        self.t.next()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return BreakStatement()

    def parse_return_statement(self) -> ReturnStatement:
        """
        Parse a return statement. The relevant grammar is:

            jump_statement
                ...
                | RETURN ';'
                | RETURN expression ';'

        :return: A return statement.
        """
        # Expect a return token
        self.t.expect(Tk.RETURN)
        self.t.next()

        # Check for an expression
        result = None
        if self.t.cur().type != Tk.SEMICOLON:
            result = self.parse_expression()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return ReturnStatement(result)

    def parse_declaration_statement(self) -> DeclarationStatement:
        """
        Parse a declaration statement. The relevant grammar is:

            block_item
                : declaration
                ...

        :return: A declaration statement.
        """
        declarations = self.parse_declaration_list()
        return DeclarationStatement(declarations)

    def parse_expression_statement(self) -> ExpressionStatement:
        """
        Parse an expression statement. The relevant grammar is:

            expression_statement
                : ';'
                | expression ';'

        :return: An expression statement.
        """
        # Parse an expression
        expression = self.parse_expression()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        return ExpressionStatement(expression)

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
        :return:         An expression tree.
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

            # Check for a ternary operator
            if operator == BinaryOperator.TERNARY:
                left = self.parse_ternary_expression(left)
            else:
                # Parse the right operand
                right = self.parse_expression_recursive(precedence)

                # Construct a binary operation
                left = BinaryExpression(operator, left, right)
        return left

    def parse_ternary_expression(self, condition: Expression) -> Expression:
        """
        Parse a ternary expression. The relevant grammar is:

            conditional_expression
                : logical_or_expression
                | logical_or_expression '?' expression ':'
                    conditional_expression

        :param condition: The initial condition expression.
        :return:          A ternary expression.
        """
        # Parse the true expression
        true = self.parse_expression()

        # Expect a colon
        self.t.expect(Tk.COLON)
        self.t.next()

        # Parse the false expression
        false = self.parse_expression_recursive(Precedence.TERNARY)

        # Construct a ternary operation
        return TernaryExpression(condition, true, false)

    def try_parse_type_name(self) -> Optional[TypeName]:
        """
        Try parse a type surrounded in parentheses. If no valid type exists,
        then return None. The relevant grammar is:

            type_name
                : specifier_qualifier_list abstract_declarator
                | specifier_qualifier_list

        :return: A type, or None.
        """
        saved = self.t.save()

        # Check for an opening parenthesis
        if self.t.cur().type != Tk.OPEN_PAREN:
            return None
        self.t.next()

        # Try parsing a type
        try:
            type = self.parse_type_name()
        except Error:
            type = None
            self.t.restore(saved)

        # If we found a type, expect a closing parenthesis
        if type is not None:
            self.t.expect(Tk.CLOSE_PAREN)
            self.t.next()
        return type

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
        saved = self.t.save()
        type = self.try_parse_type_name()

        if type is None:
            # Failed to parse a type name, try a subexpression
            return self.parse_sizeof_expression()
        elif self.t.cur().type == Tk.OPEN_BRACE:
            # There's a conflict between casting and initializer expressions in
            # the grammar; we prevent it by ensuring the next token after the
            # type isn't an open brace (for an initializer list)
            self.t.restore(saved)
            return self.parse_sizeof_expression()
        else:
            # Check the declarator is abstract
            if type.declarator is not None and type.declarator.name is not None:
                desc = "expected abstract declarator"
                raise Error(desc, type.declarator.name)

            # Parse the unary expression that follows
            operand = self.parse_sizeof_expression()
            return CastExpression(type.specifiers, type.declarator, operand)

    def parse_sizeof_expression(self) -> Expression:
        """
        Parse a sizeof expression. There are 2 uses of sizeof, either to measure
        the size of an expression, or the size of an explicit type name. The
        first sort is represented as a unary operator on an expression, and the
        second as this special expression tree node.

        The relevant grammar is:

            unary_expression
                ...
                | SIZEOF unary_expression
                | SIZEOF '(' type_name ')'

        :return: A sizeof expression.
        """
        # Check we've got a sizeof operator
        if self.t.cur().type != Tk.SIZEOF:
            return self.parse_unary_expression()
        self.t.next()

        # Try parsing a type in parentheses
        if self.t.cur().type == Tk.OPEN_PAREN:
            type = self.try_parse_type_name()
            if type is not None:
                return SizeofExpression(type.specifiers, type.declarator)

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
                | ALIGNOF '(' type_name ')'        NOT SUPPORTED

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
                | '(' type_name ')' '{' initializer_list '}'
                | '(' type_name ')' '{' initializer_list ',' '}'

            argument_expression_list
                : assignment_expression
                | argument_expression_list ',' assignment_expression

        :return: A postfix expression.
        """
        # Check for a type name in parentheses
        type = self.try_parse_type_name()
        if type is not None:
            initializer_list = self.parse_initializer_list()
            return InitializerExpression(type, initializer_list)

        # If we reach here, then parse a primary expression
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
        Parse an array access after a primary expression. The relevant grammar
        is:

            postfix_expression
                ...
                | postfix_expression '[' expression ']'
                ...

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
        Parse a function call after a primary expression. The relevant grammar
        is:

            postfix_expression
                ...
                | postfix_expression '(' ')'
                | postfix_expression '(' argument_expression_list ')'
                ...

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
        Parse a struct field access through a direct access (using '.'). The
        relevant grammar is:

            postfix_expression
                ...
                | postfix_expression '.' IDENTIFIER
                ...

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
        The relevant grammar is:

            postfix_expression
                ...
                | postfix_expression PTR_OP IDENTIFIER
                ...

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
                : INT_CONSTANT
                | FLOAT_CONSTANT
                | CHAR_CONSTANT
                | enumeration_constant

            enumeration_constant
                : IDENTIFIER

            string
                : STRING_LITERAL
                | '__func__'

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
