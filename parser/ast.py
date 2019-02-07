
# ast.py
# By Ben Anderson
# December 2018

from __future__ import annotations
from typing import Optional, Union
from enum import Enum

from parser.lexer import Token, Tk


class Node:
    """
    Base class for any AST node.
    """

    def __init__(self):
        # Every node has a range of tokens, used to trigger errors in later
        # stages of the compiler pipeline
        self.range = None


# ******************************************************************************
#     Declarations
# ******************************************************************************


class DeclarationList(Node):
    """
    Multiple declarations can occur after one set of declaration specifiers.
    This node stores a list of declarations.
    """

    def __init__(self, declarations: list):
        super().__init__()
        self.declarations = declarations


class Declaration(Node):
    """
    A declaration defines a new symbol. It consists of a set of declaration
    specifiers followed by a declarator and an optional initialization
    expression.
    """

    def __init__(self, specifiers: DeclarationSpecifiers,
                 declarator: Declarator, initializer: Initializer = None):
        super().__init__()
        self.specifiers = specifiers
        self.declarator = declarator
        self.initializer = initializer


class TypeName(Node):
    """
    A type consists of a set of declaration specifiers and a declarator. Only
    in certain cases does this declarator have to be abstract.
    """

    def __init__(self, specifiers: DeclarationSpecifiers,
                 declarator: Declarator = None):
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
        self.type_qualifiers = []
        self.function_specifiers = []


class StorageClassNode(Node):
    """
    We wrap a storage class in an AST node to associate a range property with
    it.
    """

    def __init__(self, type: StorageClass):
        super().__init__()
        self.type = type


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


class TypeSpecifierNode(Node):
    """
    We wrap a type specifier class in an AST node to associate a range property
    with it.
    """

    def __init__(self, type: TypeSpecifier):
        super().__init__()
        self.type = type

        self.typedef_name = None
        self.struct = None
        self.union = None
        self.enum = None


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


class TypeQualifierNode(Node):
    """
    We wrap a type qualifier class in an AST node to associate a range property
    with it.
    """

    def __init__(self, type: TypeQualifier):
        super().__init__()
        self.type = type


class TypeQualifier(Enum):
    """
    All type qualifiers that can occur in a set of declaration specifiers.
    """
    CONST = Tk.CONST
    RESTRICT = Tk.RESTRICT
    VOLATILE = Tk.VOLATILE


class FunctionSpecifierNode(Node):
    """
    We wrap a function specifier in an AST node to associate a range property
    with it.
    """

    def __init__(self, type: FunctionSpecifier):
        super().__init__()
        self.type = type


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
        self.size = None    # Constant expression
        self.static = None  # Token, or None
        self.vla = None     # Token, or None
        self.type_qualifiers = []


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
        self.type_qualifiers = []


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
                 value: Union[Expression, InitializerList]):
        super().__init__()
        self.designators = designators  # List of struct or array designators
        self.value = value              # An expression or initializer list


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

    def __init__(self, operator: BinaryOperatorNode, condition: Expression,
                 true: Expression, false: Expression):
        super().__init__()
        self.operator = operator
        self.condition = condition
        self.true = true
        self.false = false


class BinaryExpression(Expression):
    """
    A binary operation consists of an operator and two operands.
    """

    def __init__(self, operator: BinaryOperatorNode, left: Expression,
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

    def __init__(self, operator: UnaryOperatorNode, operand: Expression):
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

    def __init__(self, operator: UnaryOperatorNode, operand: Expression):
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


class BinaryOperatorNode(Node):
    """
    Wrapper for a binary operator enum, to associate a token range with the
    operator.
    """

    def __init__(self, operator: BinaryOperator):
        super().__init__()
        self.operator = operator


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


class UnaryOperatorNode(Node):
    """
    Wrapper for a unary operator enum, to associate a token range with the
    operator.
    """

    def __init__(self, operator: UnaryOperator):
        super().__init__()
        self.operator = operator


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
