
# ast.py
# By Ben Anderson
# December 2018

from err import Error


class AstGenerator:
    """
    The AST generator builds an abstract syntax tree (AST) from a token
    sequence. An AST is a high-level representation of a C program, structured
    as a tree.
    """

    def __init__(self, seq):
        """
        Create an AST generator.
        :param seq: The token sequence to build the AST from.
        """
        self.seq = seq

    def gen(self):
        """
        Build an AST from a token sequence.
        :return: The AST root.
        """
        pass

    def parse_declarator(self):
        """
        Parse a declarator. The relevant grammar is:
            declarator: pointer direct_declarator | direct_declarator ;

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

        Constraints:
        * Every declarator defines an identifier
        * The identifier is unique within its scope

        This function also parses abstract declarators. The 'name' field on the
        returned declarator is None when we've parsed an abstract declarator.

        You can find details on using "static" and type qualifiers within array
        expressions here:
            https://stackoverflow.com/questions/17559631/what-are-those-strange-
            array-sizes-and-static-in-c99

        :return: A declarator.
        """
        declarator = Declarator()
        self.parse_declarator_recursive(declarator)

        # Ensure we actually have a declarator
        if declarator.name is None and len(declarator.stack) == 0:
            raise Error.from_tk("expected declarator", self.seq.cur())

        # Ensure the declarator satisfies the above constraints
        # TODO: e.g. static and cvr can only occur on outermost arrays in
        # function parameters (read the standard)
        return declarator

    def parse_declarator_recursive(self, declarator):
        """
        A great article on declarator parsing describing the approach used here:
        http://blog.robertelder.org/building-a-c-compiler-type-system-the-
        formidable-declarator/
        """
        # Check for a pointer
        pointers = []
        while self.seq.cur().type == "*":
            pointers.append(self.parse_pointer())

        # Either we have an identifier or an open parenthesis
        if self.seq.cur().type == "(":
            self.seq.next()

            # Parse another declarator recursively
            self.parse_declarator_recursive(declarator)

            # Expect a closing parenthesis
            self.seq.expect(")")
            self.seq.next()
        elif self.seq.cur().type == "identifier":
            declarator.name = self.seq.cur()
            self.seq.next()

        # Check for any number of postfix array or function parts
        while self.seq.cur() is not None and (self.seq.cur().type == "(" or
                                              self.seq.cur().type == "["):
            if self.seq.cur().type == "(":
                declarator.stack.append(self.parse_declarator_function_part())
            elif self.seq.cur().type == "[":
                declarator.stack.append(self.parse_declarator_array_part())

        # The pointer part comes after arrays or functions, so that 'int *foo()'
        # becomes a function that returns a pointer to an int, and not a pointer
        # to a function that returns an int (which would be 'int (*foo)()').
        # We need to append the pointers in the reverse order that they were
        # parsed
        pointers.reverse()
        declarator.stack += pointers

    def parse_pointer(self):
        """
        Parses a pointer and any subsequent type qualifiers, returning the set
        of type qualifiers. Relevant grammar:

            pointer
                : '*' type_qualifiers pointer
                | '*' type_qualifiers
                | '*' pointer
                | '*'

        :return: A set of type qualifiers following a pointer
        """
        # Check if we've actually got a pointer
        if self.seq.cur().type != "*":
            return None
        self.seq.next()

        # Check for type qualifiers after the asterisk
        pointer = DeclaratorPointerPart()
        pointer.type_qualifiers = self.parse_type_qualifiers()
        return pointer

    def parse_type_qualifiers(self):
        """
        Parses a set of type qualifiers.
        :return: A set of type qualifiers (no duplicates, meaningless order)
        """
        type_qualifiers = set()
        while self.seq.cur() is not None:
            qualifier = self.seq.cur().type
            if qualifier in ["const", "restrict", "volatile"]:
                type_qualifiers.add(qualifier)
                self.seq.next()
            else:
                break
        return type_qualifiers

    def parse_declarator_function_part(self):
        """
        Parses a function part of a declarator. Relevant grammar:

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

        The identifier list (the old K&R style method of a function definition)
        is not supported.

        :return: A function part of a declarator.
        """
        # Expect the terminating parenthesis
        self.seq.expect("(")
        self.seq.next()

        # Keep parsing arguments
        fn_part = DeclaratorFunctionPart()
        while self.seq.cur().type != ")":
            # Parse the declaration specifiers
            specifiers = self.parse_declaration_specifiers()

            # The declarator is optional
            declarator = None
            if self.seq.cur().type != ")" and self.seq.cur().type != ",":
                declarator = self.parse_declarator()

            # Add the argument
            arg = Declaration()
            arg.specifiers = specifiers
            arg.declarator = declarator
            fn_part.args.append(arg)

            # Check for a continuing comma
            if self.seq.cur().type != ")":
                self.seq.expect(",")
                self.seq.next()

        # Expect the terminating parenthesis
        self.seq.expect(")")
        self.seq.next()
        return fn_part

    def parse_declarator_array_part(self):
        """
        Parses an array part of a declarator. Relevant grammar:

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

        :return: An array part of a declarator.
        """
        # Expect an opening brace
        self.seq.expect("[")
        self.seq.next()

        # Check for static
        array_part = DeclaratorArrayPart()
        if self.seq.cur().type == "static":
            array_part.static = True
            self.seq.next()

        # Check for type qualifiers
        array_part.type_qualifiers = self.parse_type_qualifiers()

        # Check for static again (it can be either before or after the type
        # qualifiers, according to the standard)
        if self.seq.cur().type == "static":
            array_part.static = True
            self.seq.next()

        # The expression might consist of a single asterisk and a closing brace
        # to indicate a variable length array
        if self.seq.cur().type == "*" and self.seq.peek(1) is not None \
                and self.seq.peek(1).type == "]":
            array_part.vla = True
            self.seq.next()
        elif self.seq.cur().type != "]":
            # Parse a size expression
            array_part.size_expr = self.parse_expression()

        # Expect the closing brace
        self.seq.expect("]")
        self.seq.next()
        return array_part

    def parse_declaration_specifiers(self, specifier_qualifier_only=False):
        """
        Parse a series of declaration specifiers. The relevant grammar is:
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
        all no-ops, the remaining trigger compiler errors.

        Constraints:
        * Maximum 1 storage class specifier
        * At least one type specifier
        * Signed/unsigned comes first in the type specifier
        * Function specifiers only used in the declaration of a function

        :return: The declaration specifiers.
        """
        specifiers = DeclarationSpecifiers()
        type_specifiers = []
        while self.seq.cur() is not None:
            specifier = self.seq.cur()

            # ---- Storage class (typedef, extern, static, auto, register)
            if specifier.type in {"typedef", "extern", "static", "auto",
                                  "register"}:
                # Check we're not only allowing type specifiers and qualifiers
                if specifier_qualifier_only:
                    raise Error.from_tk("cannot use storage class here",
                                        specifier)

                # Check we haven't already got a storage class
                if specifiers.storage_class is not None:
                    raise Error.from_tk("cannot specify more than one "
                                                "storage class", specifier)
                specifiers.storage_class = specifier.type
                self.seq.next()

            # ---- Type specifier (built in, struct, enum, typedef)
            elif specifier.type in {"void", "char", "short", "int", "long",
                                    "float", "double", "signed", "unsigned"}:
                # Add the type specifier
                type_specifiers.append(specifier.type)

                # Check that the type specifier list is equal to one of the
                # lists in the dictionary above, ignoring order
                type = None
                for (candidate_type, specifications) in BUILT_IN_TYPES.items():
                    if sorted(type_specifiers) in specifications:
                        type = candidate_type
                        break

                # Check the type specifier list actually matched one of the
                # above lists
                if type is None:
                    raise Error.from_tk("cannot combine type specifier",
                                        specifier)
                specifiers.type_specifier = type
                self.seq.next()

            # ---- Type qualifier (const, restrict, volatile)
            elif specifier.type in {"const", "restrict", "volatile"}:
                specifiers.type_qualifiers.add(specifier.type)
                self.seq.next()

            # ---- Function specifier
            elif specifier.type in {"inline"}:
                # Check we're not only allowing type specifiers and qualifiers
                if specifier_qualifier_only:
                    raise Error.from_tk("cannot use function specifier "
                                                "here", specifier)
                specifiers.function_specifiers.add(specifier.type)
                self.seq.next()
            else:
                break

        # We require at least one type specifier
        if len(type_specifiers) == 0:
            raise Error.from_tk("expected declaration specifier",
                                self.seq.cur())
        return specifiers

    def parse_expression(self):
        """
        An expression consists of a series of operators acting on operands. The
        relevant grammar is:

            expression
                : assignment_expression
                | expression ',' assignment_expression

        :return: The root node of an expression tree.
        """
        # Parse a series of expressions separated by commas, but only keep the
        # result from the last one
        while True:
            # Use 0 as the minimum precedence to start with to include ALL
            # binary operators
            expr = self.parse_expression_recursive(0)
            if self.seq.cur().type == ",":
                self.seq.next()
            else:
                break
        return expr

    def parse_expression_recursive(self, min_precedence):
        """
        We use a Pratt parser to parse expressions. You can find more details
        here:
            http://journal.stuffwithstuff.com/200/03/19/pratt-parsers
            -expression-parsing-made-easy/
        :return: An expression tree.
        """
        # Left operand
        left = self.parse_cast_expression()

        # Continually parse binary operators that have precedence greater than
        # the minimum precedence
        while self.seq.cur().type in PRECEDENCE:
            # Parse the next binary operator and check it has precedence greater
            # than the minimum precedence
            operator = self.seq.cur()
            precedence = PRECEDENCE[self.seq.cur().type]
            if precedence <= min_precedence:
                break
            self.seq.next()

            # Parse the right operand (a subexpression containing all operators
            # with precedence above the precedence of the current operator)
            right = self.parse_expression_recursive(precedence)

            # Construct an expression tree from the operands and binary operator
            left = BinaryOperation(operator, left, right)
        return left

    def parse_cast_expression(self):
        """
        The relevant grammar is:

            cast_expression
                : unary_expression
                | '(' type_name ')' cast_expression

        :return: An expression tree.
        """
        # An open parenthesis here could indicate either a sub-expression or a
        # type cast. We conservatively attempt to parse a type cast here, and if
        # that fails we treat the contents of the parenthesis as a
        # sub-expression instead
        if self.seq.cur().type == "(":
            specifiers = self.try_parse_type_name()
            if specifiers is None:
                # Failed to parse a type cast
                return self.parse_unary_expression()
            else:
                # Parsed a type cast. Parse the rest of the expression
                subexpr = self.parse_cast_expression()
                return Cast(specifiers, subexpr)
        else:
            return self.parse_unary_expression()

    def try_parse_type_name(self):
        """
        Tries to parse a type name surrounded by parentheses. If successful,
        then returns a 'DeclarationSpecifiers' object, and otherwise returns
        None.
        :return: The parsed declaration specifiers, or None.
        """
        # Save the current location and skip the opening parenthesis
        saved = self.seq.save()
        self.seq.expect("(")
        self.seq.next()

        # Try to parse a type name (use the declaration specifier parsing
        # code, but only allow type specifiers and qualifiers)
        try:
            specifiers = self.parse_declaration_specifiers(
                specifier_qualifier_only=True)
        except Error:
            # We failed to parse a type specifier, so try a unary
            # expression. Return to the saved location
            self.seq.restore(saved)
            return None

        # Successfully parsed a type name
        self.seq.expect(")")
        self.seq.next()
        return specifiers

    def parse_unary_expression(self):
        """
        A unary expression is one with some unary operator out the front, e.g.
        '-a' or '!a'. The relevant grammar is:

            unary_expression
                : postfix_expression
                | INC_OP unary_expression
                | DEC_OP unary_expression
                | unary_operator cast_expression
                | SIZEOF unary_expression
                | SIZEOF '(' type_name ')'

            unary_operator: '&' | '*' | '+' | '-' | '~' | '!'

        :return: An expression tree.
        """
        if self.seq.cur().type in UNARY_OPERATORS:
            # Parse the unary operation
            operator = self.seq.cur().type
            self.seq.next()
            operand = self.parse_cast_expression()
            return UnaryOperation(operator, operand)
        elif self.seq.cur().type in {"++", "--", "sizeof"}:
            # Parse the unary operation, but without the ability to cast the
            # sub-expression
            operator = self.seq.cur().type
            self.seq.next()
            operand = self.parse_unary_expression()
            return UnaryOperation(operator, operand)
        else:
            return self.parse_postfix_expression()

    def parse_postfix_expression(self):
        """
        A postfix expression is one with an array index, function call, or
        struct field access as a postfix operator. The relevant grammar is:

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

        :return: An expression tree
        """
        # Try parsing a type cast again
        specifiers = self.try_parse_type_name()
        if specifiers is not None:
            # Expect an initializer list
            assert False  # Unimplemented

        # Parse a primary expression
        tree = self.parse_primary_expression()

        # Continually parse postfix expressions
        while True:
            if self.seq.cur().type == "[":
                tree = self.parse_array_access(tree)
            elif self.seq.cur().type == "(":
                tree = self.parse_function_call(tree)
            elif self.seq.cur().type == ".":
                tree = self.parse_field_access(tree)
            elif self.seq.cur().type == "->":
                tree = self.parse_deref_field_access(tree)
            elif self.seq.cur().type in {"++", "--"}:
                tree = PostfixOperation(self.seq.cur().type, tree)
                self.seq.next()
            else:
                break
        return tree

    def parse_array_access(self, tree):
        """
        Parses an array access as a postfix operator.
        :param tree: The array to access.
        :return: An expression tree.
        """
        # Parse opening bracket
        self.seq.expect("[")
        self.seq.next()

        # Parse contents of brackets (the index of the array to access)
        index = self.parse_expression()

        # Parse closing bracket
        self.seq.expect("]")
        self.seq.next()
        return ArrayAccess(tree, index)

    def parse_function_call(self, tree):
        """
        Parses a function call as a postfix operator.
        :param tree: The function to call.
        :return: An expression tree.
        """
        # Parse opening parenthesis
        self.seq.expect("(")
        self.seq.next()

        # Parse arguments
        args = []
        while self.seq.cur().type != ")":
            # Parse an argument
            args.append(self.parse_expression_recursive(0))

            # Expect a comma if we haven't reached the end of the arguments
            if self.seq.cur().type != ")":
                self.seq.expect(",")
                self.seq.next()

        # Closing parenthesis
        self.seq.expect(")")
        self.seq.next()
        return FunctionCall(tree, args)

    def parse_field_access(self, tree):
        """
        Parses a struct field access without de-referencing (i.e. accessing with
        '.')
        :param tree: The struct to access a field on.
        :return: An expression tree.
        """
        # Parse the dot
        self.seq.expect(".")
        self.seq.next()

        # Parse the field to access
        self.seq.expect("identifier")
        access = FieldAccess(tree, self.seq.cur())
        self.seq.next()
        return access

    def parse_deref_field_access(self, tree):
        """
        Parses a struct field access with de-referencing (i.e. accessing with
        '->')
        :param tree: The pointer to the struct to access a field on.
        :return: An expression tree.
        """
        # Parse the '->'
        self.seq.expect("->")
        self.seq.next()

        # Parse the field to access
        self.seq.expect("identifier")
        access = FieldAccess(UnaryOperation("*", tree), self.seq.cur())
        self.seq.next()
        return access

    def parse_primary_expression(self):
        """
        A primary expression is a symbol or constant, or another sub-expression.
        The relevant grammar is:

            primary_expression
                : IDENTIFIER
                | constant
                | string
                | '(' expression ')'

            constant
                : INTEGER_CONSTANT
                | FLOAT_CONSTANT
                | ENUMERATION_CONSTANT

        :return: An expression node.
        """
        if self.seq.cur().type == "identifier":
            return self.parse_symbol()
        elif self.seq.cur().type == "number":
            return self.parse_constant()
        elif self.seq.cur().type == "(":
            return self.parse_subexpression()

    def parse_symbol(self):
        """
        Parses a symbol in an expression.
        :return: An expression node.
        """
        self.seq.expect("identifier")
        return Symbol(self.seq.cur())

    def parse_constant(self):
        """
        Parses a constant in an expression
        :return: An expression node.
        """
        # Numbers are the only supported constants, for now
        self.seq.expect("number")
        return Constant(self.seq.cur())

    def parse_subexpression(self):
        """
        Parses a subexpression, contained in parentheses.
        :return: An expression tree.
        """
        # Opening parenthesis
        self.seq.expect("(")
        self.seq.next()

        # Sub-expression
        subexpr = self.parse_expression()

        # Closing parenthesis
        self.seq.expect(")")
        self.seq.next()
        return subexpr


class Declaration:
    """
    A declaration is a set of declaration specifiers and a declarator. Note that
    this class allows the declarator to be abstract (i.e. not have an associated
    name/identifier). This should be guarded against when defining variables.
    """

    def __init__(self):
        self.specifiers = None
        # This may be None when parsing an abstract declarator (e.g. in function
        # declaration arguments)
        self.declarator = None


class Declarator:
    """
    A declarator can be stored as a "stack" of parts. There are array parts,
    pointer parts, and function parts. For example,
        int * (*(*(*foo)(char))(int))[3]
    The declaration specifiers consists of just "int". The remaining is the
    declarator. We have a pointer to a function "foo" (taking a char) that
    returns a pointer to a function (taking an int) that returns a pointer to
    an array of 3 pointers to ints. Read as a stack from the inside out:
        1) pointer  2) function (char)  3) pointer  4) function (int)
        5) pointer  6) array [3]  7) pointer
    You can find more details on the implementation here:
        http://blog.robertelder.org/building-a-c-compiler-type-system-the-
        formidable-declarator/
    """

    def __init__(self):
        # The FIRST element in the stack is the inner-most type adjacent to the
        # identifier
        self.stack = []

        # 'name' is None for an abstract declarator
        self.name = None


class DeclaratorPointerPart:
    """
    A pointer part in a declarator's stack. Only has to keep track of the type
    qualifiers that describe the pointer.
    """

    def __init__(self):
        self.type_qualifiers = set()


class DeclaratorFunctionPart:
    """
    A function part in a declarator's stack. Has to keep track of the function's
    arguments (which are themselves declarations with declaration specifiers and
    other declarators).
    """

    def __init__(self):
        self.args = []


class DeclaratorArrayPart:
    """
    An array part in a declarator's stack. Has to keep track of the array size.
    If the array size depends on other local variables (and is not a constant
    expression), then the array is a variable length array.
    """

    def __init__(self):
        self.static = False
        self.type_qualifiers = set()
        self.vla = False  # Variable length array, indicated explicitly by '*'
        self.size_expr = None


class DeclarationSpecifiers:
    """
    There are 4 parts to a declaration specifier: the storage class (a typedef,
    extern, static, auto, or register), the type specifiers (a built in type,
    struct, union, enum, or typedef), any type qualifiers (const, restrict,
    volatile, atomic), and any function specifiers (inline).
    """

    def __init__(self):
        self.storage_class = None
        self.type_specifier = None
        self.type_qualifiers = set()
        self.function_specifiers = set()


class StorageClass:
    TYPEDEF = "typedef"    # Specifies a typedef'd type
    EXTERN = "extern"      # Specifies external linkage
    STATIC = "static"      # Specifies internal linkage
    AUTO = "auto"          # Does nothing (for now)
    REGISTER = "register"  # Does nothing (for now)


class BuiltInType:
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


class TypeQualifier:
    CONST = "const"        # Specifies a value or pointer is constant
    RESTRICT = "restrict"  # Does nothing (for now)
    VOLATILE = "volatile"  # Does nothing (for now)


class FunctionSpecifier:
    INLINE = "inline"  # Specifies an inline function


# All possible ways of specifying a particular built-in type. These must be in
# ALPHABETICAL ORDER (sorted)
BUILT_IN_TYPES = {
    "void": [["void"]],
    "char": [["char"], ["char", "signed"]],
    "uchar": [["char", "unsigned"]],
    "short": [["short"], ["short", "signed"],
              ["int", "short", "signed"]],
    "ushort": [["short", "unsigned"], ["int", "short", "unsigned"]],
    "int": [["int"], ["signed"], ["int", "signed"], ["long"],
            ["long", "signed"], ["int", "long"],
            ["int", "long", "signed"]],
    "uint": [["unsigned"], ["int", "unsigned"], ["long", "unsigned"],
             ["int", "long", "unsigned"]],
    "llong": [["long", "long"], ["long", "long", "signed"],
              ["int", "long", "long"],
              ["int", "long", "long", "signed"]],
    "ullong": [["long", "long", "unsigned"],
               ["int", "long", "long", "unsigned"]],
    "float": [["float"]],
    "double": [["double"], ["double", "long"]],
}


class ExpressionNode:
    """
    Every node in an expression tree subclasses this.
    """
    pass


class BinaryOperation(ExpressionNode):
    """
    Stores the operator and two operands for a binary operation in an
    expression.
    """

    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right


class UnaryOperation(ExpressionNode):
    """
    Stores information about a unary operation, including its operator and
    single operand.
    """

    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand


class ArrayAccess(ExpressionNode):
    """
    Stores information about an array access.
    """

    def __init__(self, array, index_expr):
        self.array = array
        self.index = index_expr


class FunctionCall(ExpressionNode):
    """
    Stores information about a function call.
    """

    def __init__(self, function, args):
        self.function = function
        self.args = args


class FieldAccess(ExpressionNode):
    """
    Stores information about a struct field access.
    """

    def __init__(self, struct, field):
        self.struct = struct
        self.field = field  # An lexer token with type 'identifier'


class PostfixOperation(ExpressionNode):
    """
    Stores information about a postfix operation. These include
    """

    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand


class Cast(ExpressionNode):
    """
    Stores information about a type cast expression.
    """

    def __init__(self, cast_type, operand):
        self.cast_type = cast_type
        self.operand = operand


class Symbol(ExpressionNode):
    """
    Symbol stores the name of a symbol used in an expression. It could refer to
    a variable defined within the function, a global, another function, or an
    external symbol defined in another compilation unit.
    """

    def __init__(self, name):
        self.name = name


class Constant(ExpressionNode):
    """
    Stores the contents of a constant used in an expression. It could refer to
    either a number or a string.
    """

    def __init__(self, value):
        self.value = value  # A lexer token


# The precedence of all binary operators. Higher precedence means they bind more
# tightly to primary expressions.
PRECEDENCE = {
    "=": 1, "*=": 1, "/=": 1, "%=": 1, "+=": 1, "-=": 1, ">>=": 1, "<<=": 1,
    "&=": 1, "^=": 1, "|=": 1,         # Assignment expression
    "?": 2,                            # Ternary expression
    "||": 3,                           # Logical OR expression
    "&&": 4,                           # Logical AND expression
    "|": 5,                            # Bitwise OR expression
    "^": 6,                            # Bitwise XOR expression
    "&": 7,                            # Bitwise AND expression
    "==": 8, "!=": 8,                  # Equality expression
    "<": 9, ">": 9, "<=": 9, ">=": 9,  # Relational expression
    "<<": 10, ">>": 10,                # Shift expression
    "+": 11, "-": 11,                  # Additive expression
    "*": 12, "/": 12, "%": 12,         # Multiplicative expression
}

# A list of all unary operators (order is insignificant).
UNARY_OPERATORS = {"&", "*", "+", "-", "~", "!"}
