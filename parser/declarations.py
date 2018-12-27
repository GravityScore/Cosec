
# parser/declarations.py
# By Ben Anderson
# December 2018

from lexer import Tokens, Token
from err import Error


class Declaration:
    """
    A declaration defines a new symbol. It consists of a set of declaration
    specifiers followed by a declarator and an optional initialization
    expression.
    """

    def __init__(self, specifiers, declarator, initializer=None):
        """
        Create a new empty declaration.
        """
        self.specifiers = specifiers
        self.declarator = declarator
        self.initializer = initializer

    @staticmethod
    def parse(t: Tokens):
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

        :param t: The tokens list to parse the declarations from.
        :return:  An array of declarations.
        """
        # Parse a set of declaration specifiers
        specifiers = DeclarationSpecifiers()
        specifiers.parse(t)

        # Parse a list of declarators and initializers
        declarations = []
        while t.cur().type != Token.SEMICOLON:
            declarator = Declarator()
            declarator.parse(t)

            # Check for an optional initializer
            initializer = None
            if t.cur().type == Token.ASSIGN:
                t.next()
                # TODO: parse expression

            # Add a declaration
            declaration = Declaration(specifiers, declarator, initializer)
            declarations.append(declaration)

            # Check for a comma
            if t.cur().type == Token.COMMA:
                t.next()
            else:
                break

        # Skip the semicolon
        t.expect(Token.SEMICOLON)
        t.next()

        # Emit a warning if we don't have any declarators (the statement is
        # useless)
        if len(declarations) == 0:
            err = Error.from_token("useless declaration", specifiers.range)
            err.print()
        return declarations


class DeclarationSpecifiers:
    """
    There are 4 parts to a declaration specifier:
      * Storage class (typedef, extern, static, auto, or register)
      * Type specifiers (built in type, struct, union, enum, or typedef)
      * Any type qualifiers (const, restrict, volatile)
      * Any function specifiers (inline)
    """

    def __init__(self):
        """
        Creates a new empty set of declaration specifiers.
        """
        self.storage_class = None
        self.type_specifier = None
        self.type_qualifiers = set()
        self.function_specifiers = set()
        self.range = None

    def parse(self, t: Tokens, spec_qual_only=False):
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

        :param t:              The tokens list to parse the declaration
                               specifiers from.
        :param spec_qual_only: If True, only allow type specifiers and
                               qualifiers.
        :return:               The set of declaration specifiers.
        """
        start = t.cur()

        # Keep track of which type names we've seen
        type_names = []

        # Keep parsing tokens until we reach one that isn't valid
        while t.cur().type != Token.EOF:
            if t.cur().type in STORAGE_CLASSES:
                # Check we're not only allowing type specifiers and qualifiers
                if spec_qual_only:
                    desc = f"can't use '{t.cur().type}' here"
                    raise Error.from_token(desc, t.cur())

                # Check we haven't already got a storage class
                if self.storage_class is not None:
                    desc = f"can't have two storage classes"
                    raise Error.from_token(desc, t.cur())

                self.storage_class = t.cur().type
            elif t.cur().type in BUILT_IN_TYPE_NAMES:
                # Collect the type name
                type_names.append(t.cur().type)

                # Try match the accumulated type names to a built in type
                specifier = None
                for (candidate, specifications) in BUILT_IN_TYPES.items():
                    if sorted(type_names) in specifications:
                        specifier = candidate
                        break

                # Check we could find a matching type specifier
                if specifier is None:
                    raise Error.from_token("unknown type", t.cur())

                self.type_specifier = specifier
            elif t.cur().type in TYPE_QUALIFIERS:
                # Add another type qualifier
                self.type_qualifiers.add(t.cur().type)
            elif t.cur().type in FUNCTION_SPECIFIERS:
                # Check we're not only allowing type specifiers and qualifiers
                if spec_qual_only:
                    desc = f"can't use '{t.cur().type}' here"
                    raise Error.from_token(desc, t.cur())

                # Add another function specifier
                self.function_specifiers.add(t.cur().type)
            else:
                break
            t.next()

        # Check we actually parsed anything at all
        if self.type_specifier is None:
            raise Error.from_token("expected type", t.cur())

        self.range = t.combine(start, t.cur())


class StorageClass:
    """
    All storage classes that can occur in a set of declaration specifiers. These
    are only legal in a root declaration at the top level of a source code file.
    """
    TYPEDEF = "typedef"
    EXTERN = "extern"
    STATIC = "static"
    AUTO = "auto"
    REGISTER = "register"


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
    CONST = "const"
    RESTRICT = "restrict"
    VOLATILE = "volatile"


class FunctionSpecifier:
    """
    All function specifiers that can occur in a set of declaration specifiers.
    These are only legal in function declarations and definitions at the top
    level of a source code file.
    """
    INLINE = "inline"


class Declarator:
    """
    A declarator specifies the name of a variable being declared and some
    additional information about its type.
    """

    def __init__(self):
        """
        Creates a new, empty declarator.
        """
        # The first element is the inner-most part closest to the name token
        self.parts = []

        # This is None for an abstract declarator
        self.name = None

    def parse(self, t: Tokens):
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

        This function parses both named and abstract declarators. Note the old
        K&R style function definitions are not supported. You can find
        information on using 'static' and type qualifiers in array declarators
        here:

          https://stackoverflow.com/questions/17559631/what-are-those-strange-
          array-sizes-and-static-in-c99

        :param t: The tokens list to parse the declarator from.
        """
        self._parse_recursive(t)

        # Check we actually parsed something
        if len(self.parts) == 0 and self.name is None:
            raise Error.from_token("expected declarator", t.cur())

    def _parse_recursive(self, t: Tokens):
        """
        Parses a single part of a declarator (either an array, pointer, or
        function part).

        :param t: The tokens list to parse the declarator from.
        """
        # Parse a list of pointer parts
        pointers = []
        while t.cur().type == Token.MUL:
            pointer_part = DeclaratorPointerPart()
            pointer_part.parse(t)
            pointers.append(pointer_part)

        # Either an identifier or an open parenthesis can follow the pointer
        # parts
        if t.cur().type == Token.OPEN_PAREN:
            # Parse another declarator part recursively
            t.next()
            self._parse_recursive(t)

            # Expect a closing parenthesis
            t.expect(Token.CLOSE_PAREN)
            t.next()
        elif t.cur().type == Token.IDENT:
            # Found the symbol name that the declarator is declaring
            self.name = t.cur()
            t.next()

        # Parse a list of postfix array or function parts
        while True:
            if t.cur().type == Token.OPEN_PAREN:
                function_part = DeclaratorFunctionPart()
                function_part.parse(t)
                self.parts.append(function_part)
            elif t.cur().type == Token.OPEN_BRACKET:
                array_part = DeclaratorArrayPart()
                array_part.parse(t)
                self.parts.append(array_part)
            else:
                break

        # Append the pointer parts we parsed earlier, in reverse order
        pointers.reverse()
        self.parts += pointers


class DeclaratorArrayPart:
    """
    Information about an array part of a declarator. This includes an expression
    specifying the size of the array, as well information for implicit
    conversion to a pointer when used as a function argument.
    """

    def __init__(self):
        """
        Creates an empty array declarator part.
        """
        self.type_qualifiers = set()
        self.is_static = False
        self.is_vla = False  # Is variable length array
        self.size = None

    def parse(self, t: Tokens):
        """
        Parse an array declarator part from a tokens list.

        :param t: The tokens list to parse the declarator part from.
        """
        # Expect an opening bracket
        t.expect(Token.OPEN_BRACKET)
        t.next()

        # Check for static
        if t.cur().type == Token.STATIC:
            self.is_static = True
            t.next()

        # Check for type qualifiers
        while t.cur().type in TYPE_QUALIFIERS:
            self.type_qualifiers.add(t.cur().type)
            t.next()

        # Check for static again
        if t.cur().type == Token.STATIC:
            self.is_static = True
            t.next()

        # Check for a variable length array, or parse an expression
        if t.cur().type == Token.MUL and \
                t.peek(1).type == Token.CLOSE_BRACKET:
            self.is_vla = True
            t.next()  # The close bracket is consumed below
        elif t.cur().type != Token.CLOSE_BRACKET:
            pass  # TODO: parse expression

        # Expect a closing bracket
        t.expect(Token.CLOSE_BRACKET)
        t.next()


class DeclaratorFunctionPart:
    """
    Information about a function part of a declarator. This includes its list
    of arguments and their names and types.
    """

    def __init__(self):
        """
        Creates an empty function declarator part.
        """
        self.args = []

    def parse(self, t: Tokens):
        """
        Parse a function declarator part from a tokens list.

        :param t: The tokens list to parse the declarator part from.
        """
        # Expect an opening parenthesis
        t.expect(Token.OPEN_PAREN)
        t.next()

        # No arguments if the only thing in the parentheses is 'void'
        if t.cur().type == Token.VOID and t.peek(1).type == Token.CLOSE_PAREN:
            t.next()  # Skip 'void' and stop on the close parenthesis

        # Parse a list of argument declarations
        while t.cur().type != Token.CLOSE_PAREN:
            arg = DeclaratorFunctionArgument()

            # Parse a set of declaration specifiers
            arg.specifiers = DeclarationSpecifiers()
            arg.specifiers.parse(t)

            # Parse an optional declarator
            if t.cur().type != Token.CLOSE_PAREN and t.cur().type != Token.COMMA:
                arg.declarator = Declarator()
                arg.declarator.parse(t)

            # Add the argument
            self.args.append(arg)

            # Parse a comma
            if t.cur().type == Token.COMMA:
                t.next()
            else:
                break

        # Expect a close parenthesis
        t.expect(Token.CLOSE_PAREN)
        t.next()


class DeclaratorFunctionArgument:
    """
    Information about an argument in a function declaration.
    """

    def __init__(self):
        """
        Create a new, empty declarator function argument.
        """
        self.specifiers = None
        self.declarator = None


class DeclaratorPointerPart:
    """
    Information about a pointer part of a declarator. This includes any type
    qualifiers specified alongside the pointer.
    """

    def __init__(self):
        """
        Creates an empty pointer declarator part.
        """
        self.type_qualifiers = set()

    def parse(self, t: Tokens):
        """
        Parse a pointer declarator part from a tokens list.

        :param t: The tokens list to parse the declarator part from.
        """
        # Expect an asterisk
        t.expect(Token.MUL)
        t.next()

        # Parse a list of type qualifiers
        while t.cur().type in TYPE_QUALIFIERS:
            self.type_qualifiers.add(t.cur().type)
            t.next()


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
