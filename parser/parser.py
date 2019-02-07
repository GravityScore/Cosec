
# parser.py
# By Ben Anderson
# December 2018

from __future__ import annotations
from typing import Optional, Union

from parser.lexer import Tokens, Token, Tk
from parser.ast import DeclarationSpecifiers, TypeName, StorageClass, \
    StorageClassNode, TypeSpecifier, TypeSpecifierNode, TypeQualifier, \
    TypeQualifierNode, FunctionSpecifier, FunctionSpecifierNode, \
    StructSpecifier, UnionSpecifier, EnumSpecifier, EnumConst, Declarator, \
    DeclaratorPointerPart, DeclaratorFunctionPart, DeclaratorArrayPart, \
    DeclarationList, Declaration, InitializerList, Initializer, \
    StructDesignator, ArrayDesignator
from parser.ast import ExpressionList, Expression, TernaryExpression, \
    BinaryExpression, CastExpression, SizeofExpression, InitializerExpression, \
    UnaryExpression, PostfixExpression, ArrayAccessExpression, \
    FunctionCallExpression, FieldAccessExpression, SymbolExpression, \
    ConstantExpression, BinaryOperator, BinaryOperatorNode, UnaryOperator, \
    UnaryOperatorNode, Precedence
from parser.ast import FunctionDefinition, Statement, DeclarationStatement, \
    CompoundStatement, ExpressionStatement, IfStatementChain, IfStatement, \
    SwitchStatement, CaseStatement, DefaultStatement, WhileStatement, \
    DoWhileStatement, ForStatement, ContinueStatement, BreakStatement, \
    ReturnStatement, GotoStatement, LabelStatement
from error import Error


class Scope:
    """
    A scope stores a list of typedef identifiers so we can disambiguate between
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

        :param name: The symbol name to check.
        :return:     True if the name was used in a typedef in a parent scope.
        """
        # Iterate over the scopes in reverse order, so we search the inner-most
        # scope first
        for scope in reversed(self.scopes):
            if name in scope.typedefs:
                return True
        return False

    # **************************************************************************
    #     Root Node Parsing
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

    def parse_root_node(self) -> Union[DeclarationList, FunctionDefinition]:
        """
        Parse a root AST node, which occurs at the top level of the source code.
        This is either a function definition or declaration.

          external_declaration
              : function_definition
              | declaration

        :return: A root AST node.
        """
        # Find out if we're dealing with a function definition or a declaration,
        # so we can reuse the 'parse_declaration_list' function.
        saved = self.t.save()
        self.parse_declaration_specifiers()
        self.parse_declarator()
        after = self.t.cur().type
        self.t.restore(saved)

        # Depending on the discriminating token
        if after == Tk.OPEN_BRACE:
            return self.parse_function_definition()
        else:
            return self.parse_declaration_list()

    def parse_function_definition(self) -> FunctionDefinition:
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

        # Parse a block of statement
        block = self.parse_compound_statement()

        # Construct a function definition
        definition = FunctionDefinition(specifiers, declarator, block)

        # Set the definition's range to be its declaration (not its block)
        definition.range = range
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
            if initializer is not None:
                declaration.range = specifiers.range.merge(initializer.range)
            else:
                declaration.range = specifiers.range.merge(declarator.range)
            declarations.append(declaration)

            # Check if this declaration is a typedef
            if specifiers.storage_class is not None and \
                    specifiers.storage_class.type == StorageClass.TYPEDEF and \
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

        # Set the declaration list's range to be the union of all its
        # declaration's ranges
        list = DeclarationList(declarations)
        list.range = specifiers.range.merge(self.t.prev())
        return list

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
        start = self.t.cur()
        self.t.next()

        # Keep parsing initializers
        initializers = []
        while self.t.cur().type != Tk.CLOSE_BRACE:
            # Parse a designator list
            designators = self.parse_designators()

            # Parse the value for the initializer
            value = self.parse_initializer()

            # Create the initializer
            initializer = Initializer(designators, value)
            if len(designators) > 0:
                initializer.range = designators[0].range.merge(value.range)
            else:
                initializer.range = value.range
            initializers.append(initializer)

            # Parse a comma
            if self.t.cur().type == Tk.COMMA:
                self.t.next()
            else:
                break

        # Expect a closing brace
        self.t.expect(Tk.CLOSE_BRACE)
        self.t.next()

        # Set the list's range to include the opening and closing braces
        list = InitializerList(initializers)
        list.range = start.merge(self.t.prev())
        return list

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
            start = self.t.cur()
            self.t.next()

            # Depending on the type of the designator
            if start.type == Tk.OPEN_BRACKET:
                # Parse a constant expression
                expr = self.parse_expression_root()
                designator = ArrayDesignator(expr)

                # Expect a closing bracket
                self.t.expect(Tk.CLOSE_BRACKET)
            else:
                # Expect an identifier
                self.t.expect(Tk.IDENT)
                field = self.t.cur()
                designator = StructDesignator(field)

            # Skip the last token of the designator and set its range
            designator.range = start.merge(self.t.cur())
            self.t.next()

            # Add the designator
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

        This function differs from the above grammar, in that it doesn't insist
        the declarator be abstract. It's up to the calling function to check if
        the declarator is abstract or not (depending on what it wants).

        :return: The parsed type.
        """
        # Parse a set of declaration specifiers
        specifiers = self.parse_declaration_specifiers()

        # Parse an optional declarator
        declarator = None
        next = self.t.cur().type
        if next != Tk.CLOSE_PAREN and next != Tk.COMMA:
            declarator = self.parse_declarator()
        type = TypeName(specifiers, declarator)

        # Set the range
        if declarator is not None:
            type.range = specifiers.range.merge(declarator.range)
        else:
            type.range = specifiers.range
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
                | alignment_specifier declaration_specifiers   NOT SUPPORTED
                | alignment_specifier                          NOT SUPPORTED

        :return: The set of declaration specifiers.
        """
        start = self.t.cur()
        type_tokens = []

        # Keep parsing tokens until we reach one that isn't valid
        specifiers = DeclarationSpecifiers()
        while self.t.cur().type != Tk.EOF:
            if self.t.cur().type in STORAGE_CLASSES:
                self.parse_storage_class(specifiers)
            elif self.t.cur().type in TYPE_SPECIFIERS:
                type_tokens.append(self.t.cur().type)
                self.parse_type_specifier(specifiers, type_tokens)
            elif self.t.cur().type in TYPE_QUALIFIERS:
                self.parse_type_qualifier(specifiers)
            elif self.t.cur().type in FUNCTION_SPECIFIERS:
                self.parse_function_specifier(specifiers)
            elif self.t.cur().type == Tk.IDENT and \
                    self.find_typedef(self.t.cur().contents):
                self.parse_typedef_specifier(specifiers)
            elif self.t.cur().type == Tk.STRUCT:
                self.parse_struct_specifier(specifiers)
            elif self.t.cur().type == Tk.UNION:
                self.parse_union_specifier(specifiers)
            elif self.t.cur().type == Tk.ENUM:
                self.parse_enum_specifier(specifiers)
            else:
                # Token isn't a valid declaration specifier
                break

        # Check we had a type specifier
        if specifiers.type_specifier is None:
            raise Error("expected type name", self.t.cur())
        specifiers.range = start.merge(self.t.prev())
        return specifiers

    def parse_storage_class(self, specifiers: DeclarationSpecifiers):
        """
        Parses a storage class specifier within a set of declaration specifiers.
        The relevant grammar is:

            storage_class_specifier
                : TYPEDEF
                | EXTERN
                | STATIC
                | AUTO
                | THREAD_LOCAL               NOT SUPPORTED
                | REGISTER

        :param specifiers: The declaration specifiers to update.
        """
        # Check we haven't already got a storage class
        if specifiers.storage_class is not None:
            raise Error("can't have two storage classes", self.t.cur())

        # Create the storage class
        storage_class = StorageClass(self.t.cur().type)
        node = StorageClassNode(storage_class)
        node.range = self.t.cur()
        self.t.next()

        # Update the declaration specifiers
        specifiers.storage_class = node

    def parse_type_specifier(self, specifiers: DeclarationSpecifiers,
                             type_tokens: list):
        """
        Takes a list of type tokens (e.g. int, long, unsigned, etc.) and tries
        to find a type specifier that corresponds to these tokens. Triggers an
        error if it can't. The relevant grammar is:

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

        :param type_tokens: The list of type tokens to parse.
        :param specifiers:  The declaration specifiers to update.
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
            raise Error("can't combine type", self.t.cur())

        # Update the range
        node = TypeSpecifierNode(type_specifier)
        if specifiers.type_specifier is None:
            node.range = self.t.cur()
        else:
            node.range = specifiers.type_specifier.range.merge(self.t.cur())
        self.t.next()

        # Update the declaration specifiers
        specifiers.type_specifier = node

    def parse_type_qualifier(self, specifiers: DeclarationSpecifiers):
        """
        Parses a type qualifier as part of a set of declaration specifiers. The
        relevant grammar is:

            type_qualifier
                : CONST
                | RESTRICT
                | VOLATILE

        :param specifiers: The declaration specifiers to update.
        """
        # Create the type qualifier
        type_qualifier = TypeQualifier(self.t.cur().type)
        node = TypeQualifierNode(type_qualifier)
        node.range = self.t.cur()

        # Update the declaration specifiers
        specifiers.type_qualifiers.append(node)
        self.t.next()

    def parse_function_specifier(self, specifiers: DeclarationSpecifiers):
        """
        Parses a function specifier as part of a set of declaration specifiers.
        The relevant grammar is:

            function_specifier
                : INLINE

        :param specifiers: The declaration specifiers to update.
        """
        # Create the function specifier
        function_specifier = FunctionSpecifier(self.t.cur().type)
        node = FunctionSpecifierNode(function_specifier)
        node.range = self.t.cur()

        # Update the declaration specifiers
        specifiers.function_specifiers.append(node)
        self.t.next()

    def parse_typedef_specifier(self, specifiers: DeclarationSpecifiers):
        """
        Parses use of a typedef'd type within a series of declaration
        specifiers.

        :param specifiers: The declaration specifiers to update
        """
        # Check we don't have any other type specifiers
        if specifiers.type_specifier is not None:
            raise Error("can't combine type", self.t.cur())

        # Create the type specifier node
        type_specifier = TypeSpecifier.TYPEDEF
        node = TypeSpecifierNode(type_specifier)
        node.typedef_name = self.t.cur().contents
        node.range = self.t.cur()

        # Update the declaration specifiers
        specifiers.type_specifier = node
        self.t.next()

    def parse_struct_specifier(self, specifiers: DeclarationSpecifiers):
        """
        Parses a struct type specifier in a declaration. See
        'parse_struct_or_union' for the relevant grammar.

        :param specifiers: The declaration specifiers to update.
        """
        # Skip the 'struct' keyword
        self.t.expect(Tk.STRUCT)
        start = self.t.cur()
        self.t.next()

        # Parse the rest of the struct specifier
        name, fields = self.parse_struct_or_union()
        struct = StructSpecifier(name, fields)
        struct.range = start.merge(name)

        # Create the type specifier
        type_specifier = TypeSpecifier.STRUCT
        node = TypeSpecifierNode(type_specifier)
        node.struct = struct
        node.range = struct.range

        # Update the declaration specifiers
        specifiers.type_specifier = node

    def parse_union_specifier(self, specifiers: DeclarationSpecifiers):
        """
        Parses a union type specifier in a declaration. See
        'parse_struct_or_union' for the relevant grammar.

        :param specifiers: The declaration specifiers to update.
        """
        # Skip the 'union' keyword
        self.t.expect(Tk.UNION)
        start = self.t.cur()
        self.t.next()

        # Parse the rest of the union specifier
        name, fields = self.parse_struct_or_union()
        union = UnionSpecifier(name, fields)
        union.range = start.merge(name)

        # Create the type specifier
        type_specifier = TypeSpecifier.UNION
        node = TypeSpecifierNode(type_specifier)
        node.union = union
        node.range = union.range

        # Update the declaration specifiers
        specifiers.type_specifier = node

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
        # We've already parsed the 'struct' or 'union' keyword
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

    def parse_enum_specifier(self, specifiers: DeclarationSpecifiers):
        """
        Parses an enum type specifier in a declaration

        :param specifiers: The declaration specifiers to update.
        """
        # Check we don't have any other type specifiers
        if specifiers.type_specifier is not None:
            raise Error("can't combine type", self.t.cur())

        # Parse the enum specifier
        enum = self.parse_enum()
        type_specifier = TypeSpecifier.ENUM
        node = TypeSpecifierNode(type_specifier)
        node.enum = enum
        node.range = enum.range

        # Update the declaration specifiers
        specifiers.type_specifier = node

    def parse_enum(self) -> EnumSpecifier:
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
        start = self.t.cur()
        self.t.next()

        # Check for an identifier
        range = start
        name = None
        if self.t.cur().type == Tk.IDENT:
            name = self.t.cur()
            range = start.merge(name)
            self.t.next()

        # If we don't have an opening brace, then we need a name
        if self.t.cur().type != Tk.OPEN_BRACE:
            if name is None:
                raise Error("expected enum name", self.t.cur())
            enum = EnumSpecifier(name)
            enum.range = range
            return enum
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
        enum = EnumSpecifier(name, consts)
        enum.range = range
        return enum

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
        start = self.t.cur()
        declarator = Declarator()
        self.parse_declarator_recursive(declarator)

        # Check we actually parsed something
        if len(declarator.parts) == 0 and declarator.name is None:
            raise Error("expected declarator", self.t.cur())

        declarator.range = start.merge(self.t.prev())
        return declarator

    def parse_declarator_recursive(self, declarator: Declarator):
        """
        Parse a single part of a declarator (either an array, pointer, or
        function part).

        :param declarator: The resulting declarator.
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
        start = self.t.cur()
        self.t.next()

        # Parse a list of type qualifiers
        pointer = DeclaratorPointerPart()
        while self.t.cur().type in TYPE_QUALIFIERS:
            qualifier = TypeQualifier(self.t.cur().type)
            node = TypeQualifierNode(qualifier)
            node.range = self.t.cur()
            pointer.type_qualifiers.append(node)
            self.t.next()
        pointer.range = start.merge(self.t.prev())
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
        start = self.t.cur()
        self.t.next()

        # No arguments if the only thing in the parentheses is 'void'
        if self.t.cur().type == Tk.VOID and \
                self.t.peek(1).type == Tk.CLOSE_PAREN:
            self.t.next()  # Skip 'void' and stop on the close parenthesis

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
        function.range = start.merge(self.t.cur())
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

        # Expect an opening bracket
        self.t.expect(Tk.OPEN_BRACKET)
        start = self.t.cur()
        self.t.next()

        # Check for static
        if self.t.cur().type == Tk.STATIC:
            array.static = self.t.cur()
            self.t.next()

        # Check for type qualifiers
        while self.t.cur().type in TYPE_QUALIFIERS:
            qualifier = TypeQualifier(self.t.cur().type)
            node = TypeQualifierNode(qualifier)
            node.range = self.t.cur()
            array.type_qualifiers.append(node)
            self.t.next()

        # Check for static again
        if self.t.cur().type == Tk.STATIC:
            array.static = self.t.cur()
            self.t.next()

        # Check for a variable length array, or parse an expression
        if self.t.cur().type == Tk.MUL and \
                self.t.peek(1).type == Tk.CLOSE_BRACKET:
            array.vla = self.t.cur()
            self.t.next()  # The close bracket is consumed below
        elif self.t.cur().type != Tk.CLOSE_BRACKET:
            array.size = self.parse_expression()
            pass

        # Expect a closing bracket
        self.t.expect(Tk.CLOSE_BRACKET)
        array.range = start.merge(self.t.cur())
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
        start = self.t.cur()
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
        stmt = CompoundStatement(statements)
        stmt.range = start
        return stmt

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
        elif self.t.cur().type in TYPE_SPECIFIERS:
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
        stmt = LabelStatement(name)

        # Expect a colon
        self.t.expect(Tk.COLON)
        stmt.range = name.merge(self.t.cur())
        self.t.next()
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Parse the following expression
        condition = self.parse_expression_root()

        # Expect a colon
        self.t.expect(Tk.COLON)
        self.t.next()

        stmt = CaseStatement(condition)
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect a colon
        self.t.expect(Tk.COLON)
        self.t.next()

        stmt = DefaultStatement()
        stmt.range = start
        return stmt

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

        # Create an AST node to wrap the chain
        stmt = IfStatementChain(chain)
        stmt.range = chain[0].range
        return stmt

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
        stmt = IfStatement(condition, body)
        stmt.range = type
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect an expression surrounded by parentheses
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()
        condition = self.parse_expression()
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()

        # Expect a statement
        body = self.parse_statement()
        stmt = SwitchStatement(condition, body)
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect an expression surrounded by parentheses
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()
        condition = self.parse_expression()
        self.t.expect(Tk.CLOSE_PAREN)
        self.t.next()

        # Expect a statement
        body = self.parse_statement()
        stmt = WhileStatement(condition, body)
        stmt.range = start
        return stmt

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
        start = self.t.cur()
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
        stmt = DoWhileStatement(condition, body)
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect an open parenthesis
        self.t.expect(Tk.OPEN_PAREN)
        self.t.next()

        # Check if we've got a declaration or an expression
        initializer = None
        if self.t.cur().type in TYPE_SPECIFIERS:
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
        stmt = ForStatement(initializer, condition, increment, body)
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect an identifier
        self.t.expect(Tk.IDENT)
        name = self.t.cur()
        self.t.next()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        stmt = GotoStatement(name)
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        stmt = ContinueStatement()
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        stmt = BreakStatement()
        stmt.range = start
        return stmt

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
        start = self.t.cur()
        self.t.next()

        # Check for an expression
        result = None
        if self.t.cur().type != Tk.SEMICOLON:
            result = self.parse_expression()

        # Expect a semicolon
        self.t.expect(Tk.SEMICOLON)
        self.t.next()
        stmt = ReturnStatement(result)
        stmt.range = start
        return stmt

    def parse_declaration_statement(self) -> DeclarationStatement:
        """
        Parse a declaration statement. The relevant grammar is:

            block_item
                : declaration
                ...

        :return: A declaration statement.
        """
        list = self.parse_declaration_list()
        stmt = DeclarationStatement(list)
        last = list.declarations[-1].range
        stmt.range = list.declarations[0].range.merge(last)
        return stmt

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
        stmt = ExpressionStatement(expression)
        stmt.range = expression.range
        return stmt

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
            list = ExpressionList(expressions)
            list.range = expressions[0].range.merge(expressions[-1].range)
            return list

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
            operator_node = BinaryOperatorNode(operator)
            operator_node.range = self.t.cur()

            # Check the precedence is greater than the minimum
            precedence = PRECEDENCES[operator]
            if precedence.value <= min_prec.value:
                break
            self.t.next()

            # Check for a ternary operator
            if operator == BinaryOperator.TERNARY:
                left = self.parse_ternary_expression(operator_node, left)
            else:
                # Parse the right operand
                right = self.parse_expression_recursive(precedence)

                # Construct a binary operation
                temp = BinaryExpression(operator_node, left, right)
                temp.range = left.range.merge(right.range)
                left = temp
        return left

    def parse_ternary_expression(self, operator: BinaryOperatorNode,
                                 condition: Expression) -> Expression:
        """
        Parse a ternary expression. The relevant grammar is:

            conditional_expression
                : logical_or_expression
                | logical_or_expression '?' expression ':'
                    conditional_expression

        :param operator:  The ternary operator expression node.
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
        expr = TernaryExpression(operator, condition, true, false)
        expr.range = condition.range.merge(false.range)
        return expr

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
        start = self.t.cur()

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
                token = type.declarator.name
                raise Error("expected abstract declarator", token)

            # Parse the unary expression that follows
            operand = self.parse_sizeof_expression()
            expr = CastExpression(type.specifiers, type.declarator, operand)
            expr.range = start.merge(operand.range)
            return expr

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
        start = self.t.cur()
        self.t.next()

        # Try parsing a type in parentheses
        if self.t.cur().type == Tk.OPEN_PAREN:
            type = self.try_parse_type_name()
            if type is not None:
                expr = SizeofExpression(type.specifiers, type.declarator)
                expr.range = start.merge(self.t.prev())
                return expr

        # Otherwise, expect an expression (don't allow casting of the operand)
        operator_node = UnaryOperatorNode(UnaryOperator.SIZEOF)
        operator_node.range = start
        operand = self.parse_unary_expression()
        expr = UnaryExpression(operator_node, operand)
        expr.range = start.merge(operand.range)
        return expr

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
        operator_node = UnaryOperatorNode(operator)
        operator_node.range = self.t.cur()
        self.t.next()

        # Parse the operand
        if operator == UnaryOperator.INC or operator == UnaryOperator.DEC:
            # Don't allow casting for prefix operators
            operand = self.parse_unary_expression()
        else:
            operand = self.parse_cast_expression()
        expr = UnaryExpression(operator_node, operand)
        expr.range = operator_node.range.merge(operand.range)
        return expr

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
        start = self.t.cur()
        type = self.try_parse_type_name()
        if type is not None:
            initializer_list = self.parse_initializer_list()
            expr = InitializerExpression(type, initializer_list)
            expr.range = start.merge(initializer_list.range)
            return expr

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
                operator_node = UnaryOperatorNode(operator)
                operator_node.range = self.t.cur()
                self.t.next()
                result = PostfixExpression(operator_node, result)
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

        expr = ArrayAccessExpression(array, index)
        expr.range = array.range.merge(self.t.prev())
        return expr

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

        expr = FunctionCallExpression(func, args)
        expr.range = func.range.merge(self.t.prev())
        return expr

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
        access.range = struct.range.merge(self.t.cur())
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
        arrow = self.t.cur()
        self.t.next()

        # Parse the field to access
        self.t.expect(Tk.IDENT)
        field = self.t.cur()
        self.t.next()

        # Dereference the struct first, then access a field on it
        node = UnaryOperatorNode(UnaryOperator.DEREF)
        deref = UnaryExpression(node, struct)
        access = FieldAccessExpression(deref, field)
        node.range = arrow
        deref.range = struct.range.merge(arrow)
        access.range = deref.range.merge(field)
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
            result.range = self.t.cur()
            self.t.next()
        elif type == Tk.CONST_INT or type == Tk.CONST_FLOAT or \
                type == Tk.CONST_CHAR or type == Tk.CONST_STR:
            # Parse a constant
            result = ConstantExpression(self.t.cur())
            result.range = self.t.cur()
            self.t.next()
        elif type == Tk.OPEN_PAREN:
            # Skip the open parenthesis
            start = self.t.cur()
            self.t.next()

            # Parse the expression contained in the parentheses
            result = self.parse_expression()

            # Expect a closing parenthesis
            self.t.expect(Tk.CLOSE_PAREN)
            result.range = start.merge(self.t.cur())
            self.t.next()
        else:
            # Expected expression error
            raise Error("expected expression", self.t.cur())
        return result


"""
A list of all built-in type names.
"""
TYPE_SPECIFIERS = {
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
