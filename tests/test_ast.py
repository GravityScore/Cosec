
# test_ast.py
# By Ben Anderson
# December 2018

from unittest import TestCase
from lexer import TokenSequence
from ast import AstGenerator, DeclaratorPointerPart, DeclaratorFunctionPart, \
    DeclaratorArrayPart


class TestAstGenerator(TestCase):
    def assert_declaration_specifiers(self, specifiers, storage_class,
                                      type_specifier, type_qualifiers=set(),
                                      function_specifiers=set()):
        self.assertEqual(specifiers.storage_class, storage_class)
        self.assertEqual(specifiers.type_specifier, type_specifier)
        self.assertEqual(specifiers.type_qualifiers, type_qualifiers)
        self.assertEqual(specifiers.function_specifiers, function_specifiers)

    def test_declaration_specifiers(self):
        ast = AstGenerator(TokenSequence("", "void"))
        spec = ast.parse_declaration_specifiers()
        self.assert_declaration_specifiers(spec, None, "void")

        ast = AstGenerator(TokenSequence("", "unsigned"))
        spec = ast.parse_declaration_specifiers()
        self.assert_declaration_specifiers(spec, None, "uint")

        ast = AstGenerator(TokenSequence("", "unsigned short int"))
        spec = ast.parse_declaration_specifiers()
        self.assert_declaration_specifiers(spec, None, "ushort")

        ast = AstGenerator(TokenSequence("", "extern int unsigned"))
        spec = ast.parse_declaration_specifiers()
        self.assert_declaration_specifiers(spec, "extern", "uint")

        ast = AstGenerator(TokenSequence("", "long static int long inline"))
        spec = ast.parse_declaration_specifiers()
        self.assert_declaration_specifiers(spec, "static", "llong", set(),
                                           {"inline"})

        ast = AstGenerator(TokenSequence("", "int const restrict"))
        spec = ast.parse_declaration_specifiers()
        self.assert_declaration_specifiers(spec, None, "int",
                                           {"const", "restrict"}, set())

    def test_pointer(self):
        ast = AstGenerator(TokenSequence("", "*"))
        ptr = ast.parse_pointer()
        self.assertTrue(ptr is not None)
        self.assertEqual(ptr.type_qualifiers, set())

        ast = AstGenerator(TokenSequence("", "*const volatile"))
        ptr = ast.parse_pointer()
        self.assertTrue(ptr is not None)
        self.assertEqual(ptr.type_qualifiers, {"const", "volatile"})

    def test_pointer_declarators(self):
        ast = AstGenerator(TokenSequence("", "a *a **a *const a *const *a "
                                             "*const *volatile a, "
                                             "*((((a)))), ((((((*a))))))"))

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(decl.stack, [])

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertEqual(decl.stack[0].type_qualifiers, set())

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        self.assertEqual(decl.stack[0].type_qualifiers, set())
        self.assertEqual(decl.stack[1].type_qualifiers, set())

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertEqual(decl.stack[0].type_qualifiers, {"const"})

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        self.assertEqual(decl.stack[0].type_qualifiers, set())
        self.assertEqual(decl.stack[1].type_qualifiers, {"const"})
        # Remember the stack is ordered from the inside out (right to left)

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        self.assertEqual(decl.stack[0].type_qualifiers, {"volatile"})
        self.assertEqual(decl.stack[1].type_qualifiers, {"const"})
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))

    def test_array_declarators(self):
        ast = AstGenerator(TokenSequence("", "a[], a[][], *a[], (*a)[], "
                                             "*(a[][]), *(a[])[], *(a)[], "))

        # Simple array
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        ast.seq.next()

        # Simple multi-dimensional array
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        ast.seq.next()

        # Array of pointers
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        ast.seq.next()

        # Pointer to an array
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        ast.seq.next()

        # Multidimensional array of pointers
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 3)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[2], DeclaratorPointerPart))
        ast.seq.next()

        # Multidimensional array of pointers
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 3)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[2], DeclaratorPointerPart))
        ast.seq.next()

        # Array of pointers
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        ast.seq.next()

    def test_function_declarators(self):
        ast = AstGenerator(TokenSequence("", "a(), (*a)(), *a(), a(int), "
                                             "a(int b), (*(*a)(int))(double), "
                                             "(*(*a)())[], (*a[])(int)"))

        # Function
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[0].args), 0)
        ast.seq.next()

        # Pointer to a function
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[1].args), 0)
        ast.seq.next()

        # Function that returns a pointer
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorFunctionPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        self.assertEqual(len(decl.stack[0].args), 0)
        ast.seq.next()

        # Function with an integer argument (unnamed)
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[0].args), 1)
        self.assertEqual(decl.stack[0].args[0].specifiers.type_specifier, "int")
        self.assertTrue(decl.stack[0].args[0].declarator is None)
        ast.seq.next()

        # Function with an integer argument (named)
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[0].args), 1)
        self.assertEqual(decl.stack[0].args[0].specifiers.type_specifier, "int")
        self.assertEqual(decl.stack[0].args[0].declarator.name.contents, "b")
        ast.seq.next()

        # Pointer to a function (taking an int) returning a pointer to a
        # function (taking a double)
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 4)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[1].args), 1)
        self.assertEqual(decl.stack[1].args[0].specifiers.type_specifier, "int")
        self.assertTrue(isinstance(decl.stack[2], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[3], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[3].args), 1)
        self.assertEqual(decl.stack[3].args[0].specifiers.type_specifier,
                         "double")
        ast.seq.next()

        # Pointer to a function returning a pointer to an array
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 4)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorFunctionPart))
        self.assertEqual(len(decl.stack[1].args), 0)
        self.assertTrue(isinstance(decl.stack[2], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[3], DeclaratorArrayPart))
        ast.seq.next()

        # Array of pointers to functions
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 3)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[2], DeclaratorFunctionPart))
        ast.seq.next()
