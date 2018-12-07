
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
                                             "*const *volatile a"))
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

    def test_array_declarators(self):
        ast = AstGenerator(TokenSequence("", "a[], a[][], *a[], (*a)[], "
                                             "*(a[][]), *(a[])[], *(a)[], "
                                             "*((((a)))), ((((((*a))))))"))
        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 1)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 3)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[2], DeclaratorPointerPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 3)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[2], DeclaratorPointerPart))
        ast.seq.next()

        decl = ast.parse_declarator()
        self.assertEqual(decl.name.contents, "a")
        self.assertEqual(len(decl.stack), 2)
        self.assertTrue(isinstance(decl.stack[0], DeclaratorArrayPart))
        self.assertTrue(isinstance(decl.stack[1], DeclaratorPointerPart))
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
