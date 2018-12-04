
# test_ast.py
# By Ben Anderson
# December 2018

from unittest import TestCase
from ast import AstGenerator
from lexer import TokenSequence


class TestAstGenerator(TestCase):
    def test_declaration_specifiers(self):
        ast = AstGenerator(TokenSequence("", "void"))
        spec = ast.parse_declaration_specifiers()
        self.assertTrue(spec.storage_class is None)
        self.assertEqual(spec.type_specifier, "void")
        self.assertEqual(len(spec.type_qualifiers), 0)
        self.assertEqual(len(spec.function_specifiers), 0)

        ast = AstGenerator(TokenSequence("", "unsigned"))
        spec = ast.parse_declaration_specifiers()
        self.assertTrue(spec.storage_class is None)
        self.assertEqual(spec.type_specifier, "uint")
        self.assertEqual(len(spec.type_qualifiers), 0)
        self.assertEqual(len(spec.function_specifiers), 0)

        ast = AstGenerator(TokenSequence("", "unsigned int"))
        spec = ast.parse_declaration_specifiers()
        self.assertTrue(spec.storage_class is None)
        self.assertEqual(spec.type_specifier, "uint")
        self.assertEqual(len(spec.type_qualifiers), 0)
        self.assertEqual(len(spec.function_specifiers), 0)

        ast = AstGenerator(TokenSequence("", "extern unsigned int"))
        spec = ast.parse_declaration_specifiers()
        self.assertEqual(spec.storage_class, "extern")
        self.assertEqual(spec.type_specifier, "uint")
        self.assertEqual(len(spec.type_qualifiers), 0)
        self.assertEqual(len(spec.function_specifiers), 0)

        ast = AstGenerator(TokenSequence("", "long static int long inline"))
        spec = ast.parse_declaration_specifiers()
        self.assertEqual(spec.storage_class, "static")
        self.assertEqual(spec.type_specifier, "llong")
        self.assertEqual(len(spec.type_qualifiers), 0)
        self.assertEqual(spec.function_specifiers, {"inline"})

        ast = AstGenerator(TokenSequence("", "int const restrict"))
        spec = ast.parse_declaration_specifiers()
        self.assertTrue(spec.storage_class is None)
        self.assertEqual(spec.type_specifier, "int")
        self.assertEqual(spec.type_qualifiers, {"const", "restrict"})
        self.assertEqual(len(spec.function_specifiers), 0)
