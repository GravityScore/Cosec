
# parser/tests.py
# By Ben Anderson
# December 2018

import unittest
from unittest import TestCase
from parser import *


class TestDeclarationSpecifiers(TestCase):
    def test_storage_class(self):
        t = Tokens("", "int; typedef int; int register")
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertEqual(s.storage_class, StorageClass.TYPEDEF)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertEqual(s.storage_class, StorageClass.REGISTER)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

    def test_type_qualifiers(self):
        t = Tokens("", "const int; int const; const int restrict; "
                       "const volatile int restrict; int typedef volatile")
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST,
                                             TypeQualifier.RESTRICT})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST,
                                             TypeQualifier.VOLATILE,
                                             TypeQualifier.RESTRICT})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertEqual(s.storage_class, StorageClass.TYPEDEF)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.VOLATILE})
        self.assertEqual(s.function_specifiers, set())

    def test_function_specifiers(self):
        t = Tokens("", "inline void; void inline")
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.VOID)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, {FunctionSpecifier.INLINE})

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.VOID)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, {FunctionSpecifier.INLINE})

    def test_type_specifiers(self):
        t = Tokens("", "int; unsigned; signed int; short; short int; "
                       "unsigned short int; extern int unsigned; "
                       "static long int; long long int; long int long; "
                       "long static long int const")
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.UINT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.SHORT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.SHORT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.USHORT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertEqual(s.storage_class, StorageClass.EXTERN)
        self.assertEqual(s.type_specifier, TypeSpecifier.UINT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertEqual(s.storage_class, StorageClass.STATIC)
        self.assertEqual(s.type_specifier, TypeSpecifier.LONG)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.LLONG)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.LLONG)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = DeclarationSpecifiers()
        s.parse(t)
        self.assertEqual(s.storage_class, StorageClass.STATIC)
        self.assertEqual(s.type_specifier, TypeSpecifier.LLONG)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST})
        self.assertEqual(s.function_specifiers, set())


if __name__ == '__main__':
    unittest.main()
