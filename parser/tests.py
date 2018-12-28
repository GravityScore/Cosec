
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


class TestPointerDeclarators(TestCase):
    def test_no_pointer(self):
        t = Tokens("", "a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 0)

    def test_single_pointer(self):
        t = Tokens("", "*a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())

    def test_double_pointer(self):
        t = Tokens("", "**a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())

    def test_single_pointer_with_qualifier(self):
        t = Tokens("", "*const a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})

    def test_qualifier_order(self):
        t = Tokens("", "*const *a; **const a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, {TypeQualifier.CONST})

        t.next()  # Skip the semicolon
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertEqual(len(d.parts), 2)

    def test_double_pointer_with_qualifiers(self):
        t = Tokens("", "*const *restrict a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.RESTRICT})
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, {TypeQualifier.CONST})

    def test_multiple_qualifiers(self):
        t = Tokens("", "*const restrict a")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST,
                                                      TypeQualifier.RESTRICT})


class TestFunctionDeclarators(TestCase):
    def test_no_args(self):
        t = Tokens("", "a(); a(void)")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)

        t.next()  # Skip the semicolon
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)

    def test_one_arg(self):
        t = Tokens("", "a(int); a(int b)")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 1)
        arg = d.parts[0].args[0]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator is None)

        t.next()  # Skip the semicolon
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 1)
        arg = d.parts[0].args[0]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Token.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)

    def test_redundant_parentheses(self):
        t = Tokens("", "((((a(int (b))))))")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 1)
        arg = d.parts[0].args[0]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Token.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)

    def test_two_args(self):
        t = Tokens("", "a(int, char); a(int b, char c)")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 2)
        arg = d.parts[0].args[0]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator is None)
        arg = d.parts[0].args[1]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.CHAR)
        self.assertTrue(arg.declarator is None)

        t.next()  # Skip the semicolon
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 2)
        arg = d.parts[0].args[0]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Token.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)
        arg = d.parts[0].args[1]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.CHAR)
        self.assertTrue(arg.declarator.name.type, Token.IDENT)
        self.assertTrue(arg.declarator.name.contents, "c")
        self.assertEqual(len(arg.declarator.parts), 0)


class TestArrayDeclarators(TestCase):
    def test_single_array(self):
        t = Tokens("", "a[]")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)

    def test_double_array(self):
        t = Tokens("", "a[][]")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)
        self.assertTrue(isinstance(d.parts[1], DeclaratorArrayPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertEqual(d.parts[1].is_vla, False)
        self.assertEqual(d.parts[1].is_static, False)

    def test_redundant_parentheses(self):
        t = Tokens("", "((((((a[]))))[]))")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)
        self.assertTrue(isinstance(d.parts[1], DeclaratorArrayPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertEqual(d.parts[1].is_vla, False)
        self.assertEqual(d.parts[1].is_static, False)

    def test_static_qualifier(self):
        t = Tokens("", "a[static]")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, True)

    def test_pointer_qualifiers(self):
        t = Tokens("", "a[const restrict]")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST,
                                                      TypeQualifier.RESTRICT})
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)

    def test_pointer_with_static(self):
        t = Tokens("", "a[const restrict static]")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST,
                                                      TypeQualifier.RESTRICT})
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, True)


class TestMixedDeclarators(TestCase):
    def test_pointer_to_function(self):
        t = Tokens("", "(*a)()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 0)

    def test_pointer_to_function_with_qualifiers(self):
        t = Tokens("", "(*const a)()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 0)

    def test_pointer_to_function_with_args(self):
        t = Tokens("", "(*const a)(int b)")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 1)
        arg = d.parts[1].args[0]
        self.assertTrue(isinstance(arg, DeclaratorFunctionArgument))
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Token.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)

    def test_function_returning_pointer(self):
        t = Tokens("", "*a()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())

    def test_redundant_parentheses(self):
        t = Tokens("", "(((*((((a())))))))")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())

    def test_pointer_to_function_returning_pointer(self):
        t = Tokens("", "*(*a)()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 3)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 0)
        self.assertTrue(isinstance(d.parts[2], DeclaratorPointerPart))
        self.assertEqual(d.parts[2].type_qualifiers, set())

    def test_array_of_functions(self):
        t = Tokens("", "(a[])()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 0)

    def test_array_of_function_pointers(self):
        t = Tokens("", "(*a[])()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 3)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[2], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[2].args), 0)

    def test_pointer_to_array_of_function_pointers(self):
        t = Tokens("", "(*(*a)[])()")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 4)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorArrayPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertEqual(d.parts[1].is_vla, False)
        self.assertEqual(d.parts[1].is_static, False)
        self.assertTrue(isinstance(d.parts[2], DeclaratorPointerPart))
        self.assertEqual(d.parts[2].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[3], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[3].args), 0)

    def test_function_returning_pointer_to_array(self):
        t = Tokens("", "(*a())[]")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 3)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[2], DeclaratorArrayPart))
        self.assertEqual(d.parts[2].type_qualifiers, set())
        self.assertEqual(d.parts[2].is_vla, False)
        self.assertEqual(d.parts[2].is_static, False)

    def test_pointer_to_function_returning_pointer_to_function(self):
        t = Tokens("", "(*(*a)(int))(char)")
        d = Declarator()
        d.parse(t)
        self.assertEqual(d.name.type, Token.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 4)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 1)
        self.assertTrue(isinstance(d.parts[2], DeclaratorPointerPart))
        self.assertEqual(d.parts[2].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[3], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[3].args), 1)


if __name__ == '__main__':
    unittest.main()
