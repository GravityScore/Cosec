
# test_parser.py
# By Ben Anderson
# December 2018

import unittest
from unittest import TestCase
from typing import cast

from lexer import Tokens, Tk
from parser import Parser, StorageClass, TypeSpecifier, TypeQualifier,    \
    FunctionSpecifier, DeclaratorPointerPart, DeclaratorFunctionPart,     \
    DeclaratorArrayPart
from parser import ExpressionList, TernaryExpression, BinaryExpression,   \
    CastExpression, SizeofExpression, UnaryExpression, PostfixExpression, \
    ArrayAccessExpression, FunctionCallExpression, FieldAccessExpression, \
    SymbolExpression, ConstantExpression, BinaryOperator, UnaryOperator


# ******************************************************************************
#     Declarator Tests
# ******************************************************************************


class TestDeclarationSpecifiers(TestCase):
    def test_storage_class(self):
        t = Tokens("", "int; typedef int; int register")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.storage_class, StorageClass.TYPEDEF)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.storage_class, StorageClass.REGISTER)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

    def test_type_qualifiers(self):
        t = Tokens("", "const int; int const; const int restrict; "
                       "const volatile int restrict; int typedef volatile")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST,
                                             TypeQualifier.RESTRICT})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST,
                                             TypeQualifier.VOLATILE,
                                             TypeQualifier.RESTRICT})
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.storage_class, StorageClass.TYPEDEF)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.VOLATILE})
        self.assertEqual(s.function_specifiers, set())

    def test_function_specifiers(self):
        t = Tokens("", "inline void; void inline")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.VOID)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, {FunctionSpecifier.INLINE})

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.VOID)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, {FunctionSpecifier.INLINE})

    def test_type_specifiers(self):
        t = Tokens("", "int; unsigned; signed int; short; short int; "
                       "unsigned short int; extern int unsigned; "
                       "static long int; long long int; long int long; "
                       "long static long int const")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.UINT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.INT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.SHORT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.SHORT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.USHORT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.storage_class, StorageClass.EXTERN)
        self.assertEqual(s.type_specifier, TypeSpecifier.UINT)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.storage_class, StorageClass.STATIC)
        self.assertEqual(s.type_specifier, TypeSpecifier.LONG)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.LLONG)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertTrue(s.storage_class is None)
        self.assertEqual(s.type_specifier, TypeSpecifier.LLONG)
        self.assertEqual(s.type_qualifiers, set())
        self.assertEqual(s.function_specifiers, set())

        t.next()  # Skip the semicolon
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.storage_class, StorageClass.STATIC)
        self.assertEqual(s.type_specifier, TypeSpecifier.LLONG)
        self.assertEqual(s.type_qualifiers, {TypeQualifier.CONST})
        self.assertEqual(s.function_specifiers, set())


class TestPointerDeclarators(TestCase):
    def test_no_pointer(self):
        t = Tokens("", "a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 0)

    def test_single_pointer(self):
        t = Tokens("", "*a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())

    def test_double_pointer(self):
        t = Tokens("", "**a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())

    def test_single_pointer_with_qualifier(self):
        t = Tokens("", "*const a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})

    def test_qualifier_order(self):
        t = Tokens("", "*const *a; **const a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, {TypeQualifier.CONST})

        t.next()  # Skip the semicolon
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())
        self.assertEqual(len(d.parts), 2)

    def test_double_pointer_with_qualifiers(self):
        t = Tokens("", "*const *restrict a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.RESTRICT})
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, {TypeQualifier.CONST})

    def test_multiple_qualifiers(self):
        t = Tokens("", "*const restrict a")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST,
                                                      TypeQualifier.RESTRICT})


class TestFunctionDeclarators(TestCase):
    def test_no_args(self):
        t = Tokens("", "a(); a(void)")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)

        t.next()  # Skip the semicolon
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)

    def test_one_arg(self):
        t = Tokens("", "a(int); a(int b)")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 1)
        arg = d.parts[0].args[0]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator is None)

        t.next()  # Skip the semicolon
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 1)
        arg = d.parts[0].args[0]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Tk.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)

    def test_redundant_parentheses(self):
        t = Tokens("", "((((a(int (b))))))")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 1)
        arg = d.parts[0].args[0]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Tk.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)

    def test_two_args(self):
        t = Tokens("", "a(int, char); a(int b, char c)")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 2)
        arg = d.parts[0].args[0]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator is None)
        arg = d.parts[0].args[1]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.CHAR)
        self.assertTrue(arg.declarator is None)

        t.next()  # Skip the semicolon
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 2)
        arg = d.parts[0].args[0]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Tk.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)
        arg = d.parts[0].args[1]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.CHAR)
        self.assertTrue(arg.declarator.name.type, Tk.IDENT)
        self.assertTrue(arg.declarator.name.contents, "c")
        self.assertEqual(len(arg.declarator.parts), 0)


class TestArrayDeclarators(TestCase):
    def test_single_array(self):
        t = Tokens("", "a[]")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)

    def test_double_array(self):
        t = Tokens("", "a[][]")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, True)

    def test_pointer_qualifiers(self):
        t = Tokens("", "a[const restrict]")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorArrayPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST,
                                                      TypeQualifier.RESTRICT})
        self.assertEqual(d.parts[0].is_vla, False)
        self.assertEqual(d.parts[0].is_static, False)

    def test_pointer_with_static(self):
        t = Tokens("", "a[const restrict static]")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, set())
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 0)

    def test_pointer_to_function_with_qualifiers(self):
        t = Tokens("", "(*const a)()")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 0)

    def test_pointer_to_function_with_args(self):
        t = Tokens("", "(*const a)(int b)")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertEqual(d.parts[0].type_qualifiers, {TypeQualifier.CONST})
        self.assertTrue(isinstance(d.parts[1], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[1].args), 1)
        arg = d.parts[1].args[0]
        self.assertEqual(arg.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(arg.declarator.name.type, Tk.IDENT)
        self.assertTrue(arg.declarator.name.contents, "b")
        self.assertEqual(len(arg.declarator.parts), 0)

    def test_function_returning_pointer(self):
        t = Tokens("", "*a()")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())

    def test_redundant_parentheses(self):
        t = Tokens("", "(((*((((a())))))))")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
        self.assertEqual(d.name.contents, "a")
        self.assertEqual(len(d.parts), 2)
        self.assertTrue(isinstance(d.parts[0], DeclaratorFunctionPart))
        self.assertEqual(len(d.parts[0].args), 0)
        self.assertTrue(isinstance(d.parts[1], DeclaratorPointerPart))
        self.assertEqual(d.parts[1].type_qualifiers, set())

    def test_pointer_to_function_returning_pointer(self):
        t = Tokens("", "*(*a)()")
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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
        p = Parser(t)
        d = p.parse_declarator()
        self.assertEqual(d.name.type, Tk.IDENT)
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


# ******************************************************************************
#     Expression Tests
# ******************************************************************************


class TestPrimaryExpressions(TestCase):
    def test_symbols(self):
        t = Tokens("", "hello")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, SymbolExpression))
        e = cast(SymbolExpression, e)
        self.assertEqual(e.name.type, Tk.IDENT)
        self.assertEqual(e.name.contents, "hello")

    def test_constants(self):
        t = Tokens("", "123")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, ConstantExpression))
        e = cast(ConstantExpression, e)
        self.assertEqual(e.value.type, Tk.CONST_INT)
        self.assertEqual(e.value.number, 123)


class TestUnaryExpressions(TestCase):
    def test_single_operators(self):
        t = Tokens("", "-a; !b")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.NEG)
        e = e.operand
        self.assertTrue(isinstance(e, SymbolExpression))
        e = cast(SymbolExpression, e)
        self.assertTrue(e.name.type, Tk.IDENT)
        self.assertTrue(e.name.contents, "a")

        t.next()  # Skip the semicolon
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.LOGICAL_NOT)
        e = e.operand
        self.assertTrue(isinstance(e, SymbolExpression))
        e = cast(SymbolExpression, e)
        self.assertTrue(e.name.type, Tk.IDENT)
        self.assertTrue(e.name.contents, "b")

    def test_multiple_operators(self):
        t = Tokens("", "- -3; - - - 3; *&*d")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.NEG)
        e = e.operand
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.NEG)
        e = e.operand
        self.assertTrue(isinstance(e, ConstantExpression))
        e = cast(ConstantExpression, e)
        self.assertTrue(e.value.type, Tk.CONST_INT)
        self.assertTrue(e.value.number, 3)

        t.next()  # Skip the semicolon
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.NEG)
        e = e.operand
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.NEG)
        e = e.operand
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.NEG)
        e = e.operand
        self.assertTrue(isinstance(e, ConstantExpression))
        e = cast(ConstantExpression, e)
        self.assertTrue(e.value.type, Tk.CONST_INT)
        self.assertTrue(e.value.number, 3)

        t.next()  # Skip the semicolon
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.DEREF)
        e = e.operand
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.ADDR)
        e = e.operand
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.DEREF)
        e = e.operand
        self.assertTrue(isinstance(e, SymbolExpression))
        e = cast(SymbolExpression, e)
        self.assertTrue(e.name.type, Tk.IDENT)
        self.assertTrue(e.name.contents, "d")

    def test_prefix_operators(self):
        t = Tokens("", "--3; ++a")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.DEC)
        e = e.operand
        self.assertTrue(isinstance(e, ConstantExpression))
        e = cast(ConstantExpression, e)
        self.assertEqual(e.value.type, Tk.CONST_INT)
        self.assertEqual(e.value.number, 3)

        t.next()  # Skip the semicolon
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.INC)
        e = e.operand
        self.assertTrue(isinstance(e, SymbolExpression))
        e = cast(SymbolExpression, e)
        self.assertEqual(e.name.type, Tk.IDENT)
        self.assertEqual(e.name.contents, "a")


class TestSizeofExpressions(TestCase):
    def test_sizeof_expression(self):
        t = Tokens("", "sizeof a")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, UnaryExpression))
        e = cast(UnaryExpression, e)
        self.assertEqual(e.operator, UnaryOperator.SIZEOF)
        e = e.operand
        self.assertTrue(isinstance(e, SymbolExpression))
        e = cast(SymbolExpression, e)
        self.assertEqual(e.name.type, Tk.IDENT)
        self.assertEqual(e.name.contents, "a")

    def test_sizeof_declaration_specifiers(self):
        t = Tokens("", "sizeof(int)")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, SizeofExpression))
        e = cast(SizeofExpression, e)
        self.assertEqual(e.type.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(e.type.declarator is None)

    def test_sizeof_with_declarator(self):
        t = Tokens("", "sizeof(int *)")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, SizeofExpression))
        e = cast(SizeofExpression, e)
        self.assertEqual(e.type.specifiers.type_specifier, TypeSpecifier.INT)
        d = e.type.declarator
        self.assertTrue(d.name is None)
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))


class TestBinaryExpressions(TestCase):
    def test_basic_operation(self):
        t = Tokens("", "3 + 4")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, BinaryExpression))
        e = cast(BinaryExpression, e)
        self.assertEqual(e.operator, BinaryOperator.ADD)
        self.assertTrue(isinstance(e.left, ConstantExpression))
        self.assertTrue(isinstance(e.right, ConstantExpression))
        l = cast(ConstantExpression, e.left)
        self.assertEqual(l.value.type, Tk.CONST_INT)
        self.assertEqual(l.value.number, 3)
        r = cast(ConstantExpression, e.right)
        self.assertEqual(r.value.type, Tk.CONST_INT)
        self.assertEqual(r.value.number, 4)

    def test_same_precedence(self):
        t = Tokens("", "3 + 4 + 5")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, BinaryExpression))
        e = cast(BinaryExpression, e)
        self.assertEqual(e.operator, BinaryOperator.ADD)
        self.assertTrue(isinstance(e.left, BinaryExpression))
        self.assertTrue(isinstance(e.right, ConstantExpression))
        l = cast(BinaryExpression, e.left)
        self.assertEqual(l.operator, BinaryOperator.ADD)
        self.assertTrue(isinstance(l.left, ConstantExpression))
        self.assertTrue(isinstance(l.right, ConstantExpression))
        ll = cast(ConstantExpression, l.left)
        self.assertEqual(ll.value.type, Tk.CONST_INT)
        self.assertEqual(ll.value.number, 3)
        lr = cast(ConstantExpression, l.right)
        self.assertEqual(lr.value.type, Tk.CONST_INT)
        self.assertEqual(lr.value.number, 4)
        r = cast(ConstantExpression, e.right)
        self.assertEqual(r.value.type, Tk.CONST_INT)
        self.assertEqual(r.value.number, 5)

    def test_different_precedences(self):
        t = Tokens("", "3 + 4 * 5")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, BinaryExpression))
        e = cast(BinaryExpression, e)
        self.assertEqual(e.operator, BinaryOperator.ADD)
        self.assertTrue(isinstance(e.left, ConstantExpression))
        self.assertTrue(isinstance(e.right, BinaryExpression))
        l = cast(ConstantExpression, e.left)
        self.assertEqual(l.value.type, Tk.CONST_INT)
        self.assertEqual(l.value.number, 3)
        r = cast(BinaryExpression, e.right)
        self.assertEqual(r.operator, BinaryOperator.MUL)
        self.assertTrue(isinstance(r.left, ConstantExpression))
        self.assertTrue(isinstance(r.right, ConstantExpression))
        rl = cast(ConstantExpression, r.left)
        self.assertEqual(rl.value.type, Tk.CONST_INT)
        self.assertEqual(rl.value.number, 4)
        rr = cast(ConstantExpression, r.right)
        self.assertEqual(rr.value.type, Tk.CONST_INT)
        self.assertEqual(rr.value.number, 5)

    def test_subexpressions(self):
        t = Tokens("", "(3 + 4) * 5")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, BinaryExpression))
        e = cast(BinaryExpression, e)
        self.assertEqual(e.operator, BinaryOperator.MUL)
        self.assertTrue(isinstance(e.left, BinaryExpression))
        self.assertTrue(isinstance(e.right, ConstantExpression))
        l = cast(BinaryExpression, e.left)
        self.assertEqual(l.operator, BinaryOperator.ADD)
        self.assertTrue(isinstance(l.left, ConstantExpression))
        self.assertTrue(isinstance(l.right, ConstantExpression))
        ll = cast(ConstantExpression, l.left)
        self.assertEqual(ll.value.type, Tk.CONST_INT)
        self.assertEqual(ll.value.number, 3)
        lr = cast(ConstantExpression, l.right)
        self.assertEqual(lr.value.type, Tk.CONST_INT)
        self.assertEqual(lr.value.number, 4)
        r = cast(ConstantExpression, e.right)
        self.assertEqual(r.value.type, Tk.CONST_INT)
        self.assertEqual(r.value.number, 5)

    def test_ternary(self):
        t = Tokens("", "3 ? 4 : 5")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, TernaryExpression))
        e = cast(TernaryExpression, e)
        self.assertTrue(isinstance(e.condition, ConstantExpression))
        self.assertTrue(isinstance(e.true, ConstantExpression))
        self.assertTrue(isinstance(e.false, ConstantExpression))
        c = cast(ConstantExpression, e.condition)
        self.assertEqual(c.value.type, Tk.CONST_INT)
        self.assertEqual(c.value.number, 3)
        t = cast(ConstantExpression, e.true)
        self.assertEqual(t.value.type, Tk.CONST_INT)
        self.assertEqual(t.value.number, 4)
        f = cast(ConstantExpression, e.false)
        self.assertEqual(f.value.type, Tk.CONST_INT)
        self.assertEqual(f.value.number, 5)


class TestCastExpressions(TestCase):
    def test_just_declaration_specifiers(self):
        t = Tokens("", "(int) 3")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, CastExpression))
        e = cast(CastExpression, e)
        self.assertEqual(e.type.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(e.type.declarator is None)
        self.assertTrue(isinstance(e.operand, ConstantExpression))
        e = cast(ConstantExpression, e.operand)
        self.assertEqual(e.value.type, Tk.CONST_INT)
        self.assertEqual(e.value.number, 3)

    def test_with_declarator(self):
        t = Tokens("", "(int *) 3")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, CastExpression))
        e = cast(CastExpression, e)
        self.assertEqual(e.type.specifiers.type_specifier, TypeSpecifier.INT)
        d = e.type.declarator
        self.assertTrue(d.name is None)
        self.assertEqual(len(d.parts), 1)
        self.assertTrue(isinstance(d.parts[0], DeclaratorPointerPart))
        self.assertTrue(isinstance(e.operand, ConstantExpression))
        e = cast(ConstantExpression, e.operand)
        self.assertEqual(e.value.type, Tk.CONST_INT)
        self.assertEqual(e.value.number, 3)


class TestPostfixExpressions(TestCase):
    def test_postfix_operators(self):
        t = Tokens("", "3--")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, PostfixExpression))
        e = cast(PostfixExpression, e)
        self.assertEqual(e.operator, UnaryOperator.DEC)
        e = e.operand
        self.assertTrue(isinstance(e, ConstantExpression))
        e = cast(ConstantExpression, e)
        self.assertEqual(e.value.type, Tk.CONST_INT)
        self.assertEqual(e.value.number, 3)

    def test_array_access(self):
        t = Tokens("", "a[3]")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, ArrayAccessExpression))
        e = cast(ArrayAccessExpression, e)
        self.assertTrue(isinstance(e.array, SymbolExpression))
        self.assertTrue(isinstance(e.index, ConstantExpression))
        a = cast(SymbolExpression, e.array)
        self.assertEqual(a.name.type, Tk.IDENT)
        self.assertEqual(a.name.contents, "a")
        i = cast(ConstantExpression, e.index)
        self.assertEqual(i.value.type, Tk.CONST_INT)
        self.assertEqual(i.value.number, 3)

    def test_function_call_no_args(self):
        t = Tokens("", "a()")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, FunctionCallExpression))
        e = cast(FunctionCallExpression, e)
        self.assertTrue(isinstance(e.function, SymbolExpression))
        f = cast(SymbolExpression, e.function)
        self.assertEqual(f.name.type, Tk.IDENT)
        self.assertEqual(f.name.contents, "a")
        self.assertEqual(len(e.args), 0)

    def test_function_call_one_arg(self):
        t = Tokens("", "a(3)")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, FunctionCallExpression))
        e = cast(FunctionCallExpression, e)
        self.assertTrue(isinstance(e.function, SymbolExpression))
        f = cast(SymbolExpression, e.function)
        self.assertEqual(f.name.type, Tk.IDENT)
        self.assertEqual(f.name.contents, "a")
        self.assertEqual(len(e.args), 1)
        self.assertTrue(isinstance(e.args[0], ConstantExpression))
        a = cast(ConstantExpression, e.args[0])
        self.assertEqual(a.value.type, Tk.CONST_INT)
        self.assertEqual(a.value.number, 3)

    def test_function_call_multiple_args(self):
        t = Tokens("", "a(3, 4)")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, FunctionCallExpression))
        e = cast(FunctionCallExpression, e)
        self.assertTrue(isinstance(e.function, SymbolExpression))
        f = cast(SymbolExpression, e.function)
        self.assertEqual(f.name.type, Tk.IDENT)
        self.assertEqual(f.name.contents, "a")
        self.assertEqual(len(e.args), 2)
        self.assertTrue(isinstance(e.args[0], ConstantExpression))
        a = cast(ConstantExpression, e.args[0])
        self.assertEqual(a.value.type, Tk.CONST_INT)
        self.assertEqual(a.value.number, 3)
        self.assertTrue(isinstance(e.args[1], ConstantExpression))
        a = cast(ConstantExpression, e.args[1])
        self.assertEqual(a.value.type, Tk.CONST_INT)
        self.assertEqual(a.value.number, 4)

    def test_field_access(self):
        t = Tokens("", "a.b")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, FieldAccessExpression))
        e = cast(FieldAccessExpression, e)
        self.assertTrue(isinstance(e.struct, SymbolExpression))
        s = cast(SymbolExpression, e.struct)
        self.assertEqual(s.name.type, Tk.IDENT)
        self.assertEqual(s.name.contents, "a")
        self.assertEqual(e.name.type, Tk.IDENT)
        self.assertEqual(e.name.contents, "b")

    def test_field_deref_access(self):
        t = Tokens("", "a->b")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, FieldAccessExpression))
        e = cast(FieldAccessExpression, e)
        self.assertEqual(e.name.type, Tk.IDENT)
        self.assertEqual(e.name.contents, "b")
        self.assertTrue(isinstance(e.struct, UnaryExpression))
        e = cast(UnaryExpression, e.struct)
        self.assertEqual(e.operator, UnaryOperator.DEREF)
        self.assertTrue(isinstance(e.operand, SymbolExpression))
        e = cast(SymbolExpression, e.operand)
        self.assertEqual(e.name.type, Tk.IDENT)
        self.assertEqual(e.name.contents, "a")


class TestExpressionList(TestCase):
    def test_two_expressions(self):
        t = Tokens("", "a, b")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, ExpressionList))
        e = cast(ExpressionList, e)
        self.assertEqual(len(e.roots), 2)
        self.assertTrue(isinstance(e.roots[0], SymbolExpression))
        self.assertTrue(isinstance(e.roots[1], SymbolExpression))
        a = cast(SymbolExpression, e.roots[0])
        self.assertEqual(a.name.type, Tk.IDENT)
        self.assertEqual(a.name.contents, "a")
        b = cast(SymbolExpression, e.roots[1])
        self.assertEqual(b.name.type, Tk.IDENT)
        self.assertEqual(b.name.contents, "b")


if __name__ == '__main__':
    unittest.main()
