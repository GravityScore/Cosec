
# test_parser.py
# By Ben Anderson
# December 2018

import unittest
from unittest import TestCase
from typing import cast

from lexer import Tokens, Tk
from parser import Parser, StorageClass, TypeSpecifier, TypeQualifier,        \
    FunctionSpecifier, StructSpecifier, UnionSpecifier, EnumSpecifier,        \
    EnumConst, DeclaratorPointerPart, DeclaratorFunctionPart,                 \
    DeclaratorArrayPart, DeclarationList, Declaration
from parser import ExpressionList, TernaryExpression, BinaryExpression,       \
    CastExpression, SizeofExpression, UnaryExpression, PostfixExpression,     \
    ArrayAccessExpression, FunctionCallExpression, FieldAccessExpression,     \
    SymbolExpression, ConstantExpression, BinaryOperator, UnaryOperator
from parser import FunctionDefinition, Statement, CompoundStatement,          \
    DeclarationStatement, ExpressionStatement, IfStatementChain, IfStatement, \
    SwitchStatement, CaseStatement, DefaultStatement, WhileStatement,         \
    DoWhileStatement, ForStatement, ContinueStatement, BreakStatement,        \
    ReturnStatement, GotoStatement, LabelStatement


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


class TestTypedef(TestCase):
    def test_basic(self):
        t = Tokens("", "typedef int a; a b;")
        p = Parser(t)
        p.push_scope()
        d = p.parse_declaration_list()
        self.assertEqual(len(d.declarations), 1)
        d = d.declarations[0]
        self.assertEqual(d.specifiers.storage_class, StorageClass.TYPEDEF)
        self.assertEqual(d.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertEqual(d.declarator.name.contents, "a")

        d = p.parse_declaration_list()
        self.assertEqual(len(d.declarations), 1)
        d = d.declarations[0]
        self.assertEqual(d.specifiers.type_specifier, TypeSpecifier.TYPEDEF)
        self.assertEqual(d.specifiers.type_specifier.typedef_name, "a")
        self.assertEqual(d.declarator.name.contents, "b")
        p.pop_scope()


class TestStructSpecifier(TestCase):
    def test_struct(self):
        t = Tokens("", "struct thing { int a; }")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.type_specifier, TypeSpecifier.STRUCT)
        s = cast(StructSpecifier, s.type_specifier.struct)
        self.assertEqual(s.name.contents, "thing")
        self.assertEqual(len(s.fields), 1)
        d = cast(Declaration, s.fields[0])
        self.assertEqual(d.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertEqual(d.declarator.name.contents, "a")

    def test_anonymous_struct(self):
        t = Tokens("", "struct { int a; }")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.type_specifier, TypeSpecifier.STRUCT)
        s = cast(StructSpecifier, s.type_specifier.struct)
        self.assertTrue(s.name is None)
        self.assertEqual(len(s.fields), 1)
        d = cast(Declaration, s.fields[0])
        self.assertEqual(d.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertEqual(d.declarator.name.contents, "a")

    def test_incomplete_struct(self):
        t = Tokens("", "struct thing")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.type_specifier, TypeSpecifier.STRUCT)
        s = cast(StructSpecifier, s.type_specifier.struct)
        self.assertEqual(s.name.contents, "thing")
        self.assertTrue(s.fields is None)

    def test_union(self):
        t = Tokens("", "union thing { int a; }")
        p = Parser(t)
        s = p.parse_declaration_specifiers()
        self.assertEqual(s.type_specifier, TypeSpecifier.UNION)
        s = cast(UnionSpecifier, s.type_specifier.union)
        self.assertEqual(s.name.contents, "thing")
        self.assertEqual(len(s.fields), 1)
        d = cast(Declaration, s.fields[0])
        self.assertEqual(d.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertEqual(d.declarator.name.contents, "a")


class TestEnumSpecifier(TestCase):
    def test_enum(self):
        t = Tokens("", "enum thing { THING1, THING2 }")
        p = Parser(t)
        d = p.parse_declaration_specifiers()
        self.assertEqual(d.type_specifier, TypeSpecifier.ENUM)
        e = cast(EnumSpecifier, d.type_specifier.enum)
        self.assertEqual(e.name.contents, "thing")
        self.assertEqual(len(e.enum_consts), 2)
        self.assertEqual(e.enum_consts[0].name.contents, "THING1")
        self.assertEqual(e.enum_consts[1].name.contents, "THING2")

    def test_anonymous_enum(self):
        t = Tokens("", "enum { THING1, THING2 }")
        p = Parser(t)
        d = p.parse_declaration_specifiers()
        self.assertEqual(d.type_specifier, TypeSpecifier.ENUM)
        e = cast(EnumSpecifier, d.type_specifier.enum)
        self.assertTrue(e.name is None)
        self.assertEqual(len(e.enum_consts), 2)
        self.assertEqual(e.enum_consts[0].name.contents, "THING1")
        self.assertEqual(e.enum_consts[1].name.contents, "THING2")

    def test_incomplete_enum(self):
        t = Tokens("", "enum thing")
        p = Parser(t)
        d = p.parse_declaration_specifiers()
        self.assertEqual(d.type_specifier, TypeSpecifier.ENUM)
        e = cast(EnumSpecifier, d.type_specifier.enum)
        self.assertEqual(e.name.contents, "thing")
        self.assertTrue(e.enum_consts is None)


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
        self.assertEqual(e.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(e.declarator is None)

    def test_sizeof_with_declarator(self):
        t = Tokens("", "sizeof(int *)")
        p = Parser(t)
        e = p.parse_expression()
        self.assertTrue(isinstance(e, SizeofExpression))
        e = cast(SizeofExpression, e)
        self.assertEqual(e.specifiers.type_specifier, TypeSpecifier.INT)
        d = e.declarator
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
        self.assertEqual(e.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertTrue(e.declarator is None)
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
        self.assertEqual(e.specifiers.type_specifier, TypeSpecifier.INT)
        d = e.declarator
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


# ******************************************************************************
#     Statement Tests
# ******************************************************************************


class TestLabelledStatement(TestCase):
    def test_labelled_statement(self):
        t = Tokens("", "label:")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, LabelStatement))
        s = cast(LabelStatement, s)
        self.assertEqual(s.name.type, Tk.IDENT)
        self.assertEqual(s.name.contents, "label")

    def test_case_statement(self):
        t = Tokens("", "case 3:")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, CaseStatement))
        s = cast(CaseStatement, s)
        self.assertTrue(isinstance(s.condition, ConstantExpression))
        c = cast(ConstantExpression, s.condition)
        self.assertTrue(c.value.type, Tk.CONST_INT)
        self.assertTrue(c.value.number, 3)

    def test_default_statement(self):
        t = Tokens("", "default:")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, DefaultStatement))


class TestExpressionStatement(TestCase):
    def test_empty_statement(self):
        t = Tokens("", "; 3;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ExpressionStatement))
        s = cast(ExpressionStatement, s)
        self.assertTrue(isinstance(s.expression, ConstantExpression))
        e = cast(ConstantExpression, s.expression)
        self.assertTrue(e.value.type, Tk.CONST_INT)
        self.assertTrue(e.value.number, 3)

    def test_basic_expression(self):
        t = Tokens("", "3 + 4;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ExpressionStatement))
        s = cast(ExpressionStatement, s)
        self.assertTrue(isinstance(s.expression, BinaryExpression))
        e = cast(BinaryExpression, s.expression)
        self.assertTrue(isinstance(e.left, ConstantExpression))
        self.assertTrue(isinstance(e.right, ConstantExpression))

    def test_function_call(self):
        t = Tokens("", "a();")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ExpressionStatement))
        s = cast(ExpressionStatement, s)
        self.assertTrue(isinstance(s.expression, FunctionCallExpression))
        e = cast(FunctionCallExpression, s.expression)
        self.assertTrue(isinstance(e.function, SymbolExpression))
        self.assertEqual(len(e.args), 0)


class TestSelectionStatement(TestCase):
    def test_single_if(self):
        t = Tokens("", "if (a) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, IfStatementChain))
        s = cast(IfStatementChain, s)
        self.assertEqual(len(s.chain), 1)
        self.assertTrue(isinstance(s.chain[0], IfStatement))
        s = cast(IfStatement, s.chain[0])
        self.assertTrue(isinstance(s.condition, SymbolExpression))
        self.assertTrue(isinstance(s.body, CompoundStatement))
        s = cast(CompoundStatement, s.body)
        self.assertEqual(len(s.statements), 0)

    def test_else(self):
        t = Tokens("", "if (a) {} else {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, IfStatementChain))
        s = cast(IfStatementChain, s)
        self.assertEqual(len(s.chain), 2)
        self.assertTrue(isinstance(s.chain[0], IfStatement))
        a = cast(IfStatement, s.chain[0])
        self.assertTrue(isinstance(a.condition, SymbolExpression))
        self.assertTrue(isinstance(a.body, CompoundStatement))
        a = cast(CompoundStatement, a.body)
        self.assertEqual(len(a.statements), 0)
        b = cast(IfStatement, s.chain[1])
        self.assertTrue(b.condition is None)
        self.assertTrue(isinstance(b.body, CompoundStatement))
        b = cast(CompoundStatement, b.body)
        self.assertEqual(len(b.statements), 0)

    def test_single_elseif(self):
        t = Tokens("", "if (a) {} else if (b) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, IfStatementChain))
        s = cast(IfStatementChain, s)
        self.assertEqual(len(s.chain), 2)
        self.assertTrue(isinstance(s.chain[0], IfStatement))
        a = cast(IfStatement, s.chain[0])
        self.assertTrue(isinstance(a.condition, SymbolExpression))
        self.assertTrue(isinstance(a.body, CompoundStatement))
        a = cast(CompoundStatement, a.body)
        self.assertEqual(len(a.statements), 0)
        b = cast(IfStatement, s.chain[1])
        self.assertTrue(isinstance(b.condition, SymbolExpression))
        self.assertTrue(isinstance(b.body, CompoundStatement))
        b = cast(CompoundStatement, b.body)
        self.assertEqual(len(b.statements), 0)

    def test_multiple_elseifs(self):
        t = Tokens("", "if (a) {} else if (b) {} else if (c) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, IfStatementChain))
        s = cast(IfStatementChain, s)
        self.assertEqual(len(s.chain), 3)
        self.assertTrue(isinstance(s.chain[0], IfStatement))
        a = cast(IfStatement, s.chain[0])
        self.assertTrue(isinstance(a.condition, SymbolExpression))
        self.assertTrue(isinstance(a.body, CompoundStatement))
        a = cast(CompoundStatement, a.body)
        self.assertEqual(len(a.statements), 0)
        b = cast(IfStatement, s.chain[1])
        self.assertTrue(isinstance(b.condition, SymbolExpression))
        self.assertTrue(isinstance(b.body, CompoundStatement))
        b = cast(CompoundStatement, b.body)
        self.assertEqual(len(b.statements), 0)
        c = cast(IfStatement, s.chain[2])
        self.assertTrue(isinstance(c.condition, SymbolExpression))
        self.assertTrue(isinstance(c.body, CompoundStatement))
        c = cast(CompoundStatement, c.body)
        self.assertEqual(len(c.statements), 0)

    def test_elseif_else(self):
        t = Tokens("", "if (a) {} else if (b) {} else {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, IfStatementChain))
        s = cast(IfStatementChain, s)
        self.assertEqual(len(s.chain), 3)
        self.assertTrue(isinstance(s.chain[0], IfStatement))
        a = cast(IfStatement, s.chain[0])
        self.assertTrue(isinstance(a.condition, SymbolExpression))
        self.assertTrue(isinstance(a.body, CompoundStatement))
        a = cast(CompoundStatement, a.body)
        self.assertEqual(len(a.statements), 0)
        b = cast(IfStatement, s.chain[1])
        self.assertTrue(isinstance(b.condition, SymbolExpression))
        self.assertTrue(isinstance(b.body, CompoundStatement))
        b = cast(CompoundStatement, b.body)
        self.assertEqual(len(b.statements), 0)
        c = cast(IfStatement, s.chain[2])
        self.assertTrue(c.condition is None)
        self.assertTrue(isinstance(c.body, CompoundStatement))
        c = cast(CompoundStatement, c.body)
        self.assertEqual(len(c.statements), 0)

    def test_multiple_elseifs_else(self):
        t = Tokens("", "if (a) {} else if (b) {} else if (c) {} else {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, IfStatementChain))
        s = cast(IfStatementChain, s)
        self.assertEqual(len(s.chain), 4)
        self.assertTrue(isinstance(s.chain[0], IfStatement))
        a = cast(IfStatement, s.chain[0])
        self.assertTrue(isinstance(a.condition, SymbolExpression))
        self.assertTrue(isinstance(a.body, CompoundStatement))
        a = cast(CompoundStatement, a.body)
        self.assertEqual(len(a.statements), 0)
        b = cast(IfStatement, s.chain[1])
        self.assertTrue(isinstance(b.condition, SymbolExpression))
        self.assertTrue(isinstance(b.body, CompoundStatement))
        b = cast(CompoundStatement, b.body)
        self.assertEqual(len(b.statements), 0)
        c = cast(IfStatement, s.chain[2])
        self.assertTrue(isinstance(c.condition, SymbolExpression))
        self.assertTrue(isinstance(c.body, CompoundStatement))
        c = cast(CompoundStatement, c.body)
        self.assertEqual(len(c.statements), 0)
        d = cast(IfStatement, s.chain[3])
        self.assertTrue(d.condition is None)
        self.assertTrue(isinstance(d.body, CompoundStatement))
        d = cast(CompoundStatement, d.body)
        self.assertEqual(len(d.statements), 0)

    def test_switch(self):
        t = Tokens("", "switch (a) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, SwitchStatement))
        s = cast(SwitchStatement, s)
        self.assertTrue(isinstance(s.condition, SymbolExpression))
        self.assertTrue(isinstance(s.body, CompoundStatement))
        s = cast(CompoundStatement, s.body)
        self.assertEqual(len(s.statements), 0)


class TestIterationStatements(TestCase):
    def test_while(self):
        t = Tokens("", "while (a) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, WhileStatement))
        s = cast(WhileStatement, s)
        self.assertTrue(isinstance(s.condition, SymbolExpression))
        self.assertTrue(isinstance(s.body, CompoundStatement))
        s = cast(CompoundStatement, s.body)
        self.assertEqual(len(s.statements), 0)

    def test_do_while(self):
        t = Tokens("", "do {} while (a);")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, DoWhileStatement))
        s = cast(DoWhileStatement, s)
        self.assertTrue(isinstance(s.condition, SymbolExpression))
        self.assertTrue(isinstance(s.body, CompoundStatement))
        s = cast(CompoundStatement, s.body)
        self.assertEqual(len(s.statements), 0)

    def test_for_with_declaration(self):
        t = Tokens("", "for (int i = 3; i < 100; i++) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ForStatement))
        s = cast(ForStatement, s)
        self.assertTrue(isinstance(s.initializer, DeclarationList))
        self.assertTrue(isinstance(s.condition, BinaryExpression))
        self.assertTrue(isinstance(s.increment, PostfixExpression))
        i = cast(DeclarationList, s.initializer)
        self.assertEqual(len(i.declarations), 1)
        i = cast(Declaration, s.initializer.declarations[0])
        self.assertEqual(i.specifiers.type_specifier, TypeSpecifier.INT)
        self.assertEqual(i.declarator.name.contents, "i")
        self.assertEqual(len(i.declarator.parts), 0)
        self.assertTrue(isinstance(i.initializer, ConstantExpression))
        c = cast(BinaryExpression, s.condition)
        self.assertEqual(c.operator, BinaryOperator.LT)
        self.assertTrue(isinstance(c.left, SymbolExpression))
        self.assertTrue(isinstance(c.right, ConstantExpression))
        i = cast(PostfixExpression, s.increment)
        self.assertEqual(i.operator, UnaryOperator.INC)
        self.assertTrue(isinstance(i.operand, SymbolExpression))

    def test_for_with_expression(self):
        t = Tokens("", "for (i = 3; i < 100; i++) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ForStatement))
        s = cast(ForStatement, s)
        self.assertTrue(isinstance(s.initializer, BinaryExpression))
        self.assertTrue(isinstance(s.condition, BinaryExpression))
        self.assertTrue(isinstance(s.increment, PostfixExpression))
        i = cast(BinaryExpression, s.initializer)
        self.assertEqual(i.operator, BinaryOperator.ASSIGN)
        self.assertTrue(isinstance(i.left, SymbolExpression))
        self.assertTrue(isinstance(i.right, ConstantExpression))
        c = cast(BinaryExpression, s.condition)
        self.assertEqual(c.operator, BinaryOperator.LT)
        self.assertTrue(isinstance(c.left, SymbolExpression))
        self.assertTrue(isinstance(c.right, ConstantExpression))
        i = cast(PostfixExpression, s.increment)
        self.assertEqual(i.operator, UnaryOperator.INC)
        self.assertTrue(isinstance(i.operand, SymbolExpression))

    def test_for_missing_initializer(self):
        t = Tokens("", "for (; i < 100; i++) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ForStatement))
        s = cast(ForStatement, s)
        self.assertTrue(s.initializer is None)
        self.assertTrue(isinstance(s.condition, BinaryExpression))
        self.assertTrue(isinstance(s.increment, PostfixExpression))
        c = cast(BinaryExpression, s.condition)
        self.assertEqual(c.operator, BinaryOperator.LT)
        self.assertTrue(isinstance(c.left, SymbolExpression))
        self.assertTrue(isinstance(c.right, ConstantExpression))
        i = cast(PostfixExpression, s.increment)
        self.assertEqual(i.operator, UnaryOperator.INC)
        self.assertTrue(isinstance(i.operand, SymbolExpression))

    def test_for_missing_condition(self):
        t = Tokens("", "for (i = 3;; i++) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ForStatement))
        s = cast(ForStatement, s)
        self.assertTrue(isinstance(s.initializer, BinaryExpression))
        self.assertTrue(s.condition is None)
        self.assertTrue(isinstance(s.increment, PostfixExpression))
        i = cast(BinaryExpression, s.initializer)
        self.assertEqual(i.operator, BinaryOperator.ASSIGN)
        self.assertTrue(isinstance(i.left, SymbolExpression))
        self.assertTrue(isinstance(i.right, ConstantExpression))
        i = cast(PostfixExpression, s.increment)
        self.assertEqual(i.operator, UnaryOperator.INC)
        self.assertTrue(isinstance(i.operand, SymbolExpression))

    def test_for_missing_increment(self):
        t = Tokens("", "for (i = 3; i < 100;) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ForStatement))
        s = cast(ForStatement, s)
        self.assertTrue(isinstance(s.initializer, BinaryExpression))
        self.assertTrue(isinstance(s.condition, BinaryExpression))
        self.assertTrue(s.increment is None)
        i = cast(BinaryExpression, s.initializer)
        self.assertEqual(i.operator, BinaryOperator.ASSIGN)
        self.assertTrue(isinstance(i.left, SymbolExpression))
        self.assertTrue(isinstance(i.right, ConstantExpression))
        c = cast(BinaryExpression, s.condition)
        self.assertEqual(c.operator, BinaryOperator.LT)
        self.assertTrue(isinstance(c.left, SymbolExpression))
        self.assertTrue(isinstance(c.right, ConstantExpression))

    def test_for_missing_everything(self):
        t = Tokens("", "for (;;) {}")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ForStatement))
        s = cast(ForStatement, s)
        self.assertTrue(s.initializer is None)
        self.assertTrue(s.condition is None)
        self.assertTrue(s.increment is None)


class TestJumpStatements(TestCase):
    def test_goto(self):
        t = Tokens("", "goto label;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, GotoStatement))
        s = cast(GotoStatement, s)
        self.assertEqual(s.name.type, Tk.IDENT)
        self.assertEqual(s.name.contents, "label")

    def test_continue(self):
        t = Tokens("", "continue;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ContinueStatement))

    def test_break(self):
        t = Tokens("", "break;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, BreakStatement))

    def test_return_nothing(self):
        t = Tokens("", "return;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ReturnStatement))
        s = cast(ReturnStatement, s)
        self.assertTrue(s.result is None)

    def test_return_expression(self):
        t = Tokens("", "return 3;")
        p = Parser(t)
        s = p.parse_statement()
        self.assertTrue(isinstance(s, ReturnStatement))
        s = cast(ReturnStatement, s)
        self.assertTrue(isinstance(s.result, ConstantExpression))


if __name__ == "__main__":
    unittest.main()
