
# test_ast.py
# By Ben Anderson
# December 2018

from unittest import TestCase
from ast import BuiltInType, FuncDefArg, FuncDef
from lexer import Lexer, TokenSequence


class TestAstGenerator(TestCase):
    def test_func_def(self):
        lexer = Lexer("t.c", "void hi() {} int hi2(int arg1) {}"
                             "unsigned int hi3(char arg1, long arg2) {}")
        tokens = lexer.tokenize()
        tk = TokenSequence(tokens)
        fns = [FuncDef(tk), FuncDef(tk), FuncDef(tk)]
        self.assertEqual(fns[0].name.contents, "hi")
        self.assertEqual(fns[1].name.contents, "hi2")
        self.assertEqual(fns[2].name.contents, "hi3")
        self.assertEqual(fns[0].return_type.type, "void")
        self.assertEqual(fns[1].return_type.type, "int")
        self.assertEqual(fns[2].return_type.type, "uint")
        self.assertEqual(len(fns[0].args), 0)
        self.assertEqual(len(fns[1].args), 1)
        self.assertEqual(len(fns[2].args), 2)
        self.assertEqual(fns[1].args[0].name.contents, "arg1")
        self.assertEqual(fns[1].args[0].type.type, "int")
        self.assertEqual(fns[2].args[0].name.contents, "arg1")
        self.assertEqual(fns[2].args[0].type.type, "char")
        self.assertEqual(fns[2].args[1].name.contents, "arg2")
        self.assertEqual(fns[2].args[1].type.type, "int")

    def test_func_def_arg(self):
        lexer = Lexer("t.c", "char c unsigned long _hi signed there")
        tokens = lexer.tokenize()
        tk = TokenSequence(tokens)
        args = [FuncDefArg(tk), FuncDefArg(tk), FuncDefArg(tk)]
        self.assertEqual(args[0].type.type, "char")
        self.assertEqual(args[1].type.type, "uint")
        self.assertEqual(args[2].type.type, "int")
        self.assertEqual(args[0].name.contents, "c")
        self.assertEqual(args[1].name.contents, "_hi")
        self.assertEqual(args[2].name.contents, "there")

    def test_built_in_types(self):
        lexer = Lexer("t.c",
                      "char signed char unsigned char\n"
                      "short short int signed short signed short int "
                      "unsigned short unsigned short int\n"
                      "int signed signed int unsigned unsigned int\n"
                      "long int long signed long int unsigned long int\n"
                      "long long long long int signed long long signed long "
                      "long int unsigned long long unsigned long long int\n"
                      "float double long double")
        correct = ["char", "char", "uchar", "short", "short", "short", "short",
                   "ushort", "ushort", "int", "int", "int", "uint", "uint",
                   "int", "int", "int", "uint", "llong", "llong", "llong",
                   "llong", "ullong", "ullong", "float", "double", "double"]
        tokens = lexer.tokenize()
        tk = TokenSequence(tokens)
        for expected in correct:
            actual = BuiltInType(tk)
            self.assertEqual(actual.type, expected)
        self.assertTrue(tk.cur() is None)
