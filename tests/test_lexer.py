
# test_lexer.py
# By Ben Anderson
# December 2018

from unittest import TestCase
from lexer import Lexer


class TestLexer(TestCase):
    def assert_token(self, token, type, start, length, line_num, column_num,
                     contents):
        self.assertEqual(token.type, type)
        self.assertEqual(token.start, start)
        self.assertEqual(token.length, length)
        self.assertEqual(len(token.contents), length)
        self.assertEqual(token.line_num, line_num)
        self.assertEqual(token.column_num, column_num)
        self.assertEqual(token.contents, contents)

    def test_whitespace(self):
        lexer = Lexer("test.c", "\n + \t\t   \n+\r\n  \t+\t++\n\n")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 5)
        self.assert_token(tokens[0], "+", 2, 1, 2, 2, "+")
        self.assert_token(tokens[1], "+", 10, 1, 3, 1, "+")
        self.assert_token(tokens[2], "+", 16, 1, 4, 4, "+")
        self.assert_token(tokens[3], "+", 18, 1, 4, 6, "+")
        self.assert_token(tokens[4], "+", 19, 1, 4, 7, "+")

    def test_arithmetic_tokens(self):
        lexer = Lexer("test.c", "+ - * /")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
        self.assert_token(tokens[0], "+", 0, 1, 1, 1, "+")
        self.assert_token(tokens[1], "-", 2, 1, 1, 3, "-")
        self.assert_token(tokens[2], "*", 4, 1, 1, 5, "*")
        self.assert_token(tokens[3], "/", 6, 1, 1, 7, "/")

    def test_ident(self):
        lexer = Lexer("test.c", "hello _hello h3ll0 _123")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
        self.assert_token(tokens[0], "identifier", 0, 5, 1, 1, "hello")
        self.assert_token(tokens[1], "identifier", 6, 6, 1, 7, "_hello")
        self.assert_token(tokens[2], "identifier", 13, 5, 1, 14, "h3ll0")
        self.assert_token(tokens[3], "identifier", 19, 4, 1, 20, "_123")

    def test_types(self):
        lexer = Lexer("test.c", "void char short int long float double")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 7)
        self.assert_token(tokens[0], "void", 0, 4, 1, 1, "void")
        self.assert_token(tokens[1], "char", 5, 4, 1, 6, "char")
        self.assert_token(tokens[2], "short", 10, 5, 1, 11, "short")
        self.assert_token(tokens[3], "int", 16, 3, 1, 17, "int")
        self.assert_token(tokens[4], "long", 20, 4, 1, 21, "long")
        self.assert_token(tokens[5], "float", 25, 5, 1, 26, "float")
        self.assert_token(tokens[6], "double", 31, 6, 1, 32, "double")

    def test_num(self):
        lexer = Lexer("t.c", "0 12 3141592653 123456789012345678901234567890")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
        self.assert_token(tokens[0], "number", 0, 1, 1, 1, "0")
        self.assert_token(tokens[1], "number", 2, 2, 1, 3, "12")
        self.assert_token(tokens[2], "number", 5, 10, 1, 6, "3141592653")
        self.assert_token(tokens[3], "number", 16, 30, 1, 17,
                          "123456789012345678901234567890")
