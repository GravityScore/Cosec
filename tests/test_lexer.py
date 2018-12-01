
# test_lexer.py
# By Ben Anderson
# December 2018

from unittest import TestCase
from lexer import Lexer, TokenType


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
        self.assert_token(tokens[0], TokenType.PLUS, 2, 1, 2, 2, "+")
        self.assert_token(tokens[1], TokenType.PLUS, 10, 1, 3, 1, "+")
        self.assert_token(tokens[2], TokenType.PLUS, 16, 1, 4, 4, "+")
        self.assert_token(tokens[3], TokenType.PLUS, 18, 1, 4, 6, "+")
        self.assert_token(tokens[4], TokenType.PLUS, 19, 1, 4, 7, "+")

    def test_arithmetic_tokens(self):
        lexer = Lexer("test.c", "+ - * /")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
        self.assert_token(tokens[0], TokenType.PLUS, 0, 1, 1, 1, "+")
        self.assert_token(tokens[1], TokenType.MINUS, 2, 1, 1, 3, "-")
        self.assert_token(tokens[2], TokenType.ASTERISK, 4, 1, 1, 5, "*")
        self.assert_token(tokens[3], TokenType.FSLASH, 6, 1, 1, 7, "/")

    def test_ident(self):
        lexer = Lexer("test.c", "hello _hello h3ll0 _123")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
        self.assert_token(tokens[0], TokenType.IDENT, 0, 5, 1, 1, "hello")
        self.assert_token(tokens[1], TokenType.IDENT, 6, 6, 1, 7, "_hello")
        self.assert_token(tokens[2], TokenType.IDENT, 13, 5, 1, 14, "h3ll0")
        self.assert_token(tokens[3], TokenType.IDENT, 19, 4, 1, 20, "_123")

    def test_types(self):
        lexer = Lexer("test.c", "void char short int long float double")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 7)
        self.assert_token(tokens[0], TokenType.TYPE, 0, 4, 1, 1, "void")
        self.assert_token(tokens[1], TokenType.TYPE, 5, 4, 1, 6, "char")
        self.assert_token(tokens[2], TokenType.TYPE, 10, 5, 1, 11, "short")
        self.assert_token(tokens[3], TokenType.TYPE, 16, 3, 1, 17, "int")
        self.assert_token(tokens[4], TokenType.TYPE, 20, 4, 1, 21, "long")
        self.assert_token(tokens[5], TokenType.TYPE, 25, 5, 1, 26, "float")
        self.assert_token(tokens[6], TokenType.TYPE, 31, 6, 1, 32, "double")

    def test_num(self):
        lexer = Lexer("test.c", "0 12 3141592653 123456789012345678901234567890")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
        self.assert_token(tokens[0], TokenType.NUM, 0, 1, 1, 1, "0")
        self.assert_token(tokens[1], TokenType.NUM, 2, 2, 1, 3, "12")
        self.assert_token(tokens[2], TokenType.NUM, 5, 10, 1, 6, "3141592653")
        self.assert_token(tokens[3], TokenType.NUM, 16, 30, 1, 17,
                          "123456789012345678901234567890")
