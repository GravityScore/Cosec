
# tests/test_lexer.py
# By Ben Anderson
# December 2018

from unittest import TestCase
from lexer import Tokens


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
        seq = Tokens("", "\n + \t\t   \n+\r\n  \t+\t+ +\n\n")
        self.assert_token(seq.cur(), "+", 2, 1, 2, 2, "+")
        seq.next()
        self.assert_token(seq.cur(), "+", 10, 1, 3, 1, "+")
        seq.next()
        self.assert_token(seq.cur(), "+", 16, 1, 4, 4, "+")
        seq.next()
        self.assert_token(seq.cur(), "+", 18, 1, 4, 6, "+")
        seq.next()
        self.assert_token(seq.cur(), "+", 20, 1, 4, 8, "+")
        seq.next()
        self.assertTrue(seq.cur() is None)

    def test_syntax(self):
        seq = Tokens("", "+ - > >> >>=")
        self.assert_token(seq.cur(), "+", 0, 1, 1, 1, "+")
        seq.next()
        self.assert_token(seq.cur(), "-", 2, 1, 1, 3, "-")
        seq.next()
        self.assert_token(seq.cur(), ">", 4, 1, 1, 5, ">")
        seq.next()
        self.assert_token(seq.cur(), ">>", 6, 2, 1, 7, ">>")
        seq.next()
        self.assert_token(seq.cur(), ">>=", 9, 3, 1, 10, ">>=")
        seq.next()
        self.assertTrue(seq.cur() is None)

    def test_ident(self):
        seq = Tokens("", "hello _hello h3ll0 _123")
        self.assert_token(seq.cur(), "identifier", 0, 5, 1, 1, "hello")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 6, 6, 1, 7, "_hello")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 13, 5, 1, 14, "h3ll0")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 19, 4, 1, 20, "_123")
        seq.next()
        self.assertTrue(seq.cur() is None)

    def test_keywords(self):
        seq = Tokens("", "for if forextra extrafor _for for_")
        self.assert_token(seq.cur(), "for", 0, 3, 1, 1, "for")
        seq.next()
        self.assert_token(seq.cur(), "if", 4, 2, 1, 5, "if")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 7, 8, 1, 8, "forextra")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 16, 8, 1, 17, "extrafor")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 25, 4, 1, 26, "_for")
        seq.next()
        self.assert_token(seq.cur(), "identifier", 30, 4, 1, 31, "for_")
        seq.next()
        self.assertTrue(seq.cur() is None)

    def test_types(self):
        seq = Tokens("", "void char short int long float double")
        self.assert_token(seq.cur(), "void", 0, 4, 1, 1, "void")
        seq.next()
        self.assert_token(seq.cur(), "char", 5, 4, 1, 6, "char")
        seq.next()
        self.assert_token(seq.cur(), "short", 10, 5, 1, 11, "short")
        seq.next()
        self.assert_token(seq.cur(), "int", 16, 3, 1, 17, "int")
        seq.next()
        self.assert_token(seq.cur(), "long", 20, 4, 1, 21, "long")
        seq.next()
        self.assert_token(seq.cur(), "float", 25, 5, 1, 26, "float")
        seq.next()
        self.assert_token(seq.cur(), "double", 31, 6, 1, 32, "double")
        seq.next()
        self.assertTrue(seq.cur() is None)

    def test_num(self):
        seq = Tokens("", "0 12 3141592653 12345678901234567890123456789")
        self.assert_token(seq.cur(), "number", 0, 1, 1, 1, "0")
        seq.next()
        self.assert_token(seq.cur(), "number", 2, 2, 1, 3, "12")
        seq.next()
        self.assert_token(seq.cur(), "number", 5, 10, 1, 6, "3141592653")
        seq.next()
        self.assert_token(seq.cur(), "number", 16, 29, 1, 17,
                          "12345678901234567890123456789")
        seq.next()
        self.assertTrue(seq.cur() is None)
