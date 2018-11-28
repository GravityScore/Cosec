
# test_lexer.py
# By Ben Anderson
# December 2018

from unittest import TestCase

import sys
sys.path.insert(0, "../src")

from lexer import Lexer


class TestLexer(TestCase):
    def test_arithmetic_tokens(self):
        lexer = Lexer("+ - * /")
        tokens = lexer.tokenize()
        self.assertEqual(len(tokens), 4)
