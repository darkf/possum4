{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module TokenizerTests where
import Test.HUnit
import Tokenizer

-- Tokenizer tests

-- Empty
empty_test = TestCase $ do
	tokenize "" @?= []
	tokenize " " @?= []
	tokenize "   " @?= []
	tokenize "\t" @?= []

-- Identifiers
identifiers_test = TestCase $ do
	tokenize "x" @?= [Ident "x"]
	tokenize "xyz" @?= [Ident "xyz"]
	tokenize "xyz!" @?= [Ident "xyz!"]
	tokenize "xyz?" @?= [Ident "xyz?"]

identifier_stream = TestCase $ do
	tokenize "x y z" @?= [Ident "x", Ident "y", Ident "z"]
	tokenize "xyz abc? def" @?= [Ident "xyz", Ident "abc?", Ident "def"]

-- Numbers
numbers_test = TestCase $ do
	tokenize "123" @?= [Number 123.0]
	tokenize "123.0" @?= [Number 123.0]
	tokenize "123.5" @?= [Number 123.5]
	tokenize "123.75" @?= [Number 123.75]
	tokenize "1.5 2.3 4" @?= [Number 1.5, Number 2.3, Number 4]

-- Spaces
spaces_test = TestCase $ do
	tokenize "a     b  c     d" @?= [Ident "a", Ident "b", Ident "c", Ident "d"]
	tokenize "a     1  2.5     d" @?= [Ident "a", Number 1.0, Number 2.5, Ident "d"]
	tokenize "a  \t  1\t2.5     d" @?= [Ident "a", Number 1.0, Number 2.5, Ident "d"]

-- Parentheses
parens_test = TestCase $ do
	tokenize "(" @?= [LParen]
	tokenize ")" @?= [RParen]
	tokenize "()" @?= [LParen, RParen]
	tokenize " ( ) " @?= [LParen, RParen]
	tokenize "a(b)c" @?= [Ident "a", LParen, Ident "b", RParen, Ident "c"]

-- Mixing tokens test
mixed_test = TestCase $ do
	tokenize "x ab y 123.75 z 4" @?= [Ident "x", Ident "ab", Ident "y", Number 123.75, Ident "z", Number 4]
	tokenize "x ab y    123.75  z   4" @?= [Ident "x", Ident "ab", Ident "y", Number 123.75, Ident "z", Number 4]

-- Test group

tokenizerTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "identifiers" identifiers_test,
	  TestLabel "identifier_stream" identifier_stream,

	  TestLabel "numbers_test" numbers_test,

	  TestLabel "spaces_test" spaces_test,

	  TestLabel "parens_test" parens_test,

	  TestLabel "mixed_test" mixed_test
	]