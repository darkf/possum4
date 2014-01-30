module ParserTests where
import Test.HUnit
import Parser

-- Tokenizer tests

-- Identifiers
identifiers_test = TestCase $ do
	parse "x" @?= [Ident "x"]
	parse "xyz" @?= [Ident "xyz"]
	parse "xyz!" @?= [Ident "xyz!"]
	parse "xyz?" @?= [Ident "xyz?"]

identifier_stream = TestCase $ do
	parse "x y z" @?= [Ident "x", Ident "y", Ident "z"]
	parse "xyz abc? def" @?= [Ident "xyz", Ident "abc?", Ident "def"]

-- Numbers
numbers_test = TestCase $ do
	parse "123" @?= [Number 123.0]
	parse "123.0" @?= [Number 123.0]
	parse "123.5" @?= [Number 123.5]
	parse "123.75" @?= [Number 123.75]
	parse "1.5 2.3 4" @?= [Number 1.5, Number 2.3, Number 4]

-- Test group

parserTests = TestList [
	  TestLabel "identifiers" identifiers_test,
	  TestLabel "identifier_stream" identifier_stream,

	  TestLabel "numbers_test" numbers_test
	]