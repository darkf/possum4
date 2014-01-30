module ParserTests where
import Test.HUnit
import Parser

-- Tokenizer tests

identifiers_test = TestCase $ do
	parse "x" @?= [Ident "x"]
	parse "xyz" @?= [Ident "xyz"]
	parse "xyz!" @?= [Ident "xyz!"]
	parse "xyz?" @?= [Ident "xyz?"]

identifier_stream = TestCase $ do
	parse "x y z" @?= [Ident "x", Ident "y", Ident "z"]
	parse "xyz abc? def" @?= [Ident "xyz", Ident "abc?", Ident "def"]

-- Test group

parserTests = TestList [
	  TestLabel "identifiers" identifiers_test,
	  TestLabel "identifier_stream" identifier_stream
	]