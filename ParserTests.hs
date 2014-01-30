module ParserTests where
import Test.HUnit
import Tokenizer
import AST
import Parser

-- Parser tests

-- Empty
empty_test = TestCase $ do
	parse [] @?= []

literals_test = TestCase $ do
	parse [Number 123.0] @?= [NumLit 123.0]
	parse [Ident "x"] @?= [Var "x"]

parserTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test
	]