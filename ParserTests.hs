module ParserTests where
import Test.HUnit
import Parser

-- Tokenizer tests

-- Empty
empty_test = TestCase $ do
	parse "" @?= []
	parse " " @?= []
	parse "   " @?= []
	parse "\t" @?= []

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

-- Spaces
spaces_test = TestCase $ do
	parse "a     b  c     d" @?= [Ident "a", Ident "b", Ident "c", Ident "d"]
	parse "a     1  2.5     d" @?= [Ident "a", Number 1.0, Number 2.5, Ident "d"]
	parse "a  \t  1\t2.5     d" @?= [Ident "a", Number 1.0, Number 2.5, Ident "d"]

-- Mixing tokens test
mixed_test = TestCase $ do
	parse "x ab y 123.75 z 4" @?= [Ident "x", Ident "ab", Ident "y", Number 123.75, Ident "z", Number 4]
	parse "x ab y    123.75  z   4" @?= [Ident "x", Ident "ab", Ident "y", Number 123.75, Ident "z", Number 4]

-- Test group

parserTests = TestList [
	  TestLabel "empty_test" empty_test,
	  
	  TestLabel "identifiers" identifiers_test,
	  TestLabel "identifier_stream" identifier_stream,

	  TestLabel "numbers_test" numbers_test,

	  TestLabel "spaces_test" spaces_test,

	  TestLabel "mixed_test" mixed_test
	]