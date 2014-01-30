module ParserTests where
import qualified Data.Map as M
import Test.HUnit
import Tokenizer
import AST
import Parser

-- Parser tests

-- Empty
empty_test = TestCase $ do
	parse [] @?= []

-- Literals test
literals_test = TestCase $ do
	parse [Number 123.0] @?= [NumLit 123.0]
	parse [Ident "x"] @?= [Var "x"]

-- Bare application (no parentheses) with fixed arities test
bare_app_test = TestCase $ do
	let arities = M.fromList [("fn", 3), ("g", 2)]
	-- x fn 1 g 2 3 4 y
	parseWith arities [Ident "x",
					   Ident "fn",
					     Number 1.0,
					     Ident "g",
					       Number 2.0,
					       Number 3.0,
					     Number 4.0,
					     Ident "y"]
					  @?=
					  [Var "x",
					   Apply (Var "fn") [
					     NumLit 1.0,
					     Apply (Var "g") [NumLit 2.0, NumLit 3.0],
					     NumLit 4.0],
					   Var "y"
					   ]

defun_test = TestCase $ do
	-- defun foo is
	parse [Ident "defun", Ident "foo", Ident "is", Ident "end"] @?=
			[Defun "foo" [] []]
	-- defun foo x is
	parse [Ident "defun", Ident "foo", Ident "x", Ident "is", Ident "end"] @?=
			[Defun "foo" [Var "x"] []]
	-- defun foo x y z is
	parse [Ident "defun", Ident "foo", Ident "x", Ident "y", Ident "z", Ident "is", Ident "end"] @?=
			[Defun "foo" [Var "x", Var "y", Var "z"] []]

parserTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test,

	  TestLabel "bare_app_test" bare_app_test,

	  TestLabel "defun_test" defun_test
	]