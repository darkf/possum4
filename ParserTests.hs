{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

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

def_test = TestCase $ do
	-- def x 123
	parse [Ident "def", Ident "x", Number 123.0] @?= [Def "x" (NumLit 123.0)]
	-- def foo (+ 1 2)
	parse [Ident "def", Ident "foo", LParen, Ident "+", Number 1.0, Number 2.0, RParen]
		@?= [Def "foo" (Apply (Var "+") [NumLit 1.0, NumLit 2.0])]

defun_test = TestCase $ do
	-- defun foo is end
	parse [Ident "defun", Ident "foo", Ident "is", Ident "end"] @?=
			[Defun "foo" [] []]
	-- defun foo x is end
	parse [Ident "defun", Ident "foo", Ident "x", Ident "is", Ident "end"] @?=
			[Defun "foo" [Var "x"] []]
	-- defun foo x y z is end
	parse [Ident "defun", Ident "foo", Ident "x", Ident "y", Ident "z", Ident "is", Ident "end"] @?=
			[Defun "foo" [Var "x", Var "y", Var "z"] []]

	-- with bodies

	-- defun foo is 123 end
	parse [Ident "defun", Ident "foo", Ident "is", Number 123.0, Ident "end"] @?=
			[Defun "foo" [] [NumLit 123.0]]
	-- defun foo x y is x end
	parse [Ident "defun", Ident "foo",  Ident "x", Ident "y", Ident "is", Ident "x", Ident "end"] @?=
			[Defun "foo" [Var "x", Var "y"] [Var "x"]]
	-- defun foo x y is + x y end
	parseWith (M.fromList [("+", 2)]) [Ident "defun", Ident "foo", Ident "x", Ident "y", Ident "is", Ident "+", Ident "x", Ident "y", Ident "end"] @?=
			[Defun "foo" [Var "x", Var "y"] [Apply (Var "+") [Var "x", Var "y"]]]

multiple_defun_test = TestCase $ do
	parse [ Ident "defun", Ident "foo", Ident "is", Ident "end",
			Ident "defun", Ident "plus", Ident "x", Ident "y", Ident "is", Ident "end",
			Ident "defun", Ident "bar", Ident "x", Ident "is", Ident "plus", Ident "x", Number 1.0, Ident "end",
			Ident "foo",
			Ident "plus", Number 1.0, Number 2.0,
			Ident "bar", Number 5.0
		  ] @?= [
		  	Defun "foo" [] [],
		  	Defun "plus" [Var "x", Var "y"] [],
		  	Defun "bar" [Var "x"] [Apply (Var "plus") [Var "x", NumLit 1.0]],
		  	Apply (Var "foo") [],
		  	Apply (Var "plus") [NumLit 1.0, NumLit 2.0],
		  	Apply (Var "bar") [NumLit 5.0]
		  ]

-- Parenthesized applications
paren_apply_test = TestCase $ do
	-- (f x 123 y)
	parse [LParen, Ident "f", Ident "x", Number 123.0, Ident "y", RParen]
			@?= [Apply (Var "f") [Var "x", NumLit 123.0, Var "y"]]

	-- (f (g x y) z)
	parse [LParen, Ident "f",
				LParen, Ident "g", Ident "x", Ident "y", RParen,
				Ident "z", RParen]
			@?= [Apply (Var "f") [
					Apply (Var "g") [Var "x", Var "y"],
					Var "z"
				]]

-- `if` expressions
if_test = TestCase $ do
	-- if x y
	parse [Ident "if", Ident "x", Ident "y"] @?=
			[If (Var "x") (Var "y") Nothing]
	-- if id x id y
	parseWith (M.fromList [("id", 1)]) [Ident "if", Ident "id", Ident "x", Ident "id", Ident "y"] @?=
			[If (Apply (Var "id") [Var "x"]) (Apply (Var "id") [Var "y"]) Nothing]
	-- if x y else z
	parse [Ident "if", Ident "x", Ident "y", Ident "else", Ident "z"] @?=
			[If (Var "x") (Var "y") (Just $ Var "z")]
	-- if id x id y else id z
	parseWith (M.fromList [("id", 1)]) [Ident "if", Ident "id", Ident "x", Ident "id", Ident "y", Ident "else", Ident "id", Ident "z"] @?=
			[If (Apply (Var "id") [Var "x"]) (Apply (Var "id") [Var "y"])
				(Just (Apply (Var "id") [Var "z"]))]

parserTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test,

	  TestLabel "bare_app_test" bare_app_test,

	  TestLabel "def_test" def_test,

	  TestLabel "defun_test" defun_test,
	  TestLabel "multiple_defun_test" multiple_defun_test,
	  TestLabel "paren_apply_test" paren_apply_test,

	  TestLabel "if_test" if_test
	]