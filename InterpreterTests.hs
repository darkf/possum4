{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module InterpreterTests where
import qualified Data.Map as M
import Test.HUnit
import AST
import Interpreter

-- Interpreter tests

actual @??= expected = actual >>= (@?= expected)

-- Empty
empty_test = TestCase $ do
	interpret [] @??= Nil

-- Literals test
literals_test = TestCase $ do
	interpret [NumLit 123.0] @??= Number 123.0

-- Variable lookup test
lookup_test = TestCase $ do
	interpretWith [M.fromList [("x", Number 1.0)]] [Var "x"] @??= Number 1.0
	interpretWith [M.fromList [("x", Number 1.0), ("y", Number 2.0)]] [Var "y"] @??= Number 2.0

-- Scoped variable lookup test
lookup_scope_test = TestCase $ do
	let env = [M.fromList [("x", Number 1.0)] -- scope 1
			  ,M.fromList [("y", Number 2.0)] -- scope 2
			  ,M.fromList [] -- global scope
			  ]
	interpretWith env [Var "x"] @??= Number 1.0
	interpretWith env [Var "y"] @??= Number 2.0

-- Variable binding test
bind_test = TestCase $ do
	-- def x 123   x
	interpretWith [M.empty] [Def "x" (NumLit 123.0), Var "x"] @??= Number 123.0
	-- def x 123   def y 234   y
	interpretWith [M.empty] [Def "x" (NumLit 123.0),
							 Def "y" (NumLit 234.0),
							 Var "y"] @??= Number 234.0

interpreterTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test,

	  TestLabel "lookup_test" lookup_test,
	  TestLabel "lookup_scope_test" lookup_scope_test,
	  TestLabel "bind_test" bind_test
	]