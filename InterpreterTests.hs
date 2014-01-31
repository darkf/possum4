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

-- Variable test
variable_test = TestCase $ do
	interpretWith [M.fromList [("x", Number 1.0)]] [Var "x"] @??= Number 1.0
	interpretWith [M.fromList [("x", Number 1.0), ("y", Number 2.0)]] [Var "y"] @??= Number 2.0

interpreterTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test,

	  TestLabel "variable_test" variable_test
	]