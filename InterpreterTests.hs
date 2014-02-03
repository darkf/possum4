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
	interpret [StrLit "abc"] @??= Str "abc"

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

-- Defun test
defun_test = TestCase $ do
	-- defun f is end
	interpretWith [M.empty] [Defun "f" [] []] @??= Fn [] []
	-- defun f is end   f
	interpretWith [M.empty] [Defun "f" [] [], Var "f"] @??= Fn [] []
	-- defun f x is end   f
	interpretWith [M.empty] [Defun "f" [Var "x"] [], Var "f"] @??= Fn [Var "x"] []
	-- defun f x is y end   f
	interpretWith [M.empty] [Defun "f" [Var "x"] [Var "y"], Var "f"] @??= Fn [Var "x"] [Var "y"]

-- Application test
app_test = TestCase $ do
	-- just test that parameters are being passed and the function is being applied
	let env = [M.fromList [("f0", Fn [] [NumLit 1.0])
						  ,("f1", Fn [Var "x"] [NumLit 2.0])
						  ,("f2", Fn [Var "x", Var "y"] [NumLit 3.0])
						  ]]
	interpretWith env [Apply (Var "f0") []] @??= Number 1.0
	interpretWith env [Apply (Var "f1") [NumLit 1.0]] @??= Number 2.0
	interpretWith env [Apply (Var "f2") [NumLit 1.0, NumLit 2.0]] @??= Number 3.0

-- Application of Built-in Functions test
app_builtin = TestCase $ do
	interpret [Apply (Var "id") [NumLit 123.0]] @??= Number 123.0
	interpret [Apply (Var "+") [NumLit 1.0, NumLit 4.0]] @??= Number 5.0

interpreterTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test,

	  TestLabel "lookup_test" lookup_test,
	  TestLabel "lookup_scope_test" lookup_scope_test,
	  TestLabel "bind_test" bind_test,

	  TestLabel "defun_test" defun_test,

	  TestLabel "app_test" app_test,
	  TestLabel "app_test" app_builtin
	]