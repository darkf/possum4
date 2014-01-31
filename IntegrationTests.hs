{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module IntegrationTests where
import Test.HUnit
import AST
import Parser
import Interpreter

runSource :: String -> IO Value
runSource = interpret . parseString

actual @??= expected = actual >>= (@?= expected)
source $$= expected = runSource source @??= expected

-- Empty test
empty_test = TestCase $ "" $$= Nil

-- Literals
literals_test = TestCase $ do
	"123" $$= Number 123.0
	"123.0" $$= Number 123.0
	"123.5" $$= Number 123.5

	"nil" $$= Nil

-- Built-in application
app_bif_test = TestCase $ do
	"id 123" $$= Number 123.0
	"+ 1 4" $$= Number 5.0

-- def
def_test = TestCase $ do
	"def x 123" $$= Number 123.0
	"def x 123\nx" $$= Number 123.0
	"def x + 1 4\n" $$= Number 5.0

-- defun
defun_test = TestCase $ do
	"defun f is end" $$= Fn [] []
	"defun f x is end" $$= Fn [Var "x"] []
	"defun f x is + 1 2 end" $$= Fn [Var "x"] [Apply (Var "+") [NumLit 1.0, NumLit 2.0]]

integrationTests = TestList [
	  TestLabel "empty_test" empty_test,

	  TestLabel "literals_test" literals_test,

	  TestLabel "app_bif_test" app_bif_test,

	  TestLabel "def_test" def_test,
	  TestLabel "defun_test" defun_test
	]