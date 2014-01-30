{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

import Test.HUnit (runTestTT)
import TokenizerTests (tokenizerTests)
import ParserTests (parserTests)

main = do
	runTestTT tokenizerTests
	runTestTT parserTests
	return ()