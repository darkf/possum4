import Test.HUnit (runTestTT)
import TokenizerTests (tokenizerTests)
import ParserTests (parserTests)

main = do
	runTestTT tokenizerTests
	runTestTT parserTests
	return ()