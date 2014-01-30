import Test.HUnit (runTestTT)
import ParserTests (parserTests)

main = runTestTT parserTests >> return ()