import Test.HUnit (runTestTT)
import TokenizerTests (tokenizerTests)

main = runTestTT tokenizerTests >> return ()