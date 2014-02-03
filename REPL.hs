module REPL where
import Parser (parseStringWith)
import Interpreter (StateI, currentArities, interpret', exec)
import System.IO (stdout, hFlush)
import Control.Monad.IO.Class (liftIO)

repl :: StateI ()
repl = do
	liftIO $ putStr "> " >> hFlush stdout
	line <- liftIO getLine
	arities <- currentArities
	val <- interpret' $ parseStringWith arities line
	liftIO $ print val
	repl

main = exec repl >> return ()