module REPL where
import Parser (parseStringWith)
import Interpreter (StateI, currentArities, interpret', exec)
import System.IO (stdout, hFlush, readFile)
import Control.Monad.IO.Class (liftIO)

repl :: StateI ()
repl = do
	liftIO $ putStr "> " >> hFlush stdout
	line <- liftIO getLine
	arities <- currentArities
	case line of
		":q" -> return ()
		':':'l':' ':filename -> do
			-- load filename
			file <- liftIO $ readFile filename
			interpret' $ parseStringWith arities file
			repl
		_ -> do
			val <- interpret' $ parseStringWith arities line
			liftIO $ print val
			repl

main = exec repl >> return ()