module REPL where
import Parser (parseStringWith)
import Interpreter (StateI, currentArities, interpret', exec)
import System.IO (stdout, hFlush, readFile)
import Control.Monad.IO.Class (liftIO)

repl :: Bool -> StateI ()
repl showAST = do
	liftIO $ putStr "> " >> hFlush stdout
	line <- liftIO getLine
	arities <- currentArities
	case line of
		":q" -> return ()
		":ast" -> repl (not showAST)
		':':'l':' ':filename -> do
			-- load filename
			file <- liftIO $ readFile filename
			let ast = parseStringWith arities file
			if showAST then liftIO $ print ast else return ()
			interpret' ast
			repl showAST
		_ -> do
			let ast = parseStringWith arities line
			if showAST then liftIO $ print ast else return ()
			val <- interpret' ast
			liftIO $ print val
			repl showAST

main = exec (repl False) >> return ()