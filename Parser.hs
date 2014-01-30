module Parser where
import AST
import qualified Tokenizer as T

parse :: [T.Token] -> [AST]
parse [] = []
parse (t:ts) =
	case t of
		T.Number n -> NumLit n : parse ts
		T.Ident ident -> Var ident : parse ts
		_ -> error $ "unhandled token " ++ show t

parseString :: String -> [AST]
parseString = parse . T.tokenize