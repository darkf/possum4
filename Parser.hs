module Parser where
import Data.Char (isSpace)

data Token = Ident String
		   | Str String
		   | Number Double
		   | LParen
		   | RParen
		   deriving (Show, Eq)

parse :: String -> [Token]
parse "" = []
parse (c:str)
	| c == ' ' || c == '\t' = parse str -- ignore spaces/tabs
	| otherwise =
		let (token,rest) = break isSpace (c:str) in
		Ident token : parse rest