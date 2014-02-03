{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module Tokenizer where
import Data.Char (isSpace)

data Token = Ident String
		   | Str String
		   | Number Double
		   | LParen
		   | RParen
		   deriving (Show, Eq)

isIdentifierChar :: Char -> Bool
isIdentifierChar = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!?~@#$%^&*-_=+{}[]|'.,/")

splitIdentifier :: String -> (String, String)
splitIdentifier = span isIdentifierChar

tokenizeString :: Bool -> String -> Int -> (String, Int)
tokenizeString _ "" i = ("", i)
tokenizeString isEscape (c:str) i =
	if isEscape then case c of
			_ | c `elem` ['\\','\"'] -> c `plus` tokenizeString False str (i+1)
			'n' -> '\n' `plus` tokenizeString False str (i+1)
			'r' -> '\r' `plus` tokenizeString False str (i+1)
			't' -> '\t' `plus` tokenizeString False str (i+1)
			_ -> error $ "tokenizeString: invalid escape char: " ++ [c]
	else case c of
			'\\' -> tokenizeString True str (i+1)
			'\"' -> ("", i+1) -- end of string literal
			_ -> c `plus` tokenizeString False str (i+1)
	where plus c (s, len) = (c:s, len)

tryParseNum :: String -> Maybe Double
tryParseNum str =
	case reads str of
		[(x, "")] -> Just x
		_ -> Nothing

tokenize :: String -> [Token]
tokenize "" = []
tokenize (c:str)
	| isSpace c = tokenize str -- ignore spaces/tabs
	| c == '(' = LParen : tokenize str
	| c == ')' = RParen : tokenize str
	| c == '"' = let (token,len) = tokenizeString False str 0 in
		Str token : tokenize (drop len str)
	| otherwise =
		let (token,rest) = splitIdentifier (c:str) in
		case tryParseNum token of
			Just num -> Number num : tokenize rest
			Nothing ->  Ident token : tokenize rest