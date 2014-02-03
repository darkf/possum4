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
	| otherwise =
		let (token,rest) = splitIdentifier (c:str) in
		case tryParseNum token of
			Just num -> Number num : tokenize rest
			Nothing ->  Ident token : tokenize rest