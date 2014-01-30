module AST where

data AST = NumLit Double
		 | StrLit String
		 | Var String
		 | Def String AST
		 | Lambda [AST] [AST]
		 | Apply AST [AST]
		 deriving (Show, Eq)