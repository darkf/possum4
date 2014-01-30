{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module AST where

data AST = NumLit Double
		 | StrLit String
		 | Var String
		 | Def String AST
		 | Lambda [AST] [AST]
		 | Defun String [AST] [AST]
		 | Apply AST [AST]
		 deriving (Show, Eq)