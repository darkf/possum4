{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module Interpreter where
import Control.Monad.State
import qualified Data.Map as M
import AST

data Value = Number Double
		   | Str String
		   | Nil
		   deriving (Show, Eq)

data InterpState = InterpState { }

type StateI = StateT InterpState IO

interpretNode :: AST -> StateI Value
interpretNode (NumLit x) = return $ Number x

interpret' :: [AST] -> StateI Value
interpret' = foldl (\m a -> m >> interpretNode a) (return Nil)

interpret :: [AST] -> IO Value
interpret ast = evalStateT (interpret' ast) InterpState {}