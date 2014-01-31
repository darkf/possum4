{- Part of possum4
   Copyright (c) 2014 darkf
   Licensed under the terms of the MIT license
   See LICENSE.txt for details -}

module Interpreter where
import Prelude hiding (lookup)
import Control.Monad.State
import qualified Data.Map as M
import AST

data Value = Number Double
		   | Str String
		   | Fn [AST] [AST]
		   | Nil
		   deriving (Show, Eq)

type Env = [M.Map String Value] -- last is always global

data InterpState = InterpState { env :: Env }

type StateI = StateT InterpState IO

initialGlobalEnv = M.fromList [ ("nil", Nil) ]

-- look up a binding from the bottom up
lookup :: Env -> String -> Maybe Value
lookup [] _ = Nothing
lookup (env:xs) name =
	maybe (lookup xs name) Just (M.lookup name env)

-- bind in the local environment
bind :: Env -> String -> Value -> Env
bind (env:xs) name value = (M.insert name value env):xs

interpretNode :: AST -> StateI Value
interpretNode (NumLit x) = return $ Number x
interpretNode (Var x) = do
	InterpState {env=env} <- get
	maybe (error $ "unbound variable " ++ x) return (lookup env x)
interpretNode (Def name v) = do
	p@InterpState {env=env} <- get
	val <- interpretNode v
	put $ p {env=bind env name val}
	return val
interpretNode (Defun name args body) = do
	p@InterpState {env=env} <- get
	let fn = Fn args body
	put $ p {env=bind env name fn}
	return fn

interpret' :: [AST] -> StateI Value
interpret' = foldl (\m a -> m >> interpretNode a) (return Nil)

interpret :: [AST] -> IO Value
interpret ast = evalStateT (interpret' ast) InterpState {env=[initialGlobalEnv]}

interpretWith :: Env -> [AST] -> IO Value
interpretWith env ast = evalStateT (interpret' ast) InterpState {env=env}