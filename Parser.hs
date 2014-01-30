module Parser where
import AST
import Control.Monad.State
import qualified Data.Map as M
import qualified Tokenizer as T

type ArityMap = M.Map String Int

data ParserState = ParserState { arities :: ArityMap
							   , tokens :: [T.Token] -- remaining tokens
							   }
type StateP = State ParserState

-- Take n tokens from the stream, if non-empty
takeTokens :: Int -> StateP (Maybe [T.Token])
takeTokens n = do
	p@ParserState {tokens=tokens} <- get
	if length tokens < n then
		return Nothing
	else do
		let tokens' = take n tokens
		put $ p {tokens=drop n tokens}
		return $ Just tokens'

takeToken = takeTokens 1 >>= return . fmap head

lookupArity ident = do
	ParserState {arities=arities} <- get
	return $ M.lookup ident arities

-- Parse one expression
parseExpr :: StateP AST
parseExpr = do
	tok <- takeToken
	case tok of
		Just (T.Number n) -> return $ NumLit n
		Just (T.Ident ident) -> do
			arity' <- lookupArity ident
			case arity' of
				Just arity -> do
					-- parse bare application
					args <- replicateM arity parseExpr
					return $ Apply (Var ident) args
				Nothing -> return $ Var ident -- variable
		Just t -> error $ "parseExpr: unhandled token " ++ show t
		Nothing -> error "parseExpr: empty stream"

-- Parse the toplevel
parseTop :: StateP [AST]
parseTop = do
	ParserState {tokens=tokens} <- get
	if tokens == [] then return []
	else do
		expr <- parseExpr
		next <- parseTop
		return $ expr : next

parse' :: ArityMap -> [T.Token] -> [AST]
parse' arities tokens = evalState parseTop ParserState { arities=arities, tokens=tokens }

parse :: [T.Token] -> [AST]
parse = parse' M.empty

parseString :: String -> [AST]
parseString = parse . T.tokenize