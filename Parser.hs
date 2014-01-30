module Parser (parse, parseWith, parseString, parseStringWith, ArityMap) where
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
takeTokenUnsafe = do
	tok <- takeToken
	case tok of
		Just t -> return t
		Nothing -> error "takeTokenUnsafe: wanted one token, but got end of stream"

-- Takes tokens until the token applied to the predicate is True. Also takes the matching token.
takeTokensUntil :: (T.Token -> Bool) -> StateP [T.Token]
takeTokensUntil predicate = do
	p@ParserState {tokens=tokens} <- get
	let (taken, _:tokens') = break predicate tokens
	put $ p {tokens=tokens'}
	return taken

takeIdentifier = do
	tok <- takeToken
	case tok of
		Just (T.Ident s) -> return s
		Just t -> error $ "takeIdentifier: expected identifier, got " ++ show t
		Nothing -> error "takeIdentifier: expected identifier, got end of stream"

peekToken = do
	ParserState {tokens=tokens} <- get
	return $ head tokens

lookupArity ident = do
	ParserState {arities=arities} <- get
	return $ M.lookup ident arities

bindArity ident arity = do
	p@ParserState {arities=arities} <- get
	let arities' = M.insert ident arity arities
	put $ p {arities=arities'}

expectToken token = do
	tok <- takeToken
	case tok of
		Just t | t == token -> return ()
		Just t -> error $ "parser: expected " ++ show token ++ " but got " ++ show t
		Nothing -> error $ "parser: expected " ++ show token ++ " but got end of stream"

-- Continually parse expressions until the predicate applied with current token is True. Consumes the matching token.
parseUntil predicate = do
	tok <- peekToken
	case tok of
		t | predicate t -> takeToken >> return []
		_ -> do
			expr <- parseExpr
			next <- parseUntil predicate
			return $ expr : next

-- Parses a defun
-- defun f x y z is body end
parseDefun :: StateP AST
parseDefun = do
	name <- takeIdentifier
	args' <- takeTokensUntil (== T.Ident "is")
	let args = map (\(T.Ident i) -> Var i) args'
	let arity = length args
	bindArity name arity -- bind arity so that we can parse calls (and before the body so self-reference works)
	body <- parseUntil (== T.Ident "end")
	return $ Defun name args body

-- Parse one expression
parseExpr :: StateP AST
parseExpr = do
	tok <- takeToken
	case tok of
		Just (T.Number n) -> return $ NumLit n
		Just (T.Ident "defun") -> parseDefun
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

parseWith :: ArityMap -> [T.Token] -> [AST]
parseWith = parse'

parseStringWith :: ArityMap -> String -> [AST]
parseStringWith arities = parseWith arities . T.tokenize

parseString :: String -> [AST]
parseString = parse . T.tokenize