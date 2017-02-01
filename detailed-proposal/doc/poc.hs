import Control.Monad.Reader
import Control.Monad.State
import Data.Traversable
import Debug.Trace

data DState = DState
  { stateFreshCounter :: Int
  , stateFreshTypeCounter :: Int
  } deriving (Show, Eq)

data DContext = DContext
  { contextScope :: [Function]
  , contextCurrentExpr :: Expr
  } deriving (Show, Eq)

type Discover = StateT DState (Reader DContext)

data Function = Function
  { fnName :: String -- Name
  , fnType :: Type
  } deriving (Show, Eq)

type Property = Function -- With a specific type, but let's not enforce that on the type level just yet.

data Constraint =
  TypeEq Type
         Type
  deriving (Eq)

instance Show Constraint where
  show (TypeEq t1 t2) = unwords [show t1, "~", show t2]

data Type
  = Const String
  | TyVar String
  | TyFun Type
          Type
  deriving (Eq)

instance Show Type where
  show (Const s) = s
  show (TyVar s) = s
  show (TyFun t1 t2) = unwords [show t1, "->", show t2]

data Expr
  = Var String
        Type
  | App Expr
        Expr
        Type
  | Lambda String
           Expr
           Type
  deriving (Eq)

instance Show Expr where
  show (Var s _) = s
  show (App e1 e2 _) = unwords ["(" ++ show e1, show e2 ++ ")"]
  show (Lambda v e _) = unwords ["\\" ++ v, "->", show e]

runDiscover :: Int -> [Function] -> Discover a -> a
runDiscover maxsize ps func = runReader (evalStateT func initS) initC
  where
    initS = DState {stateFreshCounter = 0, stateFreshTypeCounter = 0}
    initC =
      DContext
      { contextScope = ps
      , contextCurrentExpr =
          Lambda "x" (Var "y" $ Const "Bool") (TyFun (TyVar "a") (Const "Bool"))
      }

findProperties :: Int -> [Function] -> [(Expr, [Constraint])]
findProperties maxsize ps = runDiscover maxsize ps findProperties'

freshVarName :: Discover String
freshVarName = do
  modify $ \s -> s {stateFreshCounter = stateFreshCounter s + 1}
  (('x' :) . show) <$> gets stateFreshCounter

freshVarType :: Discover Type
freshVarType = do
  modify $ \s -> s {stateFreshTypeCounter = stateFreshTypeCounter s + 1}
  (Const . ('t' :) . show) <$> gets stateFreshTypeCounter

findProperties' :: Discover [(Expr, [Constraint])]
findProperties' = do
  vs <- terminalReplacement
  newExprs <- nonTerminalExpansion
  (((vs ++) . concat) <$>) $
    for newExprs $ \newE ->
      local (\c -> c {contextCurrentExpr = newE}) findProperties'

terminalReplacement :: Discover [(Expr, [Constraint])]
terminalReplacement = do
  nts <- nonTerminals
  ts <- terminals
  curE <- asks contextCurrentExpr
    -- For each possible set of assignments of terminals to non terminals, generate one output.
    -- [Terminal] -> [NonTerminal] -> [[(NonTerminal, Terminal)]] such that all non-terminals occur in each list
  pure $
    flip map (crosss $ replicate (length nts) ts) $ \ts ->
      let zipped = zip nts ts
          go e (nt, t) = replace e nt t
          consts = map (uncurry makeConstraint) zipped
      in (foldl go curE zipped, consts)

makeConstraint :: (String, Type) -> (String, Type) -> Constraint
makeConstraint (_, t1) (_, t2) = TypeEq t2 t1

replace :: Expr -> (String, Type) -> (String, Type) -> Expr
replace e (fn, ft) (tn, tt) = go e
  where
    go (Var s t)
      | s == fn && t == ft = Var tn tt
      | otherwise = Var s t
    go (App e1 e2 t) = App (go e1) (go e2) t
    go (Lambda v e t) = Lambda v (go e) t

cross :: [a] -> [b] -> [(a, b)]
cross as bs = [(a, b) | a <- as, b <- bs]

crosss :: [[a]] -> [[a]]
crosss [] = []
crosss [as] = map (: []) as
crosss (x:xs) = [a : as | a <- x, as <- crosss xs]

terminals :: Discover [(String, Type)]
terminals = do
  (Lambda v _ (TyFun t _)) <- asks contextCurrentExpr
  fs <- asks contextScope
  pure $ (v, t) : map (\(Function n t) -> (n, t)) fs

nonTerminals :: Discover [(String, Type)]
nonTerminals = do
  fs <- asks contextScope
  let go :: Expr -> [(String, Type)]
      go (Var n t)
        | n `notElem` map fnName fs = [(n, t)]
        | otherwise = []
      go (App e1 e2 _) = go e1 ++ go e2
      go (Lambda _ e _) = go e
  e <- asks contextCurrentExpr
  pure $ go e

nonTerminalExpansion :: Discover [Expr]
nonTerminalExpansion = do
  nts <- nonTerminals
  mapM expandNonTerminal nts

expandNonTerminal :: (String, Type) -> Discover Expr
expandNonTerminal (n, t) = do
  v1 <- freshVarName
  ft <- freshVarType
  v2 <- freshVarName
  let expVar = App (Var v1 $ TyFun ft t) (Var v2 ft) t
  let go :: Expr -> Expr
      go (Var vn vt)
        | vn == n && vt == t = expVar
        | otherwise = (Var vn vt)
      go (App e1 e2 t) = App (go e1) (go e2) t
      go (Lambda v e t) = Lambda v (go e) t
  curExp <- asks contextCurrentExpr
  pure $ go curExp

main :: IO ()
main =
  forM_ (take 50 $ test findProperties') $ \(p, cs) ->
    putStrLn $ unlines [show p, show cs]

test :: Discover a -> a
test = runDiscover 5 sortFuncs

sortFuncs :: [Function]
sortFuncs =
  [ Function "sort" (TyFun (Const "[Int]") (Const "[Int]"))
  , Function "sorted" (TyFun (Const "[Int]") (Const "Bool"))
  , Function
      "isPermutationOf"
      (TyFun (Const "[Int]") (TyFun (Const "[Int]") (Const "Bool")))
  ]
