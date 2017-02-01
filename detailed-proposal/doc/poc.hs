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

data TyConraint =
    TypeEq Type
           Type
    deriving (Eq)

instance Show TyConraint where
    show (TypeEq t1 t2) = unwords [show t1, "~", show t2]

data Type
    = TyCon String
    | TyVar String
    | TyFun Type
            Type
    deriving (Eq)

instance Show Type where
    show (TyCon s) = s
    show (TyVar s) = s
    show (TyFun t1 t2) = unwords ["(" ++ show t1, "->", show t2 ++ ")"]

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
              Lambda
                  "x"
                  (Var "y" $ TyCon "Bool")
                  (TyFun (TyVar "a") (TyCon "Bool"))
        }

findProperties :: Int -> [Function] -> [(Expr, [TyConraint])]
findProperties maxsize ps = runDiscover maxsize ps findProperties'

freshVarName :: Discover String
freshVarName = do
    modify $ \s -> s {stateFreshCounter = stateFreshCounter s + 1}
    (('x' :) . show) <$> gets stateFreshCounter

freshVarType :: Discover Type
freshVarType = do
    modify $ \s -> s {stateFreshTypeCounter = stateFreshTypeCounter s + 1}
    (TyCon . ('t' :) . show) <$> gets stateFreshTypeCounter

findProperties' :: Discover [(Expr, [TyConraint])]
findProperties' = do
    vs <- terminalReplacement
    newExprs <- nonTerminalExpansion
    (((vs ++) . concat) <$>) $
        for newExprs $ \newE ->
            local (\c -> c {contextCurrentExpr = newE}) findProperties'

terminalReplacement :: Discover [(Expr, [TyConraint])]
terminalReplacement = do
    nts <- nonTerminals
    ts <- terminals
    curE <- asks contextCurrentExpr
    -- For each possible set of assignments of terminals
    -- to non terminals, generate one output.
    pure $
        flip map (crosss $ replicate (length nts) ts) $ \ts ->
            let zipped = zip nts ts
                go e (nt, t) = replace e nt t
                consts = map (uncurry makeTyConraint) zipped
            in (foldl go curE zipped, consts)

makeTyConraint :: (String, Type) -> (String, Type) -> TyConraint
makeTyConraint (_, t1) (_, t2) = TypeEq t2 t1

replace :: Expr -> (String, Type) -> (String, Type) -> Expr
replace e (fn, ft) (tn, tt) = go e
  where
    go (Var s t)
        | s == fn && t == ft = Var tn tt
        | otherwise = Var s t
    go (App e1 e2 t) = App (go e1) (go e2) t
    go (Lambda v e t) = Lambda v (go e) t

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
    forM_ (take 50 $ runDiscover 5 sortFuncs findProperties') $ \(p, cs) ->
        putStrLn $ unlines [show p, show cs]

sortFuncs :: [Function]
sortFuncs =
    [ Function "sort" (TyFun (TyCon "[Int]") (TyCon "[Int]"))
    , Function "sorted" (TyFun (TyCon "[Int]") (TyCon "Bool"))
    , Function
          "isPermutationOf"
          (TyFun (TyCon "[Int]") (TyFun (TyCon "[Int]") (TyCon "Bool")))
    ]
