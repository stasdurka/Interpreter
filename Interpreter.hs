{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use when" #-}
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Data.Maybe(fromMaybe)
-- import Data.Either(fromRight)
import Control.Monad (foldM)
import AbsMojeLatte

--
-- A syntax tree type for simple math, with variables
--
-- data Exp = IntE Int
--          | OpE  Op Exp Exp
--          | VarE String
--          | LetE String Exp Exp 

-- data Decl = VarD String Exp -- var x=e

-- data Stmt = S               -- skip
--         | AS String Exp     -- x:= e
--         | SeqS Stmt Stmt    -- S1; S2
--         | IfS Exp Stmt Stmt -- if b then S1 else S2 
--         | WhileS Exp Stmt   -- while b do S
--         | Block [Decl] Stmt -- begin [D] S end


-- type Op = Int -> Int -> Int

type Loc = Integer

-- type VEnv = M.Map String Loc
-- type FEnv = M.Map Ident [Ident]     -- fname -> [arg_names]
-- type Env = (VEnv, FEnv)

-- getVenv :: RSE a -> Env
-- getVenv = do
--     env <- ask
--     return $ fst env
type Env = M.Map String Loc

type Store = M.Map Loc Val

type RSE a = ReaderT Env (StateT Store (ExceptT String Identity)) a

type FuncDef = (Stmt, [String])    -- [Val] == arg names



data Val = IntVal Integer | BoolVal Bool | StrVal String | FunVal FuncDef
    deriving Show

-- Env -> Store -> Either String (a, Store)
{-

ReaderT r m a  ===   r -> m a
StateT s m a   ===   s -> m (a,s)
ExceptT e m a  ===   (m (Either e a))

a => Either String a
a => Store -> Either String (a, Store)
a => Env -> Store -> Either String (a, Store)

----

type ERS a = ExceptT String (ReaderT Env (StateT Store Identity)) a

ExceptT e m a  ===   (m (Either e a))
ReaderT r m a  ===   r -> m a
StateT s m a   ===   s -> m (a,s)

a => Store -> (a, Store)
a => Env -> Store -> (a, Store)
a => Env -> Store -> (Either String a, Store)


-}
evalRelOp GTH e1 e2 = e1 > e2
evalRelOp GE e1 e2 = e1 >= e2
evalRelOp LTH e1 e2 = e1 < e2
evalRelOp EQU e1 e2 = e1 == e2
evalRelOp NE e1 e2 = e1 /= e2
evalRelOp LE e1 e2 = e1 <= e2

evalMulOp Div e1 e2 = div e1 e2
evalMulOp Times e1 e2 = e1 * e2
evalMulOp Mod e1 e2 = e1 `mod` e2

evalAddOp Minus e1 e2 = e1 - e2
evalAddOp Plus e1 e2 = e1 + e2

newloc :: Store -> Loc
newloc m = if (M.null m) then 0 
          else let (i, w) = M.findMax m in i+1  

newloc' :: RSE Loc
newloc' = do 
  m <- get 
  if (M.null m) then return 0 
  else let (i, w) = M.findMax m in return (i+1) 
--  do
--    l1 <- newloc'
--    l2 <- newloc'
--    -- tutaj l1 == l2 (bo newloc' nie zapamiętuje w Store że l1 jest zajęte... :)
--    -- ale ponizej po każdym newloc' jest modify


--
-- The interpreter
--

findVar :: String -> RSE Loc
findVar name = do
    mt <- asks (M.lookup name)
    case mt of
        Just l -> return l
        Nothing -> throwError ("undefined variable: "++name)

findVal :: Loc -> RSE Val
findVal loc = do
    mv <- gets (M.lookup loc)
    case mv of
        Just v -> return v
        Nothing -> throwError "access to uninitialized location"

evalMaybe :: String -> Maybe a -> RSE a
evalMaybe s Nothing = throwError s
evalMaybe s (Just a) = return a



getIntVal :: Val -> RSE Integer
getIntVal v = case v of
    (IntVal i) -> return i
    _ -> throwError "Integer value expected" 
getBoolVal :: Val -> RSE Bool
getBoolVal v = case v of
    (BoolVal b) -> return b
    _ -> throwError "Boolean value expected"
getStrVal :: Val -> RSE String
getStrVal v = case v of
    (StrVal s) -> return s
    _ -> throwError "String value expected"

getArrEl :: Integer -> Integer -> RSE Integer
getArrEl idx size = do
    if (i >= size) then throwError ("array element out of bounds")

    else do
            -- if arr_size <= i
    --     then throwError ("array element out of bounds: "++name++"["++show l++"]")
    --     else do
    --     v <- evalMaybe ("array element out of bounds: "++name) (M.lookup (l+i) state)
    --     return (IntVal v)
        

evalExp :: Expr -> RSE Val

evalExp (Elval (EVar (Ident name))) = do
    env <- ask
    state <- get
    l <- evalMaybe ("undefined variable: "++ name) $ M.lookup name env
    v <- evalMaybe ("variable not initialized: "++name) $ M.lookup l state   -- returns value if found
    return v
evalExp (Elval (EArrEl (Ident name) expr)) = do
    loc <- findVar name
    v <- findVal loc
    size <- getIntVal v

    iv <- evalExp expr
    i <- getIntVal iv

    if (i >= size)
        then throwError 
    -- env <- ask
    -- state <- get
    -- -- index <- evalExp expr
    -- -- i <- getIntVal index
    -- l <- evalMaybe ("undefined array reference: "++ name) (M.lookup name env)
    -- --array points to the location of its size, next loc is 1st element
    -- arr_size <- evalMaybe ("unexpected error") $ M.lookup l state
    -- if arr_size <= i
    --     then throwError ("array element out of bounds: "++name++"["++show l++"]")
    --     else do
    --     v <- evalMaybe ("array element out of bounds: "++name) (M.lookup (l+i) state)
    --     return (IntVal v)

    -- return (IntVal 3) -- TODO usuń
evalExp (ELitInt n) = return $ IntVal n
evalExp ELitFalse = return $ BoolVal False
evalExp ELitTrue = return $ BoolVal True
-- evalExp EApp (Ident name) exp = do
--     env <- ask
--     state <- get

evalExp (EString str) = return $ StrVal str
evalExp (Neg e) = do
    vv <- evalExp e
    v <- getIntVal vv
    -- ~(IntVal val) <- evalExp e
    return $ IntVal (-v)

evalExp (Not e) = do
    vv <- evalExp e
    v <- getBoolVal vv
    return $ BoolVal $ not v

evalExp (EMul e1 op e2) = do
    vv1 <- evalExp e1
    v1 <- getIntVal vv1
    vv2 <- evalExp e2
    v2 <- getIntVal vv2
    return $ IntVal $ evalMulOp op v1 v2

evalExp (EAdd e1 op e2) = do
    vv1 <- evalExp e1
    vv2 <- evalExp e2
    v1 <- getIntVal vv1
    v2 <- getIntVal vv2
    return $ IntVal $ evalAddOp op v1 v2

evalExp (ERel e1 op e2) = do
    vv1 <- evalExp e1
    vv2 <- evalExp e2
    v1 <- getBoolVal vv1
    v2 <- getBoolVal vv2
    return $ BoolVal $ evalRelOp op v1 v2

evalExp (EAnd e1 e2) = do
    vv1 <- evalExp e1
    vv2 <- evalExp e2
    v1 <- getBoolVal vv1
    v2 <- getBoolVal vv2
    return $ BoolVal $ v1 && v2

evalExp (EOr e1 e2) = do
    vv1 <- evalExp e1
    vv2 <- evalExp e2
    v1 <- getBoolVal vv1
    v2 <- getBoolVal vv2
    return $ BoolVal $ v1 || v2

---
---Exec statement
---

interpret :: Stmt -> RSE ()  

interpret Empty = return ()

interpret (Ass (EVar (Ident name)) e) = do
    env <- ask
    l <- evalMaybe ("undefined variable: "++name) (M.lookup name env)
    val <- evalExp e 
    modify (M.insert l val)
interpret (Ass (EArrEl (Ident name) index_exp) val_exp) = do
    env <- ask
    store <- get
    l <- evalMaybe ("undefined array: "++name) (M.lookup name env)
    sizeval <- evalMaybe ("array not initialized: "++name) (M.lookup l store) -- size = "arr[0]"
    size <- getIntVal sizeval
    ival <- evalExp index_exp
    i <- getIntVal ival
    if (i >= size)
        then throwError ("index "++ show i ++" out of bounds for array '"++name ++"' of size " ++ show size)
        else do
            val <- evalExp val_exp
            modify (M.insert (l+i+1) val)

-- interpret (Incr (EArrEl (Ident name) e)) = do
--     l <- findVar name
--     sizeval <- findVal l
--     size <- getIntVal sizeval
--     arr_el_val <- findVal (l+i+1)
--     iv <- evalExp e   -- index
--     i <- getIntVal iv
--     if i >= size
--         then throwError ("index "++ show i ++" out of bounds for array "++name)
--         else do
--             -- modify (M.insert (l+i))
--             return ()

    

interpret (Seq s1 s2) = do {interpret s1; interpret s2}

interpret (Incr (EVar (Ident x))) = do
    env <- ask
    state <- get
    l <- evalMaybe ("undefined variable: "++x) (M.lookup x env)
    vv <- evalMaybe ("variable not initialized: "++x) $ M.lookup l state   -- returns value if found
    val <- getIntVal vv
    modify $ M.insert l $ IntVal $ val+1

interpret (Decr (EVar (Ident x))) = do
    env <- ask
    state <- get
    l <- evalMaybe ("undefined variable: "++x) (M.lookup x env)
    -- ~(IntVal val) <- evalMaybe "variable not initialized" $ M.lookup l state   -- returns value if found
    vv <- evalMaybe ("variable not initialized: "++x) $ M.lookup l state   -- returns value if found
    val <- getIntVal vv
    modify $ M.insert l $ IntVal $ val-1

interpret (Ret expr) = do
    v <- evalExp expr
    -- return v
    return () -- TODO return int zeby wyjsc z funkcji

interpret (Cond e b1) = do 
    vv <- evalExp e
    cond <- getBoolVal vv
    if cond == True then interpret (BStmt b1) else return ()

interpret (CondElse e b1 b2) = do 
--   ~(BoolVal cond) <- evalExp e
    vv <- evalExp e
    cond <- getBoolVal vv
    if cond == True then interpret (BStmt b1) else interpret (BStmt b2)
    
interpret (While e b) = do 
    vv <- evalExp e
    cond <- getBoolVal vv
    if cond == True then interpret (BStmt b) 
    else do {interpret (BStmt b); interpret (While e b)} 

interpret (BStmt (Block [] s)) =  interpret s
interpret (BStmt (Block ((Decl t item):ds) s)) =
    case item of
        Init (Ident x) expr -> do
            l <- newloc'
            val <- evalExp expr
            modify (M.insert l val)
            local (M.insert x l) (interpret (BStmt (Block ds s)))
        NoInit (Ident x) -> do
            l <- newloc'
            local (M.insert x l) (interpret (BStmt (Block ds s)))
        ArrInit (Ident x) expr -> do
            v <- evalExp expr       -- array size (IntVal)
            arr_size <- getIntVal v
            l <- newloc'
            modify (M.insert l v)    -- arr[0] = size (paradoksalnie:))
            newZerosArr arr_size
            local (M.insert x l) (interpret (BStmt (Block ds s)))
            where   -- initializes array of zeros of size n
                newZerosArr :: Integer -> RSE ()
                newZerosArr 0 = return ()
                newZerosArr n = do
                    l <- newloc'
                    modify (M.insert l (IntVal 0))
                    newZerosArr (n-1)

-- type RSE a = ReaderT Env (StateT Store (ExceptT String Identity)) a

-- Env -> Store -> Either String (a, Store)

interpretBlock :: Block -> RSE ()
interpretBlock b = interpret $ BStmt b

-- interpretProgram :: Program -> RSE ()



-- type FuncDef = (Stmt, [(String, Val)])    -- [Val] == args
-- FunVal = ... | FunVal FuncDef
interpretProgram (Program ((FnDef t (Ident fname) args block):fns) b) = do
    l <- newloc'
    local (M.insert fname l) (interpretProgram (Program fns b))
    where
        newFunc = FunVal (BStmt block, argnames)
        argnames = fmap getName args
        getName :: Arg -> String
        getName (Arg t (Ident name)) = name

execStmt :: Stmt -> IO ()
execStmt s =
  print $
    --   runExcept $ execStateT (runReaderT (interpretCatch s) M.empty) M.empty
    runExcept $ execStateT (runReaderT (interpret s) M.empty) M.empty

-- interpretCatch :: Stmt -> RSE ()
-- interpretCatch s = do
--   interpret s `catchError` (\e -> return e) --(\e -> modify (M.insert 17 (StrVal e)))

-- execProgram :: Program -> IO ()
-- execProgram p =
--   print $
--       runExcept $ execStateT (runReaderT  (interpretCatch p) M.empty) M.empty



--
-- Run the interpreter
--

-- main = print $ evalState (runReaderT (eval testE) M.empty) M.empty 

-- [] (Block [Decl Int (NoInit (Ident "x"))] (Seq (Ass (EVar (Ident "x")) (ELitInt 1)) (Seq (Incr (EVar (Ident "x"))) (Ret (Elval (EVar (Ident "x")))))))]

b1 = BStmt $
    Block 
    [Decl Int (NoInit (Ident "x"))] 
    (Seq 
        (Ass (EVar (Ident "x")) (ELitInt 1)) 
        (Seq 
            (Incr (EVar (Ident "y"))) 
            (Ret (Elval (EVar (Ident "x"))))
        )
    )


b = Block [Decl Int (NoInit (Ident "z")),Decl Int (ArrInit (Ident "arr") (ELitInt 10))] (Seq (Ass (EVar (Ident "z")) (ELitInt 2)) (Seq (Ass (EVar (Ident "z")) (Elval (EArrEl (Ident "arr") (ELitInt 1)))) (Ass (EArrEl (Ident "arr") (ELitInt 1)) (ELitInt 5))))

-- let p1 = Program [FnDef Int (Ident "main") [] (Block [Decl Int (NoInit (Ident "x"))] (Seq (Ass (EVar (Ident "x")) (ELitInt 1)) (Seq (Incr (EVar (Ident "x"))) (Ret (Elval (EVar (Ident "x")))))))]
b' = Block [Decl Int (NoInit (Ident "z")), Decl Int (ArrInit (Ident "arr") (ELitInt 10))] (Seq (Ass (EVar (Ident "z")) (ELitInt 2)) (Ass (EVar (Ident "z")) (Elval (EArrEl (Ident "arr") (ELitInt 1)))))
-- let p = Program [FnDef Int (Ident "f") [] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 1)))] (Block [Decl Int (NoInit (Ident "z")),Decl Int (ArrInit (Ident "arr") (ELitInt 10))] (Seq (Ass (EVar (Ident "z")) (ELitInt 2)) (Ass (EVar (Ident "z")) (Elval (EArrEl (Ident "arr") (ELitInt 1))))))