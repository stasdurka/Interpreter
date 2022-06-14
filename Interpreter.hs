{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use when" #-}

module Interpreter where
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

-- import Data.Maybe(fromMaybe)
-- import Data.Either(fromRight)
-- import Control.Monad (foldM)
import AbsTinyPlus
import Distribution.TestSuite (TestInstance(name))

type Loc = Integer

type Env = M.Map String Loc

type Store = M.Map Loc Val

type RSEIO a = ReaderT Env (StateT Store (ExceptT String IO)) a

type FuncDef = (Block, [String])    -- [Val] == arg names

data Val = IntVal Integer | BoolVal Bool | StrVal String | FunVal  (Block, [String]) --FuncDef
    deriving Show

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
newloc m = if M.null m then 0 
          else let (i, w) = M.findMax m in i+1  

newloc' :: RSEIO Loc
newloc' = do 
  m <- get 
  if M.null m then return 0 
  else let (i, w) = M.findMax m in return (i+1) 


findVar :: String -> RSEIO Loc
findVar name = do
    mt <- asks (M.lookup name)
    case mt of
        Just l -> return l
        Nothing -> throwError ("undefined variable: "++name)

-- State (store)
findVal :: Loc -> RSEIO Val
findVal loc = do
    mv <- gets (M.lookup loc)
    case mv of
        Just v -> return v
        Nothing -> throwError "access to uninitialized location"

evalMaybe :: String -> Maybe a -> RSEIO a
evalMaybe s Nothing = throwError s
evalMaybe s (Just a) = return a

getIntVal :: Val -> RSEIO Integer
getIntVal v = case v of
    (IntVal i) -> return i
    _ -> throwError "Integer value expected" 
getBoolVal :: Val -> RSEIO Bool
getBoolVal v = case v of
    (BoolVal b) -> return b
    _ -> throwError "Boolean value expected"
getStrVal :: Val -> RSEIO String
getStrVal v = case v of
    (StrVal s) -> return s
    _ -> throwError "String value expected"
getFnVal :: Val -> RSEIO (Block, [String]) -- FuncDef
getFnVal v = case v of
    FunVal f -> return f
    _ -> throwError "Function definition expected"
        

evalExp :: Expr -> RSEIO Val

evalExp (Elval (EVar (Ident name))) = do
    env <- ask
    state <- get
    l <- evalMaybe ("undefined variable: "++ name) $ M.lookup name env
    v <- evalMaybe ("variable not initialized: "++name) $ M.lookup l state   -- returns value if found
    return v
evalExp (Elval (EArrEl (Ident name) expr)) = do
    loc <- findVar name
    size' <- findVal loc            -- arr[0] == size of the array
    size <- getIntVal size'

    iv <- evalExp expr
    i <- getIntVal iv

    if i >= size
        then throwError ("index out of bounds for array " ++ name)
    else do
        el <- findVal (loc + i + 1)
        return el
     
evalExp (ELitInt n) = return $ IntVal n
evalExp ELitFalse = return $ BoolVal False
evalExp ELitTrue = return $ BoolVal True
evalExp (EApp (Ident name) argvalues) = do
    loc <- findVar name     -- find the location where functions definition is stored
    f_def_v <- findVal loc
    -- type FuncDef = (Block, [String])    -- [Val] == arg names
    (block, argnames) <- getFnVal f_def_v
    v <- evalF block argnames argvalues
    return v
    where
        evalF :: Block -> [String] -> [Expr] -> RSEIO Val
        evalF b (a:args) (e:exps) = do
            l <- newloc'
            val <- evalExp e
            modify (M.insert l val)
            local (M.insert a l) (evalF b args exps)
        evalF b [] [] = do
            v <- interpret (BStmt b)
            case v of
                Nothing -> throwError "function definition must include a return statement"
                Just n -> return $ IntVal n
        evalF _ _ _ = return $ IntVal (-1) -- przypadek jeśli len(args) /= len(exps), nie powinien się zdarzyć


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
    v1 <- getIntVal vv1
    v2 <- getIntVal vv2
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

-- evalExp (EApp (Ident fname) exprs) = do
--     return $ IntVal 0
---
---Exec statement
---

interpret :: Stmt -> RSEIO (Maybe Integer)

interpret Empty = return Nothing

interpret (Ass (EVar (Ident name)) e) = do
    env <- ask
    l <- evalMaybe ("undefined variable: "++name) (M.lookup name env)
    val <- evalExp e 
    modify (M.insert l val)
    return Nothing
interpret (Ass (EArrEl (Ident name) index_exp) val_exp) = do
    env <- ask
    store <- get
    l <- evalMaybe ("undefined array: "++name) (M.lookup name env)
    sizeval <- evalMaybe ("array not initialized: "++name) (M.lookup l store) -- size = "arr[0]"
    size <- getIntVal sizeval
    ival <- evalExp index_exp
    i <- getIntVal ival
    if i >= size
        then throwError ("index "++ show i ++" out of bounds for array '"++name ++"' of size " ++ show size)
        else do
            val <- evalExp val_exp
            modify (M.insert (l+i+1) val)
    return Nothing

-- interpret (Incr el) = interpret (Ass (el ))
 
-- interpret (Incr (EArrEl id e)) = 
--     return Nothing   -- TODO
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

-- interpret (Decr (EArrEl (Ident name) e)) = do
--     return Nothing   -- TODO

interpret (Seq s1 s2) = do 
    ret1 <- interpret s1
    case ret1 of
        Nothing -> interpret s2
        Just n -> return $ Just n

-- interpret (Incr (EVar (Ident x))) = do
--     env <- ask
--     state <- get
--     l <- evalMaybe ("undefined variable: "++x) (M.lookup x env)
--     vv <- evalMaybe ("variable not initialized: "++x) $ M.lookup l state   -- returns value if found
--     val <- getIntVal vv
--     modify $ M.insert l $ IntVal $ val+1
--     return Nothing

-- interpret (Decr (EVar (Ident x))) = do
--     env <- ask
--     state <- get
--     l <- evalMaybe ("undefined variable: "++x) (M.lookup x env)
--     -- ~(IntVal val) <- evalMaybe "variable not initialized" $ M.lookup l state   -- returns value if found
--     vv <- evalMaybe ("variable not initialized: "++x) $ M.lookup l state   -- returns value if found
--     val <- getIntVal vv
--     modify $ M.insert l $ IntVal $ val-1
--     return Nothing

interpret (Ret expr) = do
    v <- evalExp expr
    intv <- getIntVal v
    -- return v
    return $ Just intv -- TODO return int zeby wyjsc z funkcji

interpret (Cond e b1) = do 
    vv <- evalExp e
    cond <- getBoolVal vv
    if cond == True then interpret (BStmt b1) else return Nothing

interpret (CondElse e b1 b2) = do
--   ~(BoolVal cond) <- evalExp e
    vv <- evalExp e
    cond <- getBoolVal vv
    if cond == True then interpret (BStmt b1) else interpret (BStmt b2)
    
interpret (While e b) = do 
    vv <- evalExp e
    cond <- getBoolVal vv
    if cond == False then return Nothing
    else do {interpret (BStmt b); interpret (While e b)} 

interpret (For (Ident i) exp block) = do
    range' <- evalExp exp
    range <- getIntVal range'
    if range > 0 then
        throwError "range can't be negative in a 'for' loop"
    else do
        let i = 0
        interpFor i range (BStmt block)
        where
            interpFor :: Integer -> Integer -> Stmt -> RSEIO (Maybe Integer)
            interpFor i range bstmt = do
                if i == range then return Nothing
                else do
                    ret <- interpret bstmt
                    case ret of
                        Nothing -> interpFor (i+1) range bstmt
                        Just n -> return $ Just n
interpret (Func e) = do
    evalExp e
    return Nothing

interpret (BStmt (NoDecl s)) = interpret s
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
                newZerosArr :: Integer -> RSEIO ()
                newZerosArr 0 = return ()
                newZerosArr n = do
                    l <- newloc'
                    modify (M.insert l (IntVal 0))
                    newZerosArr (n-1)
interpret (Print e) = do
    v <- evalExp e
    liftIO $ putStrLn $ showVal v
    return Nothing
    where
        showVal :: Val -> String
        showVal v = case v of
            IntVal n -> show n
            BoolVal b -> show b
            StrVal str -> show str
            FunVal f -> show f

interpretBlock :: Block -> RSEIO (Maybe Integer)
interpretBlock b = interpret $ BStmt b

interpretProgram :: Program -> RSEIO Integer
interpretProgram (Program ((FnDef t (Ident fname) args block):fns) b) = do
    l <- newloc'
    modify (M.insert l newFunc)
    local (M.insert fname l) (interpretProgram (Program fns b))
    where
        newFunc = FunVal (block, argnames)
        argnames = fmap getName args
        getName :: Arg -> String
        getName (Arg t (Ident name)) = name
        getName (ArrRef t (Ident name)) = name
        getName (VarRef t (Ident name)) = name

interpretProgram (Program [] b_main) = do
    interpret (BStmt b_main)
    ret <- interpret (BStmt b_main)
    case ret of 
        Just n -> return n
        Nothing -> return 0


execStmt :: Stmt -> IO (Either String Store)
execStmt s = 
    --   runExcept $ execStateT (runReaderT (interpretCatch s) M.empty) M.empty
    runExceptT $ execStateT (runReaderT (interpret s) M.empty) M.empty

execProgram :: Program -> IO (Either String Store)
execProgram p =
    runExceptT $ execStateT (runReaderT (interpretProgram p) M.empty) M.empty

exec :: Program -> IO String
exec p = do
    ret <- execProgram p
    case ret of
        Left err -> return $ "runtime error: " ++ err
        Right s -> return $ "main executed successfully"

-- b1 = BStmt $
--     Block 
--     [Decl Int (NoInit (Ident "x"))] 
--     (Seq 
--         (Ass (EVar (Ident "x")) (ELitInt 1)) 
--         (Seq 
--             (Incr (EVar (Ident "y"))) 
--             (Ret (Elval (EVar (Ident "x"))))
--         )
--     )


b = Block [Decl Int (NoInit (Ident "z")),Decl Int (ArrInit (Ident "arr") (ELitInt 10))] (Seq (Ass (EVar (Ident "z")) (ELitInt 2)) (Seq (Ass (EVar (Ident "z")) (Elval (EArrEl (Ident "arr") (ELitInt 1)))) (Ass (EArrEl (Ident "arr") (ELitInt 1)) (ELitInt 5))))

-- let p1 = Program [FnDef Int (Ident "main") [] (Block [Decl Int (NoInit (Ident "x"))] (Seq (Ass (EVar (Ident "x")) (ELitInt 1)) (Seq (Incr (EVar (Ident "x"))) (Ret (Elval (EVar (Ident "x")))))))]
b' = Block [Decl Int (NoInit (Ident "z")), Decl Int (ArrInit (Ident "arr") (ELitInt 10))] (Seq (Ass (EVar (Ident "z")) (ELitInt 2)) (Ass (EVar (Ident "z")) (Elval (EArrEl (Ident "arr") (ELitInt 1)))))
-- let p = Program [FnDef Int (Ident "f") [] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 1)))] (Block [Decl Int (NoInit (Ident "z")),Decl Int (ArrInit (Ident "arr") (ELitInt 10))] (Seq (Ass (EVar (Ident "z")) (ELitInt 2)) (Ass (EVar (Ident "z")) (Elval (EArrEl (Ident "arr") (ELitInt 1))))))

test_arr = Program [FnDef Int (Ident "f") [Arg Int (Ident "x")] (Block [Decl Int (Init (Ident "k") (ELitInt 0))] (Seq (Ass (EVar (Ident "x")) (EAdd (Elval (EVar (Ident "x"))) Plus (ELitInt 1))) (Ret (Elval (EVar (Ident "x"))))))] (Block [Decl Int (Init (Ident "z") (ELitInt 2)),Decl Int (ArrInit (Ident "arr") (ELitInt 5))] (Seq (Ass (EArrEl (Ident "arr") (ELitInt 1)) (ELitInt 100)) (Print (EAdd (Elval (EArrEl (Ident "arr") (ELitInt 1))) Plus (ELitInt 1)))))
test0 = Program [FnDef Int (Ident "f") [Arg Int (Ident "x")] (Block [Decl Int (Init (Ident "k") (ELitInt 0))] (Seq (Ass (EVar (Ident "x")) (EAdd (Elval (EVar (Ident "x"))) Plus (ELitInt 1))) (Ret (Elval (EVar (Ident "x"))))))] (Block [Decl Int (Init (Ident "z") (ELitInt 2)),Decl Int (ArrInit (Ident "arr") (ELitInt 5))] (Ass (EArrEl (Ident "arr") (ELitInt 0)) (ELitInt 100)))
bad = Program [FnDef Int (Ident "f") [] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 1)))] (Block [Decl Int (NoInit (Ident "z"))] (Ass (EVar (Ident "x")) (ELitInt 2)))