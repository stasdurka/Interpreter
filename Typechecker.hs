{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Redundant return" #-}
module Typechecker where
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import AbsTinyPlus
import Prelude

type Env = Map.Map Name TType
type Name = String

data TType = T Type | TFunc [TType] | TArr  -- int arr[size:int]
            deriving (Eq, Ord, Show, Read)

type RE a = ReaderT Env (ExceptT String Identity) a
runTypeOf :: Env -> RE a -> Either String a
runTypeOf env exp = runIdentity (runExceptT (runReaderT exp env)) 

typecheck :: Program -> Either String ()
typecheck p = runTypeOf Map.empty (checkProgram p)


findVar :: Name -> RE TType
findVar name = do
    mt <- asks (Map.lookup name)
    case mt of
        Just t -> return t
        Nothing -> throwError ("'"++name++"' not initialized")

checkType :: Expr -> TType -> RE TType
checkType e t = do
    t' <- typeOf e
    if t == t' then return t
    else throwError ("type " ++ show t ++ " expected instead of " ++ show t')

compareTypes :: TType -> TType -> RE TType
compareTypes t t' = do
    if t == t' then return t
    else throwError ("type " ++ show t ++ " expected instead of " ++ show t')

typeOf :: Expr -> RE TType
typeOf (ELitInt _) = return $ T Int
-- typeOf1 (ELam n t0 e1) = do
--   t1 <- local (Map.insert n t0) (typeOf1 e1)
--   return $ t0 :-> t1
typeOf (Elval (EArrEl (Ident name) expr)) = do
    findVar name
    t <- checkType expr (T Int)
    return t

typeOf (Elval (EVar (Ident name))) = findVar name

typeOf ELitTrue = return (T Bool)

typeOf ELitFalse = return (T Bool)

typeOf (EApp (Ident fname) args) = do         -- function application
    t <- asks (Map.lookup fname)              -- types that each arg should have
    case t of
        Nothing -> throwError ("function "++fname++" not declared")
        -- Nothing -> return (T Int) -- TODO debug!!!!!!!!!!!
        Just (TFunc argTypes) -> checkArgs fname argTypes args
        _ -> throwError (fname++" is not a function") 
        where 
        checkArgs :: Name -> [TType] -> [Expr] -> RE TType      
        checkArgs fname types exps = do
            case (types, exps) of
                (t:ts,e:es) -> do
                    etype <- typeOf e
                    if etype == t 
                        then checkArgs fname ts es
                        else throwError ("function "++ fname ++": invalid argument type")
                ([],[]) -> return (T Int)
                ([], e:es) -> throwError ("function "++ fname ++": too many arguments given")
                (t:ts, []) -> throwError ("function "++ fname ++": too few arguments given")


typeOf (EString _) = return (T Str)

typeOf (Neg exp) = checkType exp (T Int)

typeOf (Not exp) = checkType exp (T Bool)

typeOf (EMul exp1 op exp2) = do
    t1 <- checkType exp1 (T Int)
    t2 <- checkType exp2 (T Int)
    return $ T Int

typeOf (EAdd exp1 op exp2) = do
    t1 <- checkType exp1 (T Int)
    t2 <- checkType exp2 (T Int)
    return (T Int)

typeOf (ERel exp1 op exp2) = do
    t1 <- checkType exp1 (T Int)
    t2 <- checkType exp2 (T Int)
    return $ T Bool

typeOf (EAnd exp1 exp2) = do
    t1 <- checkType exp1 (T Bool)
    t2 <- checkType exp2 (T Bool)
    return $ T Bool

typeOf (EOr exp1 exp2) = do
    t1 <- checkType exp1 (T Bool)
    t2 <- checkType exp2 (T Bool)
    return (T Bool)

argToType :: Arg -> TType
argToType (Arg t id) = T t
argToType (ArrRef t id) = TArr
argToType (VarRef t id) = T t

createFuncT :: [Arg] -> TType
createFuncT args = 
    let toType :: Arg -> TType
        toType (Arg t id) = T t
        toType (ArrRef t id) = TArr
        toType (VarRef t id) = T t
    in TFunc (fmap toType args)

checkFunction :: TopDef -> RE ()

checkFunction def@(FnDef t (Ident fname) args b) =
    local (Map.insert fname fType) (checkFunction' def)  -- umożliwia rekurencję
    where
        fType = createFuncT args
        checkFunction' :: TopDef -> RE ()
        checkFunction' (FnDef t name@(Ident f) [] b) = do
            compareTypes (T t) (T Int)      -- throws error if not int
            checkBlock b
        checkFunction' (FnDef t name args@(arg:as) b) = do
            case arg of
                Arg t' (Ident argname) -> do
                    -- compareTypes (T t) (T Int)      -- throws error
                    local (Map.insert argname (T t')) (checkFunction' (FnDef t name as b))
                ArrRef t' (Ident argname) -> do
                    local (Map.insert argname TArr) (checkFunction' (FnDef t name as b))
                VarRef t' (Ident argname) -> do
                    local (Map.insert argname (T t')) (checkFunction' (FnDef t name as b))


checkProgram :: Program -> RE ()
checkProgram (JustMain main) = checkBlock main
checkProgram (Program [] main) = checkBlock main
checkProgram (Program (f@(FnDef t (Ident name) args b):fs) mainBlock) = do
    checkFunction f
    let fType = createFuncT args
    local (Map.insert name fType) (checkProgram (Program fs mainBlock))

checkBlock :: Block -> RE ()
checkBlock  (NoDecl stmt) = checkStmt stmt
checkBlock (Block [] stmt) = checkStmt stmt
checkBlock (Block ((Decl t item):ds) stmt) = do
    case item of
        NoInit (Ident name) -> local (Map.insert name t') (checkBlock (Block ds stmt))
        Init (Ident name) expr -> do
            t' <- checkType expr t'          -- throws error if wrong type
            local (Map.insert name t') (checkBlock (Block ds stmt))
        ArrInit (Ident name) expr -> do
            t'' <- checkType expr (T Int)       -- throws error
            local (Map.insert name TArr) (checkBlock (Block ds stmt))

        where t' = T t

checkStmt :: Stmt -> RE ()
checkStmt Empty = return ()
checkStmt (Func expr) = 
    case expr of
        (EApp id exp) -> return ()
        _ -> throwError "standalone expression must be a function application"
checkStmt (BStmt b) = checkBlock b
checkStmt (Seq s1 s2) = do
    checkStmt s1
    checkStmt s2
checkStmt (Ass lval expr) =
    case lval of 
        EVar (Ident name) -> do
            t <- findVar name       -- throws error if var not initialized
            -- t <- asks (Map.lookup name)
            if t == TArr then throwError "arrays cannot be copied explicitly"
            else do
                t' <- checkType expr t-- throws error if t /= t'
                return ()
        EArrEl (Ident name) expr -> do
            t1 <- findVar name
            t2 <- checkType expr (T Int)
            return ()
-- checkStmt (Incr (EVar (Ident name))) = do
--     t <- findVar name
--     compareTypes t (T Int)
--     return ()
-- checkStmt (Decr (EVar (Ident name))) = do
--     t <- findVar name
--     compareTypes t (T Int)
--     return ()
checkStmt (Ret expr) = do
    checkType expr (T Int)
    return ()
checkStmt (Cond expr b) = do
    checkType expr (T Bool)
    checkBlock b
checkStmt (CondElse expr b1 b2) = do
    checkType expr (T Bool)
    checkBlock b1
    checkBlock b2
checkStmt (While expr b) = do
    checkType expr (T Bool)
    checkBlock b
-- checkStmt (For ident expr b) = do           -- for i in range ...
--     checkType expr (T Int)
--     checkBlock b
checkStmt (Print (e:es)) = do
    t <- typeOf e
    checkStmt (Print es)
checkStmt (Print []) = return ()
-- checkStmt (PrintLn exps) = checkStmt (Print exps)
    


-- always returns TInt if the arguments match their expected types,
-- and throws an error otherwise


  -- return $ fromJust t
-- typeOf (EApp e1 e2) = do
--   tlambda <- typeOf e1
--   tx <- typeOf e2
--   case tlambda of 
--     tx :-> tret -> return tret
--     t -> throwError ("Cannot apply "++show e1++"::"++show t++" to "++show e2)

-- runTypeOf Map.empty (typeOf (EVar "a"))

-- prog = Program [] bmain
-- bmain = Block 
--     [Decl Int (NoInit (Ident "x"))] 
--     (Seq 
--         (Ass (EVar (Ident "x")) (ELitInt 1)) 
--         (Seq 
--             (Incr (EVar (Ident "y"))) 
--             (Ret (Elval (EVar (Ident "x"))))
--         )
--     )

-- ex0 = runTypeOf Map.empty (typeOf (EOr (ELitInt 1) (ELitInt 2))) --wrong
-- ex1 = runTypeOf Map.empty (typeOf (EAdd (ELitInt 1) Plus (ELitInt 2))) --good

-- ex2 = runTypeOf Map.empty (checkProgram prog)   -- y not initialized

-- prog100 = Program [FnDef Int (Ident "f") [] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 1)))] (Block [] (Seq (Ass (EVar (Ident "z")) (ELitInt 5)) (Ret (ELitInt 1))))
-- ex100 = runTypeOf Map.empty (checkProgram prog100)

-- prog101 = Program [FnDef Int (Ident "f") [] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 1)))] (Block [Decl Int (NoInit (Ident "c"))] (Seq (Ass (EVar (Ident "z")) (ELitInt 5)) (Ret (ELitInt 1))))
-- ex101 = runTypeOf Map.empty (checkProgram prog101)

-- prog000 = Program [FnDef Int (Ident "f") [] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 1)))] (Block [Decl Int (NoInit (Ident "x"))] (Ass (EVar (Ident "x")) (ELitInt 2)))
-- exxx = runTypeOf Map.empty (checkProgram prog000)

-- test0 = Program [FnDef Int (Ident "f") [Arg Int (Ident "x")] (Block [Decl Int (Init (Ident "k") (ELitInt 0))] (Seq (Ass (EVar (Ident "x")) (EAdd (Elval (EVar (Ident "x"))) Plus (ELitInt 1))) (Ret (Elval (EVar (Ident "x"))))))] (Block [Decl Int (Init (Ident "z") (ELitInt 2)),Decl Int (ArrInit (Ident "arr") (ELitInt 5))] (Ass (EArrEl (Ident "arr") (ELitInt 0)) (ELitInt 100)))
