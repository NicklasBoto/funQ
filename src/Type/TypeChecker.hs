{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Type.TypeChecker where

import AST.AST 
import qualified Data.Map as M
-- import qualified Parser.Abs as P (Gate(..))
import Control.Monad.Except
import Control.Monad.State
import Data.Either
import Prelude hiding (GT)

(>=>), (><) :: Type -> Type -> Type
(>=>) = (:=>)
(><)  = (:><)

data Named
    = Bound Integer
    | Free String
    deriving (Ord, Eq, Show)

type Context = M.Map Named Type

data Err 
    = NoSuchGateError
    | TypeMismatchError Type Type
    | TypeNonFactorizableError Type
    | NotInScopeError Named
    | Fail String
    deriving Show

newtype Check a = MkCheck (StateT Context (Except Err) a)
    deriving (Functor, Applicative, Monad, MonadError Err, MonadState Context)

instance MonadFail Check where
    fail = throwError . Fail

runResult :: Check a -> Either Err a
runResult (MkCheck s) = runExcept $ evalStateT s M.empty

getResult :: Check a -> a
getResult r = case runResult r of
    Right r -> r
    Left  e -> error $ show e

tcFile :: FilePath -> IO [Err]
tcFile p = typecheckProgram <$> runFile p

tc :: String -> [Err]
tc = typecheckProgram . run

typecheckProgram :: [Function] -> [Err]
typecheckProgram prog = lefts $ map (run' . uncurry (typecheck 0) . f) prog
    where f (Func _ typ term) = (term, typ)
          context = buildProgramContext prog
          run' (MkCheck s) = runExcept $ evalStateT s context

buildProgramContext :: Program -> Context
buildProgramContext = M.fromList . map f
       where f (Func name typ _) = (Free name, typ)

-- f : (Bit -o (QBit -o Bit))
-- f = \x.\y.x

-- ca f -> [x : Bit, y : QBit ], x

-- typecheck :: Program -> TC Program
-- typ ner

-- f : QBit -> Bit
-- f = new

baseType :: MonadError Err m => Type -> Type -> m ()
baseType exp typ = if typ == exp 
    then return () else throwError $ TypeMismatchError exp typ

typecheck :: Integer -> Term -> Type -> Check ()
typecheck i (Abs body) (n :=> p) = do
    modify $ M.insert (Bound i) n
    typecheck (i+1) body p
typecheck i (Idx j) t = do
    c <- get
    case M.lookup (Bound j) c of
        Nothing    -> throwError $ NotInScopeError (Bound j)
        Just type' -> baseType t type'
typecheck i (Bit _) t = baseType t TypeBit
typecheck i (Gate g) t = baseType t =<< inferGate g
typecheck i New t = baseType t (TypeBit :=> TypeQBit)
typecheck i Meas t = baseType t (TypeQBit :=> TypeBit)
typecheck i Void t = baseType t TypeVoid
typecheck i (QVar v) t = do
    c <- get
    case M.lookup (Free v) c of
        Nothing -> throwError $ NotInScopeError (Free v)  
        Just type' -> baseType t type'
typecheck i (Tup [e]) t = typecheck i e t
typecheck i (Tup (e:es)) (t :>< t') = typecheck i e t >> typecheck i (Tup es) t'
typecheck i (Let eq inn) t = do
    l :>< r <- infer i eq
    modify $ M.insert (Bound (i+1)) r . M.insert (Bound i) l
    typecheck (i+2) inn t
typecheck i (App e1 e2) t = infer i e1 >>= \case
        (TypeVar v :=> b) -> do
            --typecheck i e2 (TypeVar v)
            --if t == b then return () else throwError $ TypeMismatchError t b
            

            te2 <- infer i e2 

            modify (M.insert (Bound i) te2)
            if t == b 
                then return ()
                else throwError $ TypeMismatchError t b
        (a :=> b) -> do
                te2 <- infer i e2
                if a == te2 && t == b 
                    then return () 
                    else throwError $ TypeMismatchError a te2
        t'        -> throwError $ TypeMismatchError (t' :=> t) t'
typecheck i e t = error $ show e

-- tc :: [Type] ->

-- typecheck i _c (App (Abs term) r) type' = undefined
-- typ upp
-- \0. \1.(here i is 2, c=[0:Typ0,1:Typ1]) 1 

infer :: Integer -> Term -> Check Type
infer i (QVar v) = do 
    c <- get 
    case M.lookup (Free v) c of
        Just type' -> return type'
        Nothing    -> throwError $ NotInScopeError (Free v)
infer i (Idx x)  = do
    c <- get 
    let v = Bound (i - x - 1)
    case M.lookup v c of
        Just type' -> return type'
        Nothing -> throwError $ NotInScopeError v
infer i (Bit _)  = return TypeBit 
infer i Void     = return TypeVoid
infer i (Gate g) = inferGate g
infer i Meas     = return $ TypeQBit :=> TypeBit
infer i New      = return $ TypeBit :=> TypeQBit
infer i (Tup ts) = foldr1 (><) <$> mapM (infer i) ts
infer i (App l r) = do
    il <- infer i l 
    ir <- infer i r
    inferApp il ir
infer i (IfEl b true false) = do
    typecheck i b TypeBit
    trueType  <- infer i true
    falseType <- infer i false 
    inferIf trueType falseType
infer i (Let eq inn) = do
    ~(l :>< r) <- checkTup =<< infer i eq
    modify $  M.insert (Bound (i+1)) r . M.insert (Bound i) l
    infer (i+2) inn
infer i (Abs e) = do
    modify $ M.insert (Bound i) (TypeVar ("a"++ show i)) 
    et <- infer (i+1) e
    return $ TypeVar ("a" ++ show i) :=> et

--     rightType <- infer i e
--     -- Need to insert, not lookup.
--     c <- get 
--     let varType = M.lookup (Bound i) c
--     return $ varType :=> rightType
--     where
--         varType = M.lookup (Bound i) c
-- -- 



-- f : Bit -> QBit -> Bit
-- f = \b . \q 


-- f = \x . \y . 


-- f x =
-- (\x.aoeu)y
-- \x.aoeu -> y

-- f : Bit -> Bit
-- f x = \y.x

-- infer (\y.x) [x : Bit] ==> a -> Bit

---- *
-- (\x.(\y.x)0) : Bit -> Bit []

-- (\y.x) 0 : Bit [x : Bit]
---- (\y.x) : a -> Bit [x : Bit]
---- 0 : a [x : Bit, y : a]
---- 0 : Bit [x : Bit, y : Bit]

---- *
-- (\x.(\y.\z.x) 0 1) : Bit -> Bit []
-- (((\y.\z.x) 0) 1) : Bit [x : Bit]
---- 0 [x : Bit, y : Bit, z : a2]
---- 1 [x : Bit, y : Bit, z : Bit]

--let (var, var)
checkTup :: Type -> Check Type
checkTup (l :>< r) = return $ l >< r
checkTup t         = throwError $ TypeNonFactorizableError t


inferApp :: Type -> Type -> Check Type 
inferApp (n :=> p) t
    | n == t    = return p
    | otherwise = throwError $ TypeMismatchError n t

inferIf :: Type -> Type -> Check Type
inferIf t f
    | t == f    = return t
    | otherwise = throwError $ TypeMismatchError t f


-- Func "f" TypeQBit (App New (Bit 0))

inferGate :: Gate -> Check Type
inferGate GCNOT = return ((TypeQBit :>< TypeQBit) :=> (TypeQBit :>< TypeQBit))
inferGate GSWP  = return ((TypeQBit :>< TypeQBit) :=> (TypeQBit :>< TypeQBit))
inferGate GFRDK = return ((TypeQBit :>< TypeQBit :>< TypeQBit) :=> (TypeQBit :>< TypeQBit :>< TypeQBit))
inferGate GTOF  = return ((TypeQBit :>< TypeQBit :>< TypeQBit) :=> (TypeQBit :>< TypeQBit :>< TypeQBit))
inferGate GH    = return (TypeQBit :=> TypeQBit) 
inferGate GX    = return (TypeQBit :=> TypeQBit) 
inferGate GY    = return (TypeQBit :=> TypeQBit) 
inferGate GZ    = return (TypeQBit :=> TypeQBit) 
inferGate GI    = return (TypeQBit :=> TypeQBit) 
inferGate GS    = return (TypeQBit :=> TypeQBit) 
inferGate GT    = return (TypeQBit :=> TypeQBit) 
inferGate _g    = throwError NoSuchGateError
 

-- id : Bit -o Bit
-- id = \x . (\y . x) 0

-- tc id []
-- tc (App (\y . x) 0) [x : Bit]

-- (\y . x) [x : Bit]
-- x [x : Bit, y : a]

-- [x : Bit]
-- \y . x : a -> Bit

-- App (\y . x : a -> Bit) (0 : Bit) : Bit


shouldTypecheck1 = "id : Bit -o Bit id x = (\\y.x) 0"

ast = run shouldTypecheck1