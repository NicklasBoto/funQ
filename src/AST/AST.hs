{-# LANGUAGE LambdaCase #-}

module AST.AST where

import Parser.Par
import qualified Parser.Abs as P
import qualified Data.Map as M

type Bit = P.Bit
type Gate = P.Gate
type Env = M.Map String Integer

name :: P.Variable -> String
name (P.Variable v) = v

tupmap :: (P.Term -> b) -> P.Tup -> (b, b)
tupmap f (P.Tuple a b) = (f a, f b)

data Term
    = Idx  Integer       -- bound
    | QVar String        -- free
    | Bit  Bit
    | Gate Gate
    | Tup  (Term, Term)
    | App  Term Term
    | IfEl Term Term Term
    | Let  (Term, Term) Term Term
    | Abs  Term
    | New
    | Meas
    | Void

instance Show Term where
    show (Idx i) = show i
    show (QVar s) = s
    show (Bit b) = show b
    show (Gate g) = show g
    show (Tup t) = show t
    show (App l r) = show l ++ " " ++ show r
    show (IfEl c t f) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show f
    show (Let t e i) = "let " ++ show t ++ " = " ++ show e ++ " in " ++ show i
    show (Abs t) = "λ " ++ show t
    show New = "new"
    show Meas = "measure"
    show Void = "*"

type Type = P.Type

data Function = Func String Type Term 

instance Show Function where
    show (Func n t e) = "\n" ++ n ++ " : " ++ show t ++ "\n"
                             ++ n ++ " = " ++ show e ++ "\n"

tobruijn :: Env -> P.Term -> Term 
tobruijn env (P.TLamb _ var term) = Abs $ tobruijn env' term
    where env' = M.insert (name var) 0 (M.map succ env)
tobruijn env (P.TApp l r) = App (tobruijn env l) (tobruijn env r)
tobruijn env (P.TVar (P.Variable "new")) = New 
tobruijn env (P.TVar (P.Variable "meas")) = Meas
tobruijn env (P.TVar (P.Variable "measure")) = Meas
tobruijn env (P.TVar var) = case M.lookup (name var) env of
    Just idx -> Idx idx
    Nothing  -> QVar (name var)
tobruijn env (P.TIfEl cond true false) = 
    IfEl (tobruijn env cond) (tobruijn env true) (tobruijn env false)
tobruijn env (P.TLet tup eq inn) = 
    Let (tupmap (tobruijn env) tup) (tobruijn env eq) (tobruijn env inn)
tobruijn env (P.TTup t) = Tup $ tupmap (tobruijn env) t
tobruijn _env (P.TBit b) = Bit b
tobruijn _env (P.TGate g) = Gate g
tobruijn _env P.TStar = Void

bruijnize :: P.Term -> Term
bruijnize = tobruijn M.empty

lambdaizeFunction :: P.FunDec -> Function 
lambdaizeFunction (P.FDecl n t fun) = Func (unfun n) t (bruijnize $ lambdaize fun)
    where unfun (P.FunVar x) = init $ filter (/=' ') x

lambdaize :: P.Function -> P.Term
lambdaize (P.FDef _ [] term) = term
lambdaize (P.FDef _n (a:args) term) = P.TLamb lambda (unArg a) (lambdaize (P.FDef _n args term))
    where unArg (P.FArg v) = v
          lambda = P.Lambda "\\"

toIm :: P.Program -> [Function]
toIm (P.PDef fs) = map lambdaizeFunction fs

run :: String -> [Function]
run s = case pProgram (myLexer s) of
    Left s -> error s
    Right p -> toIm p

runFile :: FilePath -> IO [Function]
runFile path = run <$> readFile path

-- cause this might vanish from Parser.Abs
-- instance C.Show Type where
  -- show (TypeVar (Variable s)) = s
  -- show TypeBit = "Bit"
  -- show TypeQbit = "QBit"
  -- show TypeVoid = "⊤"
  -- show (TypeDup t) = "!" C.++ C.show t
  -- show (TypeTens l r) = C.show l C.++ " ⊗ " C.++ C.show r
  -- show (TypeFunc l r) = C.show l C.++ " ⊸ " C.++ C.show r
