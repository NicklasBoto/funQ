module AST.AST
    ( -- * Base types
      Program
    , Function(..)
    , Term(..)
    , Type(..)
    , Gate(..)

     -- * Term functions
    , toIm

     -- * Type functions
    , convertType

     -- * Run functions
    , runFile
    , run
    , reverseType
    )
    where

import Parser.Par ( myLexer, pProgram )
import Parser.Print ( printTree )
import qualified Parser.Abs as P
import qualified Data.Map as M
import Parser.Abs (Gate(..), Bit(..))

type Env = M.Map String Integer

-- | Types for our intermediatary representation.
--   Use the same as the parser generated types.
-- type Bit = P.Bit
-- type Gate = P.Gate

-- | Representation of functions 
data Function = Func String Type Term

type Program = [Function]

name :: P.Var -> String
name (P.Var v) = v

-- | A term in our intermediatary representation.
data Term
    = Idx  Integer      -- bound
    | Fun String        -- free
    | Bit  Bit
    | Gate Gate
    | Tup  Term Term
    | App  Term Term
    | IfEl Term Term Term
    | Let Term Term
    | Abs Term
    | New
    | Meas
    | Unit
    deriving Eq

instance Show Function where
    show (Func n t e) = "\n" ++ n ++ " : " ++ show t ++ "\n"
                             ++ n ++ " = " ++ show e ++ "\n"

instance Show Term where
    show = printTree . reverseImTerm 0

data Type
    = TypeVar String -- "?a, a", FVar a, TVar a
    | TypeFlex String Type -- a or !a
    | TypeBit                      
    | TypeQBit
    | TypeUnit
    | TypeDup Type
    | Type :>< Type
    | Type :=> Type
    deriving (Eq, Ord, Read)

infixr 1 :=>
infixr 2 :><

instance Show Type where
    show = printTree . reverseType

-- | Converts from Parser type to our representation of type.
convertType :: P.Type -> Type
convertType (P.TypeVar (P.Var var)) = TypeVar var
convertType P.TypeBit               = TypeBit
convertType P.TypeQbit              = TypeQBit
convertType P.TypeVoid              = TypeUnit
convertType (P.TypeDup type')       = TypeDup (convertType type')
convertType (P.TypeTens l r)        = convertType l :>< convertType r
convertType (P.TypeFunc l r)        = convertType l :=> convertType r

-- | Converts from our type to Parser type .
reverseType :: Type -> P.Type
reverseType (TypeVar var) = P.TypeVar (P.Var var)
reverseType (TypeFlex id  fx) = P.TypeVar (P.Var ('?' : id ++ " as " ++ printTree (reverseType fx)))
reverseType TypeBit = P.TypeBit
reverseType TypeQBit = P.TypeQbit
reverseType TypeUnit = P.TypeVoid
reverseType (TypeDup type') = P.TypeDup (reverseType type')
reverseType (l :>< r) = P.TypeTens (reverseType l) (reverseType r)
reverseType (l :=> r) = P.TypeFunc (reverseType l) (reverseType r)

-- | Converts a Parser Term to our intermediatary Term.
-- 
--   de Bruijn every bound variable (Let and function abstraction).
--   Uses an environment to keep track of the Bruijn indicies.
--   Converts names such as "new" to its own terms New.
makeImTerm :: Env -> P.Term -> Term
makeImTerm env (P.TLamb _ var term) = Abs $ makeImTerm env' term
    where env' = M.insert (name var) 0 (M.map succ env)
makeImTerm env (P.TApp l r) = App (makeImTerm env l) (makeImTerm env r)
makeImTerm env (P.TVar (P.Var "new")) = New
makeImTerm env (P.TVar (P.Var "meas")) = Meas
makeImTerm env (P.TVar (P.Var "measure")) = Meas
makeImTerm env (P.TVar var) = case M.lookup (name var) env of
    Just idx -> Idx idx
    Nothing  -> Fun (name var)
makeImTerm env (P.TIfEl cond true false) = 
    IfEl (makeImTerm env cond) (makeImTerm env true) (makeImTerm env false)
makeImTerm env (P.TLet x y eq inn) = Let (makeImTerm env eq) (makeImTerm env' inn)
    where env' = M.insert (name x) 0 $ M.insert (name y) 1 (M.map (succ . succ) env)
makeImTerm env (P.TTup (P.Tuple t ts)) = foldr1 Tup $ map (makeImTerm env) (t:ts)
makeImTerm _env (P.TBit b) = Bit b
makeImTerm _env (P.TGate g) = Gate g
makeImTerm _env P.TStar = Unit

-- | Convert a function to intermediate abstract syntax (lambdaized, with de Bruijn indices)
makeImFunction :: P.FunDec -> Function
makeImFunction (P.FDecl n t fun) = Func (unfun n) (convertType t) (makeImTerm M.empty $ lambdaize fun)
    where unfun (P.FunVar x) = init $ filter (/=' ') x

-- | Convert all functions to lambda abstractions
lambdaize :: P.Function -> P.Term
lambdaize (P.FDef _ [] term) = term
lambdaize (P.FDef _n (a:args) term) = P.TLamb lambda (unArg a) (lambdaize (P.FDef _n args term))
    where unArg (P.FArg v) = v
          lambda = P.Lambda "\\"

-- | Translate abstract syntax from parser to intermediate abstract syntax
toIm :: P.Program -> Program
toIm (P.PDef fs) = map makeImFunction fs

-- | Translate the intermediate abstract syntax to the abstract parser syntax.
fromIm :: Program -> P.Program
fromIm = P.PDef . map reverseImFunction

-- | Convert a function from intermediate abstract syntax to the abstract parser syntax.
reverseImFunction :: Function -> P.FunDec
reverseImFunction (Func name t term) = P.FDecl (P.FunVar (name++" :")) (reverseType t) (P.FDef (P.Var name) [] term')
    where term' = reverseImTerm 0 term

-- | imTerm in reverse. From the intermediate term to the parser term.
reverseImTerm :: Integer -> Term -> P.Term
reverseImTerm env (Idx idx)    = P.TVar $ P.Var $ 'x' : show (env - idx - 1)
reverseImTerm env (Fun s)     = P.TVar $ P.Var s
reverseImTerm env (Bit b)      = P.TBit b 
reverseImTerm env (Gate g)     = P.TGate g
reverseImTerm env (Tup l r)    = P.TTup $ P.Tuple (reverseImTerm env l) [reverseImTerm env r] -- FIXME
reverseImTerm env (App  t1 t2) = P.TApp (reverseImTerm env t1) (reverseImTerm env t2)
reverseImTerm env (IfEl c t e) = P.TIfEl (reverseImTerm env c) (reverseImTerm env t) (reverseImTerm env e)
reverseImTerm env (Let eq inn) = P.TLet (P.Var ('x' : show (env + 1))) (P.Var ('x' : show env)) (reverseImTerm env eq) (reverseImTerm (env + 2) inn)
reverseImTerm env (Abs  term)  = P.TLamb (P.Lambda "\\") (P.Var ('x' : show env)) (reverseImTerm (env+1) term)
reverseImTerm env New          = P.TVar (P.Var "new")
reverseImTerm env Meas         = P.TVar (P.Var "meas")
reverseImTerm env Unit         = P.TStar

run :: String -> Program
run s = case pProgram (myLexer s) of
    Left s -> error s
    Right p -> toIm p

test :: String -> String
test = printTree . fromIm . run

testFile :: FilePath -> IO String
testFile path = test <$> readFile path

propTestFile :: FilePath  -> IO Bool
propTestFile path = do
    once <- testFile path
    let twice = test once
    return (once == twice)

runFile :: FilePath -> IO Program
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
