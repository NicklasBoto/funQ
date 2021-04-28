module AST.AST
    ( -- * Base types
      Program
    , Function(..)
    , Term(..)
    , Type(..)
    , AST.AST.Gate(..)

     -- * Term functions
    , toIm

     -- * Type functions
    , convertType

     -- * Run functions
    , runFile
    , run
    , reverseType
    , AST.AST.Bit(..)
    ) 
    where

import Parser.Par ( myLexer, pProgram )
import Parser.Print ( printTree )
import qualified Parser.Abs as P
import qualified Data.Map as M
import Data.Char

type Env = M.Map String Integer

-- | Types for our intermediatary representation.
--   Use the same as the parser generated types.
-- type Bit = P.Bit
-- type Gate = P.Gate

-- | Representation of functions 
data Function = Func String Type Term

type Program = [Function]

fname :: P.FunVar -> String
fname (P.FunVar v) = filter (\x -> x /= ' ' && x /= ':') v

name :: P.Var -> String
name (P.Var v) = v

nameL :: P.LetVar -> String
nameL (P.LVar v) = name v

-- | A term in our intermediatary representation.
data Term
    = Idx  Integer      -- bound
    | Fun String        -- free
    | Bit AST.AST.Bit
    | Gate Gate
    | Tup  Term Term
    | App  Term Term
    | IfEl Term Term Term
    | Let Term Term
    | Abs Type Term
    | New
    | Meas
    | Unit
    deriving Eq

data Gate
    = GH
    | GX
    | GY
    | GZ
    | GI
    | GS
    | GT
    | GCNOT
    | GTOF
    | GSWP
    | GFRDK
    | GQFT Int
    | GQFTI Int
    | GCR Int
    | GCRI Int
    | GGate P.GateIdent
  deriving (Eq, Ord, Show, Read)

data Bit = BZero | BOne
  deriving (Eq, Ord, Show, Read)
instance Show Function where
    show (Func n t e) = "\n" ++ n ++ " : " ++ show t ++ "\n"
                             ++ n ++ " = " ++ show e ++ "\n"

instance Show Term where
    show = printTree . reverseImTerm 0

data Type
    = TypeBit                      
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
convertType P.TypeBit         = TypeBit
convertType P.TypeQbit        = TypeQBit
convertType P.TypeVoid        = TypeUnit
convertType (P.TypeDup type') = TypeDup (convertType type')
convertType (P.TypeTens l r)  = convertType l :>< convertType r
convertType (P.TypeFunc l r)  = convertType l :=> convertType r

-- | Converts from our type to Parser type .
reverseType :: Type -> P.Type
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
makeImTerm env (P.TLamb _ var type' term) = Abs (convertType type') $ makeImTerm env' term
    where env' = M.insert (fname var) 0 (M.map succ env)
makeImTerm env (P.TApp l r) = App (makeImTerm env l) (makeImTerm env r)
makeImTerm env (P.TVar (P.Var "new")) = New
makeImTerm env (P.TVar (P.Var "meas")) = Meas
makeImTerm env (P.TVar (P.Var "measure")) = Meas
makeImTerm env (P.TVar var) =  case M.lookup (name var) env of
    Just idx -> Idx idx
    Nothing  -> Fun (name var)
makeImTerm env (P.TIfEl cond true false) =
    IfEl (makeImTerm env cond) (makeImTerm env true) (makeImTerm env false)
makeImTerm env (P.TLet x [y] eq inn) = Let (makeImTerm env eq) (makeImTerm (letEnv y x env) inn)
makeImTerm env (P.TLet x (y:ys) eq inn) = Let (makeImTerm env eq) (makeImTerm (letEnv y x env) (P.TLet y ys (toTerm y) inn))
makeImTerm env (P.TTup (P.Tuple t ts)) = foldr1 Tup $ map (makeImTerm env) (t:ts)
makeImTerm _env (P.TBit (P.BBit 0)) = Bit BZero 
makeImTerm _env (P.TBit (P.BBit 1)) = Bit BOne 
makeImTerm _env (P.TGate (P.GGate (P.GateIdent g))) 
    | init g == "QFT"  = Gate $ GQFT (nums g)
    | init g == "QFTI" = Gate $ GQFTI (nums g)
    | takeWhile isLetter g == "CR" = Gate $ GCR (nums g) 
    | takeWhile isLetter g == "CRI" = Gate $ GCRI (nums g) 
    where nums = read . dropWhile isLetter
makeImTerm _env (P.TGate g) = Gate $ gateToASTGate g 
makeImTerm _env P.TStar = Unit

letEnv :: P.LetVar -> P.LetVar -> Env -> Env
letEnv x y env = M.insert (nameL y) 1 $ M.insert (nameL x) 0 (M.map (succ . succ) env)

toTerm :: P.LetVar -> P.Term
toTerm (P.LVar v) = P.TVar v

-- | Convert a function to intermediate abstract syntax (lambdaized, with de Bruijn indices)
makeImFunction :: P.FunDec -> Function
makeImFunction (P.FDecl _ t function) = Func name (convertType t) term --(unfun n) (convertType t) (makeImTerm M.empty $ lambdaize fun)
    where
        (P.FDef (P.Var name) args body) = function
        term = makeImTerm M.empty $ lambdaize (debangFunc t) args body

-- | Debangs outer level of a type.
debangFunc :: P.Type -> P.Type
debangFunc (P.TypeDup t@(P.TypeFunc n p)) = t
debangFunc t = t

-- | Lambdaizes and types the argument types based on the type signature.
lambdaize :: P.Type -> [P.Arg] -> P.Term -> P.Term
lambdaize _t [] body                                    = body 
lambdaize (P.TypeFunc n p) (P.FArg (P.Var v) : vs) body = P.TLamb (P.Lambda "\\") (P.FunVar v) n (lambdaize p vs body)
lambdaize (P.TypeDup (P.TypeFunc n p)) (P.FArg (P.Var v) : vs) body = P.TLamb (P.Lambda "\\") (P.FunVar v) n (lambdaize p vs body)

-- | Translate abstract syntax from parser to intermediate abstract syntax
toIm :: P.Program -> Program
toIm (P.PDef fs) = map makeImFunction fs

-- | Translate the intermediate abstract syntax to the abstract parser syntax.
fromIm :: Program -> P.Program
fromIm = P.PDef . map reverseImFunction

-- | Convert a function from intermediate abstract syntax to the abstract parser syntax.
reverseImFunction :: Function -> P.FunDec
reverseImFunction (Func name type' term) = P.FDecl (P.FunVar name) (reverseType type')function
    where
        function = P.FDef (P.Var name) [] (reverseImTerm 0 term)

-- | imTerm in reverse. From the intermediate term to the parser term.
reverseImTerm :: Integer -> Term -> P.Term
reverseImTerm env (Idx idx)    = P.TVar $ P.Var $ 'x' : show (env - idx - 1)
reverseImTerm env (Fun s)      = P.TVar $ P.Var s
reverseImTerm env (Bit BZero)  = P.TBit $ P.BBit 0
reverseImTerm env (Bit BOne)   = P.TBit $ P.BBit 1
reverseImTerm env (Gate g)     = P.TGate P.GH -- undefined -- P.TGate g --FIXME
reverseImTerm env (Tup l r)    = P.TTup $ P.Tuple (reverseImTerm env l) [reverseImTerm env r] -- FIXME
reverseImTerm env (App  t1 t2) = P.TApp (reverseImTerm env t1) (reverseImTerm env t2)
reverseImTerm env (IfEl c t e) = P.TIfEl (reverseImTerm env c) (reverseImTerm env t) (reverseImTerm env e)
reverseImTerm env (Let eq inn) = P.TLet ((P.LVar . P.Var) $ 'y' : show (env + 1)) 
                               [(P.LVar . P.Var) $ 'x' : show env] (reverseImTerm env eq) (reverseImTerm (env + 2) inn)
reverseImTerm env (Abs type' term)  = P.TLamb (P.Lambda "\\") (P.FunVar ('x' : show env)) (reverseType type') (reverseImTerm (env+1) term)
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

gateToASTGate :: P.Gate -> Gate 
gateToASTGate g = case g of
    P.GH    -> GH
    P.GX    -> GX
    P.GY    -> GY
    P.GZ    -> GZ
    P.GI    -> GI
    P.GS    -> GS
    P.GT    -> AST.AST.GT
    P.GCNOT -> GCNOT
    P.GTOF  -> GTOF
    P.GSWP  -> GSWP
    P.GFRDK -> GFRDK  

-- cause this might vanish from Parser.Abs
-- instance C.Show Type where
  -- show (TypeVar (Variable s)) = s
  -- show TypeBit = "Bit"
  -- show TypeQbit = "QBit"
  -- show TypeVoid = "⊤"
  -- show (TypeDup t) = "!" C.++ C.show t
  -- show (TypeTens l r) = C.show l C.++ " ⊗ " C.++ C.show r
  -- show (TypeFunc l r) = C.show l C.++ " ⊸ " C.++ C.show r
