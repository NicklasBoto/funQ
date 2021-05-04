{-# language LambdaCase #-}

module SemanticAnalysis.SemanticAnalysis (runAnalysis, SemanticError(..)) where

import Data.List
import Parser.Abs
import qualified Data.Set as Set
import Data.Char ( digitToInt, isDigit, isLetter )

data SemanticError 
  = FunNameMismatch String 
  | NoMainFunction
  | DuplicateFunction String
  | UnknownGate String
  | InvalidBit String
  | TooManyArguments String

instance Show SemanticError where
  show (FunNameMismatch e) = "FunNameMismatch: Mismatchig names in function declaration and definition for " ++ e
  show NoMainFunction = "NoMainFunction: No main function has been declared"
  show (DuplicateFunction e) = "DuplicateFunction: Duplicate function declarations for " ++ e
  show (UnknownGate e) = "UnknownGate: Gates not recognized: " ++ e
  show (TooManyArguments e) = "TooManyArguments: " ++ e
  show (InvalidBit e) = "InvalidBit: Expected value of bits to be 0 or 1 but got " ++ e

runAnalysis :: Program -> Either SemanticError ()
runAnalysis (PDef fs) = do 
  mainDefined fs
  funNameMatch fs
  dupFun fs
  unknownGate fs
  onlyBits fs
  correctNumberOfArgs fs

-- | Checks if main function is defined
mainDefined :: [FunDec] -> Either SemanticError ()
mainDefined fs = if any isMain fs then Right () else Left NoMainFunction
  where isMain f = funName f == "main"

-- | Checks that function name in type signature matches the name in function definition
funNameMatch :: [FunDec] -> Either SemanticError ()
funNameMatch fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl (FunVar s) _ (FDef (Var s') _ _)) = funName' s == s' 
        funName' s = takeUntil " " (takeUntil ":" s)
        genErr     = FunNameMismatch
        errorMsg f = funName f

-- | Checks that no functions are declared more than once
dupFun :: [FunDec] -> Either SemanticError ()
dupFun fs = checkSemantics fs isValid genErr errorMsg
  where isValid f  = length (filter (== funName f) funNames) == 1
        funNames   = map funName fs
        genErr     = DuplicateFunction
        errorMsg f = funName f

-- | Checks that there are no unknown gates present. Primarly, this function checks gates 
-- | that are acceptable through the catch-all GateIdent term in the grammar. Typically,
-- | gates that are included as such can not be easily specified without massive repitition
-- | in the grammar or are generic (for instance the phase shift CR).
unknownGate :: [FunDec] -> Either SemanticError ()
unknownGate fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl _ _ (FDef _ _ t)) = length (unknownGates t []) == 0
        unknownGates :: Term -> [String] -> [String]
        unknownGates (TGate (GGate (GateIdent g))) gs
          | init g == "QFT" && length g == 4 && (digitToInt $ last g) <= 5 = gs
          | init g == "QFTI" && length g == 5 && (digitToInt $ last g) <= 5 = gs
          | takeWhile isLetter g == "CR" && all isDigit (dropWhile isLetter g) = gs 
          | takeWhile isLetter g == "CRI" && all isDigit (dropWhile isLetter g) = gs
          | takeWhile isLetter g == "CCR" && all isDigit (dropWhile isLetter g) = gs 
          | takeWhile isLetter g == "CCRI" && all isDigit (dropWhile isLetter g) = gs
          | otherwise = gs ++ [g]
        unknownGates (TApp t1 t2) gs                  = gs ++ unknownGates t1 [] ++ unknownGates t2 []
        unknownGates (TIfEl t1 t2 t3) gs              = gs ++ unknownGates t1 [] ++ unknownGates t2 [] ++ unknownGates t3 [] 
        unknownGates (TLet _ _ t1 t2) gs              = gs ++ unknownGates t1 [] ++ unknownGates t2 []
        unknownGates (TLamb _ _ _ t1) gs              = gs ++ unknownGates t1 []
        unknownGates _ gs                             = gs

        genErr = UnknownGate
        errorMsg (FDecl _ _ (FDef _ _ t)) = concat $ intersperse ", " $ unknownGates t []

-- | Checks that bits only are accept zero or ones.     
onlyBits :: [FunDec] -> Either SemanticError ()
onlyBits fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl _ _ (FDef _ _ t)) = length (invalidBit t []) == 0
        invalidBit :: Term -> [String] -> [String]
        invalidBit (TBit (BBit n)) gs   = if not (n == 0 || n == 1) then show n : gs else gs
        invalidBit (TApp t1 t2) gs      = gs ++ invalidBit t1 [] ++ invalidBit t2 []
        invalidBit (TIfEl t1 t2 t3) gs  = gs ++ invalidBit t1 [] ++ invalidBit t2 [] ++ invalidBit t3 [] 
        invalidBit (TLet _ _ t1 t2) gs  = gs ++ invalidBit t1 [] ++ invalidBit t2 []
        invalidBit (TLamb _ _ _ t1) gs    = gs ++ invalidBit t1 []
        invalidBit _ gs                 = gs
        genErr = InvalidBit
        errorMsg (FDecl _ _ (FDef _ _ t)) = concat $ intersperse ", " $ invalidBit t []

-- | Checks that not too many function arguments are used for function definition.
correctNumberOfArgs :: [FunDec] -> Either SemanticError ()
correctNumberOfArgs fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl _ t (FDef _ args _)) = length args < size t
        size (TypeFunc t1 t2) = size t1 + size t2
        size (TypeTens t1 t2) = size t1 + size t2
        size (TypeDup t)      = size t 
        size _                = 1
        genErr = TooManyArguments
        errorMsg f@(FDecl _ t (FDef _ args _)) = "function " ++ funName f ++ " has " ++ (show $ length args) ++ " arguments but it only takes " ++ (show $ size t - 1)

-- Utils
-- | Iterates functions, checks given predicate and collects all errors, then
-- | prints entire error. 
checkSemantics :: [FunDec] -> (FunDec -> Bool) -> (String -> SemanticError) -> (FunDec -> String) -> Either SemanticError ()
checkSemantics fs isValid err errMsg = if null errors then Right () else Left $ err errors 
  where errors = concat $ intersperse ", " $ unique $ check fs []
        check :: [FunDec] -> [String] -> [String]   
        check [] ms = ms
        check (f:fs) ms = if isValid f then check fs ms else check fs (errMsg f : ms)  
        unique = Set.toList . Set.fromList

takeUntil :: String -> String -> String
takeUntil _ [] = [] 
takeUntil [] _ = [] 
takeUntil xs (y:ys) = if isPrefixOf xs (y:ys) then [] else y:(takeUntil xs (tail (y:ys)))

funName :: FunDec -> String 
funName (FDecl _ _ (FDef (Var s) _ _)) = s