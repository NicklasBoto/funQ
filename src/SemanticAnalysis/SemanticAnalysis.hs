{-# language LambdaCase #-}

module SemanticAnalysis.SemanticAnalysis (runAnalysis, SemanticError(..)) where

import Data.List
import Parser.Abs
import qualified Data.Set as Set

data SemanticError 
  = FunNameMismatch String
  | NoMainFunction String
  | DuplicateFunction String
  | UnknownGate String
  | InvalidBit String
  | TooManyArguments String

instance Show SemanticError where
  show (FunNameMismatch e) = "FunNameMismatch: " ++ e
  show (NoMainFunction e) = "NoMainFunction: " ++ e
  show (DuplicateFunction e) = "DuplicateFunction: " ++ e
  show (UnknownGate e) = "UnknownGate: " ++ e
  show (TooManyArguments e) = "TooManyArguments: " ++ e
  show (InvalidBit e) = "InvalidBit: " ++ e

runAnalysis :: Program -> Either SemanticError ()
runAnalysis (PDef fs) = do 
  mainDefined fs
  funNameMatch fs
  dupFun fs
  unknownGate fs
  onlyBits fs
  correctNumberOfArgs fs

mainDefined :: [FunDec] -> Either SemanticError ()
mainDefined fs = if any isMain fs then Right () else Left $ NoMainFunction "No main function has been declared"
  where isMain f = funName f == "main"

funNameMatch :: [FunDec] -> Either SemanticError ()
funNameMatch fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl (FunVar s) _ (FDef (Var s') _ _)) = funName' s == s' 
        funName' s = takeUntil " " (takeUntil ":" s)
        genErr e   = FunNameMismatch $ "Mismatchig names in function declaration and definition for " ++ e
        errorMsg f = funName f

dupFun :: [FunDec] -> Either SemanticError ()
dupFun fs = checkSemantics fs isValid genErr errorMsg
  where isValid f  = length (filter (== funName f) funNames) == 1 -- && not (elem (funName f) funNames)
        funNames   = map funName fs
        genErr e   = DuplicateFunction $ "Duplicate function declarations for " ++ e
        errorMsg f = funName f

unknownGate :: [FunDec] -> Either SemanticError ()
unknownGate fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl _ _ (FDef _ _ t)) = length (unknownGates t []) == 0
        unknownGates :: Term -> [String] -> [String]
        unknownGates (TGate (GGate (GateIdent g))) gs = gs ++ [g]
        unknownGates (TApp t1 t2) gs                  = gs ++ unknownGates t1 [] ++ unknownGates t2 []
        unknownGates (TIfEl t1 t2 t3) gs              = gs ++ unknownGates t1 [] ++ unknownGates t2 [] ++ unknownGates t3 [] 
        unknownGates (TLet _ _ t1 t2) gs              = gs ++ unknownGates t1 [] ++ unknownGates t2 []
        unknownGates (TLamb _ _ t1) gs                = gs ++ unknownGates t1 []
        unknownGates _ gs                             = gs
        genErr e = UnknownGate $ e ++ " are not predefined gates"
        errorMsg (FDecl _ _ (FDef _ _ t)) = concat $ intersperse ", " $ unknownGates t []
        
onlyBits :: [FunDec] -> Either SemanticError ()
onlyBits fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl _ _ (FDef _ _ t)) = length (invalidBit t []) == 0
        invalidBit :: Term -> [String] -> [String]
        invalidBit (TBit (BBit n)) gs   = if not (n == 0 || n == 1) then show n : gs else gs
        invalidBit (TApp t1 t2) gs      = gs ++ invalidBit t1 [] ++ invalidBit t2 []
        invalidBit (TIfEl t1 t2 t3) gs  = gs ++ invalidBit t1 [] ++ invalidBit t2 [] ++ invalidBit t3 [] 
        invalidBit (TLet _ _ t1 t2) gs  = gs ++ invalidBit t1 [] ++ invalidBit t2 []
        invalidBit (TLamb _ _ t1) gs    = gs ++ invalidBit t1 []
        invalidBit _ gs                 = gs
        genErr e = InvalidBit $ "Expected value of bits to be 0 or 1 but got " ++ e
        errorMsg (FDecl _ _ (FDef _ _ t)) = concat $ intersperse ", " $ invalidBit t []

correctNumberOfArgs :: [FunDec] -> Either SemanticError ()
correctNumberOfArgs fs = checkSemantics fs isValid genErr errorMsg
  where isValid (FDecl _ t (FDef _ args _)) = length args < size t
        size (TypeFunc t1 t2) = size t1 + size t2
        size (TypeTens t1 t2) = size t1 + size t2
        size (TypeDup t)      = size t 
        size _                = 1
        genErr e = TooManyArguments $ e
        errorMsg f@(FDecl _ t (FDef _ args _)) = "function " ++ funName f ++ " has " ++ (show $ length args) ++ " arguments but it only takes " ++ (show $ size t - 1)

-- Utils
checkSemantics :: [FunDec] -> (FunDec -> Bool) -> (String -> SemanticError) -> (FunDec -> String) -> Either SemanticError ()
checkSemantics fs isValid err errMsg = if null errors then Right () else Left $ err errors 
  where errors = concat $ intersperse ", " $ unique $ check fs []
        check :: [FunDec] -> [String] -> [String]   
        check [] ms = ms
        check (f:fs) ms = if isValid f then check fs ms else check fs (errMsg f : ms)  
        unique = Set.toList . Set.fromList

takeUntil :: String -> String -> String
takeUntil xs [] = [] 
takeUntil [] ys = [] 
takeUntil xs (y:ys) = if isPrefixOf xs (y:ys)
                      then []
                      else y:(takeUntil xs (tail (y:ys)))

funName :: FunDec -> String 
funName (FDecl _ _ (FDef (Var s) _ _)) = s