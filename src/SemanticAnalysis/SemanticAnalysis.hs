{-# language LambdaCase #-}

module SemanticAnalysis.SemanticAnalysis where

import qualified FunQ as Q
import qualified AST.AST as A
import Parser.Par (pProgram, myLexer)
import qualified Interpreter.Interpreter as I
import System.Console.Haskeline
import Control.Monad.Except
    ( MonadIO(liftIO),
      MonadError(throwError),
      ExceptT(..),
      mapExceptT,
      runExceptT,
      withExceptT, replicateM )
import Data.Bifunctor ( Bifunctor(bimap) )
import Control.Exception (try)
import qualified Type.HM as HM
import Data.List
import Parser.Abs
import qualified Data.Set as Set

data SemanticError 
  = FunNameMismatch String
  | NoMainFunction String
  | DupFun String
  | UnknownGate String
    deriving Show

semanticAnalysis :: Program -> Either SemanticError ()
semanticAnalysis p = dupFun p -- funNameMatch p -- >>= mainDefined p 


funNameMatch :: Program -> Either SemanticError ()
funNameMatch p@(PDef fs) = if null mismatches then Right () else Left $ FunNameMismatch $ "Mismatchig names in function declaration and definition for " ++ mismatches -- mapM_ check fs
  where mismatches = concat $ intersperse ", " $ check fs []
        check :: [FunDec] -> [String] -> [String]   
        check [] ms = ms
        check ((FDecl (FunVar s) _ (FDef (Var s') _ _)):fs) ms = if (funName' s) == s' then check fs ms else check fs (ms ++ [(funName' s)])  
        funName' :: String -> String 
        funName' s = takeUntil " " (takeUntil ":" s)
    

mainDefined :: Program -> Either SemanticError ()
mainDefined (PDef fs) = if any isMain fs then Right () else Left $ NoMainFunction "No main function has been declared"
  where isMain f = funName f == "main"



dupFun :: Program -> Either SemanticError ()
dupFun (PDef fs) = if null collectErrors then Right () else Left $ DupFun $ "Duplicate functions declarations for " ++ collectErrors
  where collectErrors = concat $ intersperse ", " $ check funNames []
        check :: [String] -> [String] -> [String]
        check []     errors = errors 
        check (f:fs) errors = if dup f && (not $ elem f fs) then check fs (f:errors) else check fs errors
        funNames = map funName fs
        dup :: String -> Bool
        dup f = length (filter (== f) funNames) > 1
        

-- gateIdent blir fail! 
unknownGate :: Program -> Either SemanticError ()
unknownGate (PDef fs) = undefined -- then Right () else Left $ UnknownGate ""
  where check :: [FunDec] -> [String] -> [String]
        check [] gs = gs
        check ((FDecl _ _ (FDef _ _ t)):fs) gs = if null $ unknownGates t then check fs gs else check fs (gs ++ unknownGates t)
        unknownGates :: Term -> [String]
        unknownGates = undefined

      --   data Term
      --   = TVar Var
      --   | TBit Bit
      --   | TGate Gate
      --   | TTup Tup
      --   | TStar
      --   | TApp Term Term
      --   | TIfEl Term Term Term
      --   | TLet LetVar [LetVar] Term Term
      --   | TLamb Lambda Var Term
      -- deriving (C.Eq, C.Ord, C.Show, C.Read)

-- Endast 0/1 (måste göra om grammar!)


-- antalet argument är samma eller fler som antalet typer 


-- Utils
takeUntil :: String -> String -> String
takeUntil [] [] = []                           --don't need this
takeUntil xs [] = [] 
takeUntil [] ys = [] 
takeUntil xs (y:ys) = if isPrefixOf xs (y:ys)
                      then []
                      else y:(takeUntil xs (tail (y:ys)))

funName :: FunDec -> String 
funName (FDecl _ _ (FDef (Var s) _ _)) = s