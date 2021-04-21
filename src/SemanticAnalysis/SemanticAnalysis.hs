{-# language LambdaCase #-}

module SemanticAnalysis where

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

data SemanticError 
    = FunNameMismatch String

---- Semantic analysis
-- flytta ut i egen fil
-- error som e fint :) 
semanticAnalysis :: Program -> Either SemanticError ()
semanticAnalysis p = funNameMatch p >>= anotherCheck 

funNameMatch :: Program -> Either SemanticError ()
funNameMatch p@(PDef fs) = if (all check fs) then return p else throwError $ FunNameMismatch ""
  where check :: FunDec -> Bool
        check (FDecl (FunVar s) _ (FDef (Var s') _ _)) = (funName s) == s'
        funName :: String -> String 
        funName s = takeUntil " " (takeUntil ":" s)

takeUntil :: String -> String -> String
takeUntil [] [] = []                           --don't need this
takeUntil xs [] = [] 
takeUntil [] ys = [] 
takeUntil xs (y:ys) = if isPrefixOf xs (y:ys)
                      then []
                      else y:(takeUntil xs (tail (y:ys)))

-- check main exist (remove main check from interpreter)


-- duplicate function names (se typcheckaren)


-- gateIdent blir fail! 


-- Endast 0/1 (måste göra om grammar!)


-- antalet argument är samma eller fler som antalet typer 


anotherCheck :: Program -> Either SemanticError ()
anotherCheck p = return ()