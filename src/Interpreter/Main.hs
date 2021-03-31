{-# language LambdaCase #-}

module Interpreter.Main where

import qualified FunQ as Q
import Parser.Abs as Abs
import qualified AST.AST as A
import Parser.Par (pProgram, myLexer)
import Control.Monad.Except
import Interpreter.Interpreter
--import System.Console.Haskeline

-- Should be in a main pipeline file
run :: String -> IO Value
run fileName = do
    prg <- readFile fileName
    program <- parse prg
    res <- Q.run $ runExceptT $ interpret (A.toIm program)
    case res of
        Left err -> do
            putStrLn "INTERPRETER ERROR"
            putStrLn $ show err
            error "INTERPRETER ERROR"
        Right i -> do
            return i


parse :: String -> IO Program
parse s = case pProgram (myLexer s) of
  Left err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    error "SYNTAX ERROR"
  Right prg -> do
    -- putStrLn $ show prg 
    putStrLn $ show $ A.toIm prg
    return prg
