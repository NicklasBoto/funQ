{-# language LambdaCase #-}

module Interpreter.Main where

import qualified FunQ as Q
import Parser.Abs as Abs
import qualified AST.AST as A
import Parser.Par (pProgram, myLexer)
import Control.Monad.Except
import Interpreter.Interpreter
import System.Console.Haskeline

-- TODO: 
-- * köra fq utryck i cmd (utan att mata in en fil)
-- * flytta ut till direkt under src
-- * koppla ihop med typechecker!
-- * inte ska dö om interpreter/typechecker failar
-- * coolt: kunna loada en fil och köra funktioner
-- * kunna skriva run [filnamn] utan hela sökvägen -> letar i subdirectories efter filen
main :: IO ()
main = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "λ: "
      case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just input -> do
            let w = words input 
            outputStrLn $ "w: " ++ show w
            case head w of
              "run" -> do 
                outputStrLn $ "runs " ++ (w !! 1)
                liftIO $ run (w !! 1)
                loop
              _ -> do outputStrLn $ "Input " ++ input ++ " corresponds to no action" 
                      loop

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
            putStrLn $ "Result: " ++ show i
            return i


parse :: String -> IO Program
parse s = case pProgram (myLexer s) of
  Left err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    error "SYNTAX ERROR"
  Right prg -> do
    -- putStrLn $ show prg 
    -- putStrLn $ show $ A.toIm prg
    return prg
