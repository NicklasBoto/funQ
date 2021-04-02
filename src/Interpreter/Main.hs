{-# language LambdaCase #-}

module Interpreter.Main where

import qualified FunQ as Q
import Parser.Abs as Abs
import qualified AST.AST as A
import Parser.Par (pProgram, myLexer)
import Control.Monad.Except
import Interpreter.Interpreter
import System.Console.Haskeline
import Control.Monad.Except
import qualified Type.HM as HM

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
                liftIO $ runExceptT $ run (w !! 1)
                loop
              _ -> do outputStrLn $ "Input " ++ input ++ " corresponds to no action" 
                      loop

type Run = ExceptT Error IO

data Error
  = ParseError String
  | TypeError HM.TypeError
  | ValueError ValueError
  | NoSuchFile FilePath
    deriving Show

-- Should be in a main pipeline file
run :: String -> Run Value
run fileName = do
    prg <- liftIO $ readFile fileName
    parsedPrg <- parse prg
    typecheck parsedPrg
    res <- liftIO $ Q.run $ runExceptT $ interpret parsedPrg
    case res of
        Left err -> do
            liftIO $ putStrLn "INTERPRETER ERROR"
            throwError $ ValueError err
        Right i -> do
            liftIO $ putStrLn $ "Result: " ++ show i
            return i


parse :: String -> Run A.Program
parse s = case pProgram (myLexer s) of
  Left err -> do
    liftIO $ putStrLn "SYNTAX ERROR"
    throwError $ ParseError err 
  Right prg -> do
    -- putStrLn $ show prg 
    -- putStrLn $ show $ A.toIm prg
    return $ A.toIm prg

-- [Function] -> Either TypeError ()
-- Func String Type Term
typecheck :: A.Program -> Run ()
typecheck p = case HM.typecheck p of
  Left err -> do
    liftIO $ putStrLn "TYPE ERROR"
    throwError $ TypeError err
  Right _ -> do
    liftIO $ putStrLn "typecheck: ok!"