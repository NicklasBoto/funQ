{-# language LambdaCase #-}

module Interpreter.Main where

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
      withExceptT )
import Data.Bifunctor ( Bifunctor(bimap) )
import Control.Exception (try)
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
                liftIO $ runIO (w !! 1)
                loop
              _ -> do outputStrLn $ "Input " ++ input ++ " corresponds to no action" 
                      loop

type Run = ExceptT Error IO

data Error
  = ParseError String
  | TypeError HM.TypeError
  | ValueError I.ValueError
  | NoSuchFile FilePath

instance Exception Error

instance Show Error where
  show (ParseError e) =
    "syntax error:\n" ++ e

  show (TypeError e) =
    "type error:\n" ++ show e

  show (ValueError e) =
    "value error:\n" ++ show e

  show (NoSuchFile f) =
    "file not found: " ++ f

runIO :: FilePath -> IO ()
runIO path = runExceptT (run path) >>= \case
  Left  err -> putStrLn $ "*** Exception, " ++ show err
  Right val -> print val

toErr :: (i -> Either e v) -> (e -> Error) -> (v -> o) -> i -> Run o
toErr f l r = ExceptT . return . bimap l r . f

run :: FilePath -> Run I.Value
run path = readfile path >>= parse >>= typecheck >>= eval

readfile :: FilePath -> Run String
readfile path = do
  e <- liftIO (try (readFile path) :: IO (Either IOError String))
  case e of
    Left  _ -> throwError $ NoSuchFile path
    Right s -> return s

parse :: String -> Run A.Program
parse = toErr (pProgram . myLexer) ParseError A.toIm

typecheck :: A.Program -> Run A.Program
typecheck = toErr HM.typecheck TypeError . const <*> id

eval :: A.Program -> Run I.Value
eval = withExceptT ValueError . mapExceptT Q.run . I.interpret