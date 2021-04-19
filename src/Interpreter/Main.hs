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
      withExceptT, replicateM )
import Data.Bifunctor ( Bifunctor(bimap) )
import Control.Exception (try)
import qualified Type.TypeChecker as TC
import Data.List

-- TODO:
-- * fixa partial application
-- * sugar for multiple arguments 

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
            case head w of
              "run" -> do
                outputStrLn $ "runs " ++ (w !! 1)
                liftIO $ runIO (w !! 1)
                loop
              _ -> do
                liftIO $ runTerminalIO $ "main : a main = " ++ input
                loop

type Run a = ExceptT Error IO a

data Error
  = ParseError String
  | TypeError TC.TypeError
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

runTerminalIO :: String -> IO ()
runTerminalIO s = runExceptT (runTerminal s) >>= \case
  Left  err -> putStrLn $ "*** Exception, " ++ show err
  Right val -> putStrLn $ "Output: " ++ show val

runTerminal :: String -> Run I.Value
runTerminal s = parse s >>= typecheck >>= eval

readfile :: FilePath -> Run String
readfile path = do
  e <- liftIO (try (readFile path) :: IO (Either IOError String))
  case e of
    Left  _ -> throwError $ NoSuchFile path
    Right s -> return s

parse = toErr (pProgram . myLexer) ParseError A.toIm

typecheck :: A.Program -> Run A.Program
typecheck = toErr TC.typecheck TypeError . const <*> id

eval :: A.Program -> Run I.Value
eval = withExceptT ValueError . mapExceptT Q.run . I.interpret

rundistest :: FilePath -> IO ()
rundistest path = do
  res <- runExceptT $ rundist path
  case res of
    Left err -> print err
    Right r  -> gatherResults r

rundist :: FilePath -> Run [I.Value]
rundist path = do 
  a <- readfile path >>= parse >>= typecheck
  evaldist a 300


evaldist :: A.Program -> Int -> Run [I.Value]
evaldist prg reps = replicateM reps $ (withExceptT ValueError . mapExceptT Q.run . I.interpret) prg
--   where 

        -- sortera listan 
        -- -> plocka ut unika element 
        -- -> räkna antalet av de olika unika elementen i startlistan
        -- -> konvertera till binärt
        -- -> printa andelar 
-- input (1,0,1)

exVTup :: I.Value
exVTup = I.VTup (I.VBit 1) (I.VTup (I.VBit 0) (I.VBit 1))


readtup :: I.Value -> Int
readtup tup@(I.VTup a as) = (toDec . catchBit . reverse . I.fromVTup) tup
  where catchBit []            = []
        catchBit (I.VBit b:bs) = (fromIntegral . toInteger) b : catchBit bs
        toDec []        = 0
        toDec (b:bs)    = b + 2*toDec bs

findUniques []     as = as
findUniques (b:bs) as = if b `elem` as then findUniques bs as else findUniques bs (insert b as)
countOcc as = map length $ (group . sort) as
uniquesAndCount as = zip (sort (findUniques as [])) (countOcc as)

toBin 0 = []
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
toBin n | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

stat :: Int -> [(Int, Int)] -> [(Int, Double, Int)]
stat _   []         = []
stat len ((a,b):as) = (a, dub b/dub len, b) : stat len as
  where dub = fromIntegral . toInteger

prettystats :: (Int, Double, Int) -> String
prettystats (a,b,c) = show a ++ ": " ++ "\t" ++ (show . truncateboi) b ++ "%" ++ "\t" ++ show c

-- truncateboi :: Double -> Double
truncateboi :: Double -> Double
truncateboi d = (fromIntegral . truncate) (10000*(d :: Double))/100

gatherResults :: [I.Value] -> IO ()
gatherResults vals@(I.VTup _ _:as) = do
  let res = uniquesAndCount $ map readtup vals
  let stats = stat (length vals) res
  mapM_ (putStrLn . prettystats) stats