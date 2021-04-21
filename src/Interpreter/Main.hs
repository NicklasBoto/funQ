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
      withExceptT, replicateM, zipWithM )
import Data.Bifunctor ( Bifunctor(bimap) )
import Control.Exception (try)
import qualified Type.TypeChecker as TC
import Data.List
import Data.Maybe

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
                liftIO $ runTerminalIO $ "main : T main = " ++ input
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

readfile :: FilePath -> Run String
readfile path = do
  e <- liftIO (try (readFile path) :: IO (Either IOError String))
  case e of
    Left  _ -> throwError $ NoSuchFile path
    Right s -> return s

parse = toErr (pProgram . myLexer) ParseError A.toIm

typecheck :: A.Program -> Run A.Program
typecheck = toErr TC.typecheck TypeError . const <*> id

runTerminalIO :: String -> IO ()
runTerminalIO s = runExceptT (runTerminal s) >>= \case
  Left  err   -> putStrLn $ "*** Exception, " ++ show err
  Right (v,t) -> putStrLn $ show v ++ " : " ++ show t

runTerminal :: String -> Run (I.Value, A.Type)
runTerminal s = do
  p@[A.Func _ _ term] <- parse s
  typ <- toErr (TC.runCheck . TC.infer) TypeError id term
  val <- eval p
  return (val, typ)

eval :: A.Program -> Run I.Value
eval = withExceptT ValueError . mapExceptT Q.run . I.interpret

-- | Distribution testing code below

rundistest :: FilePath -> IO ()
rundistest path = do
  res <- runExceptT $ rundist path
  case res of
    Left err -> print err
    Right r  -> gatherResults r

rundist :: FilePath -> Run [I.Value]
rundist path = do
  a <- readfile path >>= parse >>= typecheck
  evaldist a 10

evaldist :: A.Program -> Int -> Run [I.Value]
evaldist prg reps = replicateM reps $ (withExceptT ValueError . mapExceptT Q.run . I.interpret) prg

gatherResults :: [I.Value] -> IO ()
gatherResults vals = do
  let res = countUniques $ map readtup vals
  let stats = stat (length vals) res
  mapM_ (putStrLn . prettystats) stats

readtup :: I.Value -> Int
readtup = toDec . catchBit . reverse . I.fromVTup
  where catchBit []            = []
        catchBit (I.VBit b:bs) = (fromIntegral . toInteger) b : catchBit bs
        toDec []     = 0
        toDec (b:bs) = b + 2*toDec bs

countUniques :: [Int] -> [(Int, Int)]
countUniques as = zip (sort (findUniques as [])) (countOcc as)
  where findUniques []     as = as
        findUniques (b:bs) as = if b `elem` as then findUniques bs as else findUniques bs (insert b as)
        countOcc as           = map length $ (group . sort) as

stat :: Int -> [(Int, Int)] -> [(Int, Double, Int)]
stat _   []         = []
stat len ((a,b):as) = (a, dub b/dub len, b) : stat len as
  where dub = fromIntegral . toInteger

prettystats :: (Int, Double, Int) -> String
prettystats (a,b,c) = concatMap show ((fillzeros . toBin) a) ++ ": " ++ "\t" ++ (show . truncateboi) b ++ "%" ++ "\t" ++ show c
  where toBin 0 = []
        toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        toBin n | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
        truncateboi d = (fromIntegral . truncate) (10000*(d :: Double))/100
        fillzeros as = if length as == 4 then as else replicate (4 - length as) 0 ++ as


-- | Adder testing code below, non-functional right now
-- runAdderTest :: FilePath -> IO ()
-- runAdderTest path = do
--   ss <- readFile path
--   let indsA = splitInto3 $ findinputinds ss "A"
--   let indsB = splitInto3 $ findinputinds ss "B"
--   let inpsA = splitInto3 (map show inputs1)
--   let inpsB = splitInto3 (map show inputs2)
--   res <- runExceptT (zipWithM4 (runAdder path) indsA indsB inpsA inpsB :: Run [[I.Value]])
--   case res of
--     Left err -> print err
--     Right r  -> print r

runAdder :: FilePath -> [Int] -> [Int] -> [String] -> [String] -> Run [I.Value]
runAdder path indsA indsB inputsA inputsB = do
  a <- liftIO $ readFile path -- >>= parse >>= typecheck
  let b = applyInputs (words a) indsA inputsA
  let c = unwords $ applyInputs b indsA inputsA
  q <- parse c >>= typecheck
  evaldist q 1

  -- evaldist a 10
-- ta första 3 av varje, applicera, kör, spara resultat, repetera
-- tar in 3 av indexA, 3 av indexB, 3 av värdenA, 3 av värdenB, sträng; ger ut strängen

splitInto3 :: [a] -> [[a]]
splitInto3 []    = []
splitInto3 [b]   = [[b]]
splitInto3 [a,b] = [[a,b]]
splitInto3 as    = let (a,b) = splitAt 3 as in a : splitInto3 b

applyInputs :: [String] -> [Int] -> [String] -> [String]
applyInputs ss inds inputs = foldl applyInput ss indputs
  where indputs = zip inds inputs

applyInput ::  [String] -> (Int, String) -> [String]
applyInput ss (ind, inp)= replaceNth ss ind inp

replaceNth :: [String] -> Int -> String -> [String]
replaceNth [] _ _  = []
replaceNth (x:xs) n newVal
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth xs (n-1) newVal

compareResults :: [I.Value] -> IO ()
compareResults vals = undefined

findinputinds :: String -> String -> [Int]
findinputinds s which = inputA : inputA + 2 : [inputA + 4]
  where inputA  = fromMaybe (-1) (elemIndex ("exInt" ++ which) ss)
        ss      = words s

inputs1, inputs2 :: [Int]
inputs1 = map fst (inputslist 3)
inputs2 = map snd (inputslist 3)

inputslist :: Int -> [(Int, Int)]
inputslist n = [(a,b) | a <- [0..2^n-1], b <- [0..2^n-1]]

outputs :: [Int] -> [Int]
outputs as = zipWith (+) inputs1 inputs2

test :: IO [String]
test = do
  a <- readFile "test/interpreter-test-suite/qft-adder3.fq"
  return $ words a

toBin 0 = []
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
toBin n | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]