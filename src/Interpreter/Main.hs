{-# language LambdaCase #-}

module Interpreter.Main where

import qualified FunQ as Q
import qualified AST.AST as A
import Parser.Par (pProgram, myLexer)
import qualified Interpreter.Interpreter as I
import System.Console.Haskeline
import Control.Monad.Except
  ( MonadIO(liftIO),
      MonadTrans(lift),
      MonadError(throwError),
      ExceptT(..),
      mapExceptT,
      runExceptT,
      withExceptT, replicateM, zipWithM )
import Data.Bifunctor ( Bifunctor(bimap, first) )
import Control.Exception (try)
import qualified Type.TypeChecker as TC
import Data.List
import Data.Maybe
import Control.Monad.State.Lazy

import Parser.Abs
import qualified SemanticAnalysis.SemanticAnalysis as S

-- | Runs the funq interpreter.
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "λ: "
      case minput of
          Nothing -> return ()
          Just ":q" -> return ()
          Just ":help" -> outputStrLn ":q to quit, :run filename to run a file, type to expressions" >> loop
          Just input -> do
            let w = words input
            case head w of 
              ":run" -> do
                case length w of
                  1 -> outputStrLn "Need to specify file to run" >> loop
                  _ -> do
                    outputStrLn $ "runs " ++ (w !! 1)
                    liftIO $ runIO (w !! 1)
                    loop
              _ -> do
                liftIO $ runTerminalIO $ "main : T main = " ++ input
                loop

type Run a = ExceptT Error IO a

data Error
  = ParseError String
  | SemanticError S.SemanticError
  | TypeError TC.TypeError
  | ValueError I.ValueError
  | NoSuchFile FilePath

instance Exception Error

instance Show Error where
  show (SemanticError e) =
    "semantic error:\n" ++ show e

  show (ParseError e) =
    "syntax error:\n" ++ e

  show (TypeError (TC.TError f e)) =
    "type error in function " ++ f ++ ":\n" ++ show e

  show (ValueError e) =
    "value error:\n" ++ show e

  show (NoSuchFile f) =
    "file not found: " ++ f

-- | Runs funq on a file.
runIO :: FilePath -> IO ()
runIO path = runExceptT (readfile path >>= run) >>= \case
  Left  err -> putStrLn $ "*** Exception, " ++ show err
  Right val -> print val

runTerminalIO :: String -> IO ()
runTerminalIO s = runExceptT (runTerminal s) >>= \case
  Left  err   -> putStrLn $ "*** Exception, " ++ show err
  Right (v,t) -> putStrLn $ show v ++ " : " ++ show t

runTerminal :: String -> Run (I.Value, A.Type)
runTerminal s = do
  p@[A.Func _ _ term] <- parse s >>= semanticAnalysis >>= convertAST
  typ <- toErr (TC.runCheck . TC.infer) TypeError id term
  val <- eval p
  return (val, typ)

readfile :: FilePath -> Run String
readfile path = do
  e <- liftIO (try (readFile path) :: IO (Either IOError String))
  case e of
    Left  _ -> throwError $ NoSuchFile path
    Right s -> return s

run :: String -> Run I.Value
run s = parse s >>= semanticAnalysis >>= convertAST >>= typecheck >>= eval

-- Components
convertAST :: Program -> Run A.Program
convertAST = return . A.toIm 

typecheck :: A.Program -> Run A.Program
typecheck = toErr TC.typecheck TypeError . const <*> id

eval :: A.Program -> Run I.Value
eval p = (liftIO $ Q.run $ I.interpret p) >>= ExceptT . return . first ValueError

semanticAnalysis :: Program -> Run Program 
semanticAnalysis = toErr S.runAnalysis SemanticError . const <*> id

-- Utils 
toErr :: (i -> Either e v) -> (e -> Error) -> (v -> o) -> i -> Run o
toErr f l r = ExceptT . return . bimap l r . f

parse :: String -> Run Program
parse s = case (pProgram $ myLexer s) of 
  Left err  -> throwError $ ParseError err
  Right b   -> return b

-- | Runs a file some number of times.
rundistest :: FilePath -> Int -> IO ()
rundistest path runs = do
  res <- runExceptT $ rundist path runs
  case res of
    Left err -> putStrLn $ "*** Exception:, " ++ show err
    Right r  -> gatherResults r

rundist :: FilePath -> Int -> Run [I.Value]
rundist path runs = do 
  a <- readfile path >>= parse >>= semanticAnalysis >>= convertAST >>= typecheck
  evaldist a runs

evaldist :: A.Program -> Int -> Run [I.Value]
evaldist prg reps = replicateM reps $ eval prg

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

runAdder :: FilePath -> [Int] -> [Int] -> [String] -> [String] -> Run [I.Value]
runAdder path indsA indsB inputsA inputsB = do
  a <- liftIO $ readFile path -- >>= parse >>= typecheck
  let b = applyInputs (words a) indsA inputsA
  let c = unwords $ applyInputs b indsA inputsA
  q <- parse c >>= semanticAnalysis >>= convertAST >>= typecheck 
  evaldist q 1

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