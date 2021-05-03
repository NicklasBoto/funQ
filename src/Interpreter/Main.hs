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
eval p = liftIO (Q.run $ I.interpret p) >>= ExceptT . return . first ValueError

semanticAnalysis :: Program -> Run Program
semanticAnalysis = toErr S.runAnalysis SemanticError . const <*> id

-- Utils 
toErr :: (i -> Either e v) -> (e -> Error) -> (v -> o) -> i -> Run o
toErr f l r = ExceptT . return . bimap l r . f

parse :: String -> Run Program
parse s = case pProgram (myLexer s) of
  Left err  -> throwError $ ParseError err
  Right b   -> return b

-- | Distribution runs of programs
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
  let nbits = lengthV $ head vals
  let res = countUniques $ map readtup vals
  let stats = stat (length vals) res
  mapM_ (putStrLn . prettystats nbits) stats
    where lengthV :: I.Value -> Int
          lengthV (I.VBit b)   = 1
          lengthV (I.VTup _ v) = 1 + lengthV v

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

prettystats :: Int -> (Int, Double, Int) -> String
prettystats len (a,b,c) = concatMap show ((fillzeros len . toBin) a) ++ ": " ++ "\t" ++ (show . truncateboi) b ++ "%" ++ "\t" ++ show c
  where truncateboi d = (fromIntegral . truncate) (10000*(d :: Double))/100
        
toBin :: Int -> [Int]
toBin 0 = []
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
toBin n | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

fillzeros :: Int -> [Int] -> [Int]
fillzeros len as = if length as == len then as else replicate (len - length as) 0 ++ as

-- kör program med index 1 -> sätt tillbaka in1,in2,... -> kör om med index 2

-- | Run program with generated inputs
runNewInputs :: FilePath -> IO ()
runNewInputs path = do
  file <- readFile path
  let ixs = inds file
  res <- runExceptT $ mapM (evalNewInputs ixs file) [0..7]
  case res of
    Left err -> putStrLn $ "*** Exception:, " ++ show err
    Right r  -> gatherResults r

evalNewInputs :: [Int] -> String -> Int ->  Run I.Value
evalNewInputs ixs prg inputNr = do
  let newfile = updateIns 0 ixs (inputsNew 3 !! inputNr) prg in parse newfile >>= semanticAnalysis >>= convertAST >>= typecheck >>= eval

updateIns :: Int -> [Int] -> [String] -> String -> String
updateIns _ [] _ prg = prg
updateIns n (ix:ixs) (input:inputs) prg
  | n == ix = updateIns (n+1) (map (+1) ixs) inputs (replaceIx prgs (ix + 1) input)
  | otherwise = updateIns (n+1) (ix:ixs) (input:inputs) prg
  where prgs = words prg

replaceIx :: [String] -> Int -> String -> String
replaceIx list ix val  = let (a,b) = splitAt ix list in
    let elem = last a in unwords $ take (ix-1) a ++ mend elem val : b
    where mend elem val
            | head elem == '(' = '(' : val ++ ","
            | last elem == ')' = val ++ ")"
            | otherwise = val ++ ","

inds :: String -> [Int]
inds l = findIndices predN (words l)
  where predN :: String -> Bool
        predN s = let clean = filter (\x -> x `elem` ['a'..'z'] || x `elem` ['1'..'9']) s in
                  clean `elem` ins

file :: [String]
file =  ["main",":","(Bit","><","Bit","><","Bit)","main","=","let","(a,b,c)","=","TOFFOLI","(in1,","in2,","in3)","in","(meas","a,","meas","b,","meas","c)"]

inputsNew :: Int -> [[String]]
inputsNew len = let bins = map (fillzeros len . toBin) (concat (replicate 4 [6,7])) in [map ((++) "new " . show) a | a <- bins]

ins :: [String]
ins = ["in" ++ show a | a <- [1..9]]

-- testinds :: IO ()
-- testinds = do
--   res <- readFile "test/interpreter-test-suite/toffoli.fq"
--   print $ updateIns 0 (inds res) inputsNew res
