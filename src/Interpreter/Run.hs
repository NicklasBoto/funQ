{-# language LambdaCase #-}

module Interpreter.Run where

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
import Control.Exception (Exception, try)
import qualified Type.TypeChecker as TC
import Data.List
import Data.Maybe
import Control.Monad.State.Lazy

import Parser.Abs
import qualified SemanticAnalysis.SemanticAnalysis as S

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

  show (TypeError (TC.TError where' why)) =
    "type error in function " ++ where' ++ ":\n" ++ show why

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

parse :: String -> Run Program
parse = toErr (pProgram . myLexer) ParseError id

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

runProgram :: A.Program -> Run (I.Value, A.Type)
runProgram p = do
  typecheck p
  val <- eval p
  typ <- case lookup "main" [(n,t) | A.Func n t _ <- p] of
    Nothing -> throwError $ SemanticError S.NoMainFunction 
    Just  t -> return t
  return (val, typ)

parseExp :: [Char] -> A.Term
parseExp e = either semanticerror (const (fetchTerm (A.toIm prog))) (S.runAnalysis prog)
  where prog = either syntaxerror id $ pProgram (myLexer ("main : T main = " ++ e)) 
        fetchTerm [A.Func _ _ t] = t
        syntaxerror   e = errorWithoutStackTrace $ "*** Exception:\n" ++ e
        semanticerror e = errorWithoutStackTrace $ "*** Exception, semantic error:\n" ++ show e

checkProgram :: FilePath -> IO A.Program
checkProgram path = runExceptT (readfile path >>= parse >>= semanticAnalysis >>= convertAST >>= typecheck) >>= \case
  Left  e -> errorWithoutStackTrace $ "*** Exception, " ++ show e
  Right p -> return p

-- Utils 
toErr :: (i -> Either e v) -> (e -> Error) -> (v -> o) -> i -> Run o
toErr f l r = ExceptT . return . bimap l r . f

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
toBin n | even n         = toBin (n `div` 2) ++ [0]

fillzeros :: Int -> [Int] -> [Int]
fillzeros len as = if length as == len then as else replicate (len - length as) 0 ++ as

-- | Run program with auto-generated inputs
runNewInputs :: FilePath -> IO ()
runNewInputs path = do
  file <- readFile path
  let ixs = inds file
  res <- runExceptT $ mapM (evalNewInputs ixs file) [0..7]
  case res of
    Left err -> putStrLn $ "*** Exception:, " ++ show err
    Right r  -> gatherGenResults [0..7] r

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
        ins = ["in" ++ show a | a <- [1..9]]

inputsNew :: Int -> [[String]]
inputsNew len = let bins = map (fillzeros len . toBin) [0..2^len-1] in [map ((++) "new " . show) a | a <- bins]

prettyPrintGens :: Int -> (Int,Int) -> String
prettyPrintGens len (ein, aus) = show einB ++ "\t" ++ show ausB
  where einB = fillzeros len . toBin $ ein
        ausB = fillzeros len . toBin $ aus

gatherGenResults :: [Int] -> [I.Value] -> IO ()
gatherGenResults ins outs = do
  let nbits = lengthV $ head outs
  let res = map readtup outs
  let terms = zip ins res
  putStrLn $ "  in  " ++ "\t" ++ "result"
  mapM_ (putStrLn . prettyPrintGens nbits) terms
    where lengthV :: I.Value -> Int
          lengthV (I.VBit b)   = 1
          lengthV (I.VTup _ v) = 1 + lengthV v