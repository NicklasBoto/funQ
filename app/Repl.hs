{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import qualified Interpreter.Run as Run
import Interpreter.Interpreter hiding ( eval )
import Type.TypeChecker hiding ( throwError, linenv )
import System.Console.Haskeline
import System.Console.Repline
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import FunQ
import Data.List
import Data.Maybe
import Data.Char
import Data.Functor
import Paths_qfunc
import Data.Version
import AST.AST
import Text.Parsec
import qualified Data.Map as Map
import qualified Data.Set as Set

type Repl = HaskelineT (StateT ReplState (ExceptT Run.Error IO))

data Assign = Assign String Type Value
    deriving Show   

data ReplState = RS 
    { funs   :: Program
    , linenv :: LinEnv
    } deriving Show

emptyRS :: ReplState 
emptyRS = RS [] Set.empty

buildLin s = ErrorEnv s "<interactive>"

liftRun = lift . lift 


printr :: MonadIO io => String -> io ()
printr = liftIO . putStrLn

getfun :: String -> Repl (Maybe (Term, Type))
getfun name = do
    env <- gets funs
    return $ lookup name [(n,(e,t)) | Func n t e <- env]

ops :: [(String, String -> Repl ())]
ops = [ ("help"   , helpCmd  )
      , ("?"      , helpCmd  )
      , ("quit"   , quitCmd  )
      , ("run"    , runCmd   )
      , ("version", verCmd   )
      , ("type"   , typeCmd  )
      ]

helpTexts :: Map.Map String String
helpTexts = Map.fromList 
    [ ("quit", "\nQuit the session")
    , ("run", "FILEPATH [RUNS]\nRun a file")
    , ("?", "[COMMAND]\nShow help")
    , ("help", "[COMMAND]\nShow help")
    , ("ml", "\nEnter multiline mode")
    , ("version", "\nShow version")
    ]

helpCmd :: String ->  Repl ()
helpCmd [] = printr $ "Available commands:\n" 
           ++ intercalate "\n" [fst x | x <- ops] 
           ++ "\n\nFor info on a specific command type :help COMMAND (or :? COMMAND)"
helpCmd arg = case Map.lookup arg helpTexts of
    Nothing   -> printr $ "no such command: " ++ show arg
    Just help -> printr $ "Usage: " ++ arg ++ " " ++ help
help _ = printr $ "invalid arguments"

quitCmd, runCmd, verCmd, typeCmd :: String -> Repl ()
quitCmd _ = abort
runCmd    = liftIO . Run.runIO
verCmd  _ = printr $ showVersion version
typeCmd n = getfun n >>= \case
    Nothing    -> printr $ "no such function " ++ n
    Just (_,t) -> printr $ show t

parseAssign :: Stream s m Char => ParsecT s u m [Char]
parseAssign = manyTill alphaNum (skipMany space >> string "=")


evalCmd :: String -> Repl ()
evalCmd line = case parse parseAssign "" line of
    Left _     -> do
        env <- gets funs  
        lin <- gets linenv
        let term = parseExp line
        typ <- case runCheckWith (infer term) (buildTopEnv env) (buildLin lin) of
            Left  err -> liftRun $ throwError $ Run.TypeError err
            Right typ -> return typ 
        (mval, mtyp) <- liftRun $ Run.runProgram $ Func "main" typ term : env
        addLinears term
        printr $ show mval ++ " : " ++ show mtyp
    Right name -> do 
        let termString = tail $ dropWhile (/='=') line
            term = parseExp termString
        env <- gets funs 
        lin <- gets linenv
        case runCheckWith (infer term) (buildTopEnv env) (buildLin lin) of
            Left  err -> liftRun $ throwError $ Run.TypeError err
            Right typ -> do
                env <- gets funs
                modify $ \s -> s{funs=Func name typ term : env}

addLinears :: Term -> Repl ()        
addLinears term = do
    env <- gets funs
    ts  <- map snd . catMaybes <$> mapM getfun (names term) 
    let ns = map fst $ filter (isLinear . snd) $ zip (names term) ts
    lin <- gets linenv
    modify $ \s -> s{linenv = Set.union (Set.fromList ns) lin}

compl :: Monad m => WordCompleter m
compl n = return $ filter (isPrefixOf n) $ map ((':':) . fst) ops

toIO ex = runExceptT ex >>= \case
  Left  err   -> putStrLn $ "*** Exception, " ++ show err
  Right val   -> print val

main :: IO ()
main = toIO $ flip evalStateT emptyRS $ evalReplOpts $ ReplOpts
    { banner           = const (pure "λ ")
    , command          = dontCrash . evalCmd
    , options          = ops
    , prefix           = Just ':'
    , multilineCommand = Just "ml"
    , tabComplete      = Combine (Word compl) File
    , initialiser      = printr $ "funQ " ++ showVersion version ++ "\n:? for help"
    , finaliser        = printr "Leaving funQ." $> Exit
    }