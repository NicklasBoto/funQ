{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
import Control.Monad.Catch
import Data.List
import Data.Maybe
import Data.Char
import Data.Functor
import Paths_qfunc
import Data.Version
import AST.AST
import Text.Parsec
    ( alphaNum,
      space,
      string,
      manyTill,
      parse,
      skipMany,
      ParsecT,
      Stream )
import qualified Data.Map as Map
import qualified Data.Set as Set

-- type Repl = HaskelineT (StateT ReplState (ExceptT Run.Error IO))
type Repl = HaskelineT (StateT ReplState IO)

data Assign = Assign String Type Value
    deriving Show

data ReplState = RS
    { funs   :: Set.Set Function
    , linenv :: LinEnv
    } deriving Show

emptyRS :: ReplState
emptyRS = RS Set.empty Set.empty

emptyFun :: String -> Function
emptyFun n = Func n TypeUnit Unit

err :: Show e => Either e v -> Repl v
err (Left  e) = errorWithoutStackTrace $ show e
err (Right v) = return v

buildLin :: LinEnv -> ErrorEnv
buildLin s = ErrorEnv s "<interactive>"

printr :: MonadIO io => String -> io ()
printr = liftIO . putStrLn

getfun :: String -> Repl (Maybe (Term, Type))
getfun name = do
    env <- gets (Set.toList . funs)
    return $ lookup name [(n,(e,t)) | Func n t e <- env]

ops :: [(String, String -> Repl ())]
ops = [ ("help"   , helpCmd  )
      , ("?"      , helpCmd  )
      , ("quit"   , quitCmd  )
      , ("run"    , runCmd   )
      , ("version", verCmd   )
      , ("type"   , typeCmd  )
      , ("env"    , envCmd   )
      , ("load"   , loadCmd  )
      , ("clear"  , clearCmd )
      , ("delete" , delCmd   )
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

quitCmd, runCmd, verCmd, typeCmd, envCmd, loadCmd, clearCmd, delCmd :: String -> Repl ()
quitCmd _ = abort
runCmd    = liftIO . Run.runIO . init
verCmd  _ = printr $ showVersion version
typeCmd "" = mapM_ (printr . showType) =<< gets funs
    where showType (Func n t _) = n ++ " : " ++ show t
typeCmd n = do
    env <- gets (Set.toList . funs)
    lin <- gets linenv
    case runCheckWith (infer (parseExp n)) (buildTopEnv env) (buildLin lin) of
        Left  e -> printr $ show e
        Right t -> printr $ show t
envCmd _ = mapM_ (printr . show) . Set.toList =<< gets funs
loadCmd path = do
    let paths = words path
    let load s = do prog <- Set.delete (emptyFun "main") . Set.fromList <$> liftIO (runFile s)
                    env <- gets funs
                    lin <- gets linenv
                    modify $ \s -> s{linenv = Set.difference lin (Set.map (\(Func n _ _) -> n) prog)}
                    modify $ \s -> s{funs= Set.union prog env} -- snygga upp dessa när vi har tid 
    mapM_ (dontCrash . load) paths
clearCmd _  = put emptyRS
delCmd name = do
    lin <- gets linenv
    env <- gets funs
    put $ RS (Set.delete (emptyFun name) env) (Set.delete name lin)

parseAssign :: Stream s m Char => ParsecT s u m [Char]
parseAssign = manyTill alphaNum (skipMany space >> string "=")

evalCmd :: String -> Repl ()
evalCmd line = case parse parseAssign "" line of
    Left _     -> do
        env <- gets (Set.toList . funs)
        lin <- gets linenv
        let term = parseExp line
        typ <- err $ runCheckWith (infer term) (buildTopEnv env) (buildLin lin)
        (mval, mtyp) <- err =<< liftIO (runExceptT (Run.runProgram $ Func "main" typ term : env))
        addLinears term
        printr $ show mval ++ " : " ++ show mtyp
    Right name -> do
        let termString = tail $ dropWhile (/='=') line
            term = parseExp termString
        env <- gets funs
        lin <- gets linenv
        case runCheckWith (infer term) (buildTopEnv (Set.toList env)) (buildLin lin) of
            Left  err -> errorWithoutStackTrace $ show err
            Right typ -> modify (\s -> s{funs=Set.insert (Func name typ term) env})
                      >> when (Set.member (Func name typ term) env)
                              (modify $ \s -> s{linenv=Set.delete name lin})

addLinears :: Term -> Repl ()
addLinears term = do
    env <- gets funs
    ts  <- map snd . catMaybes <$> mapM getfun (names term)
    let ns = map fst $ filter (isLinear . snd) $ zip (names term) ts
    lin <- gets linenv
    modify $ \s -> s{linenv = Set.union (Set.fromList ns) lin}

compl :: WordCompleter (StateT ReplState IO)
compl n = do
    env <- gets (Set.toList . funs)
    return $ filter (isPrefixOf n)
           $ map ((':':) . fst) ops
          ++ [n | Func n _ _ <- env]

mainWith :: ReplState -> IO ()
mainWith rs = flip evalStateT rs $ evalReplOpts $ ReplOpts
    { banner           = const (pure "λ ")
    , command          = dontCrash . evalCmd
    , options          = ops
    , prefix           = Just ':'
    , multilineCommand = Just "ml"
    , tabComplete      = Combine (Word compl) File
    , initialiser      = printr $ "funQ " ++ showVersion version ++ "\n:? for help"
    , finaliser        = printr "Leaving funQ." $> Exit
    }

mainFile :: String -> IO ()
mainFile path = liftIO (runFile path) >>= (mainWith . flip RS Set.empty) . Set.fromList

main :: IO ()
main = mainWith emptyRS