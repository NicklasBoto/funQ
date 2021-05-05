{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

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
err (Left  e) = errorWithoutStackTrace $ "*** Exception, " ++ show e
err (Right v) = return v

buildLin :: LinEnv -> ErrorEnv
buildLin s = ErrorEnv s "<interactive>"

printr :: MonadIO io => String -> io ()
printr = liftIO . putStrLn

getfun :: String -> Repl (Maybe (Term, Type))
getfun name = gets $ (\env -> lookup name [(n,(e,t)) | Func n t e <- env]) . (Set.toList . funs)

ops :: [(String, String -> Repl ())]
ops = [ ("help"   , helpCmd )
      , ("?"      , helpCmd )
      , ("quit"   , quitCmd )
      , ("run"    , runCmd  )
      , ("version", verCmd  )
      , ("type"   , typeCmd )
      , ("env"    , envCmd  )
      , ("load"   , loadCmd )
      , ("clear"  , clearCmd)
      , ("delete" , delCmd  )
      , ("ml"     , return . return ())
      ]

helpTexts :: Map.Map String String
helpTexts = Map.fromList
    [ ("quit", "\nQuit the session")
    , ("run", "FILEPATH [RUNS]\nRun a file")
    , ("?", "[COMMAND]\nShow help")
    , ("help", "[COMMAND]\nShow help")
    , ("ml", "\nEnter multiline mode")
    , ("version", "\nShow version")
    , ("type", "[EXPRESSION]\nShow the type of an expression. No arguments shows the types in the environment.")
    , ("env", "\nShow environment")
    , ("load", "\nLoad a file into the environment")
    , ("clear", "\nClear the environment")
    , ("delete", "\nDelete a function from the environment")
    ]

helpCmd :: String ->  Repl ()
helpCmd [] = printr $ "Available commands:\n"
           ++ intercalate "\n" [fst x | x <- ops]
           ++ "\n\nFor info on a specific command type :help COMMAND (or :? COMMAND)"
helpCmd arg = case Map.lookup arg helpTexts of
    Nothing   -> printr $ "no such command: " ++ arg
    Just help -> printr $ "Usage: " ++ arg ++ " " ++ help

quitCmd, runCmd, verCmd, typeCmd, envCmd, loadCmd, clearCmd, delCmd :: String -> Repl ()
quitCmd _ = abort
runCmd paths = case words paths of
    [path] -> liftIO . Run.runIO $ path
    _      -> printr "invalid arguments"
verCmd  _ = printr $ showVersion version
typeCmd "" = mapM_ (printr . showType) =<< gets funs
    where showType (Func n t _) = n ++ " : " ++ show t
typeCmd n = gets (Set.toList . funs)
        >>= \env -> either (printr . show) (printr . show)
          $ runCheckWith (infer (parseExp n)) (buildTopEnv env) (buildLin Set.empty)
envCmd _ = mapM_ (printr . show) =<< gets funs
loadCmd path = mapM_ (dontCrash . load) (words path)
    where fnames = Set.map (\(Func n _ _) -> n)
          load s = liftIO (Run.checkProgram s)
               >>= modify
               . (\prog (RS env lin) ->
                   RS (Set.union prog env) (Set.difference lin (fnames prog)))
               . Set.delete (emptyFun "main") . Set.fromList
clearCmd  _ = put emptyRS
delCmd name = modify \(RS env lin) -> RS (Set.delete (emptyFun name) env) (Set.delete name lin)

parseAssign :: Stream s m Char => ParsecT s u m [Char]
parseAssign = manyTill alphaNum (skipMany space >> string "=")

evalCmd :: String -> Repl ()
evalCmd line = case parse parseAssign "" line of
    Left _     -> do
        env <- gets (Set.toList . funs)
        lin <- gets linenv
        let term = Run.parseExp line
        typ <- err $ runCheckWith (infer term) (buildTopEnv env) (buildLin lin)
        (mval, mtyp) <- err =<< liftIO (runExceptT (Run.runProgram $ Func "main" typ term : env))
        addLinears term
        printr $ show mval ++ " : " ++ show mtyp
    Right name -> do
        let termString = tail $ dropWhile (/='=') line
            term = Run.parseExp termString
        env <- gets funs
        lin <- gets linenv
        case runCheckWith (infer term) (buildTopEnv (Set.toList env)) (buildLin lin) of
            Left  err -> errorWithoutStackTrace $ "*** Exception, " ++ show err
            Right typ -> modify (\s -> s{funs=Set.insert (Func name typ term) env})
                      >> when (Set.member (Func name typ term) env)
                              (modify \s -> s{linenv=Set.delete name lin})

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
          ++ ["new", "meas", "measure"]

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
mainFile path = liftIO (Run.checkProgram path)
            >>= (mainWith . flip RS Set.empty)
              . Set.delete (emptyFun "main")
              . Set.fromList

main :: IO ()
main = mainWith emptyRS