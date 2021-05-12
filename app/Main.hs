{-# LANGUAGE OverloadedStrings #-}

module Main where

import FunQ
import Paths_qfunc ( version )
import Turtle
import Data.Version ( showVersion )
import Data.Functor
import qualified Interpreter.Run as Run
import qualified Repl

desc :: Description
desc = "funQ - Functional Quantum Programming"

-- | Parses an optional input file.
parseInput :: Parser (Maybe (String, Maybe Int, Bool))
parseInput = optional $ (,,) <$> (encodeString <$> argPath "src" "The file to run")
                            <*> optional (optInt "runs" 'r' "Runs the program n times")
                            <*> switch "interactive" 'i' "Load file in interactive environment"

-- | Option to show the funq version.
parseVersion :: Parser (IO ())
parseVersion = switch "version" 'v' "Shows the interpreter version" $> ver
    where ver = putStrLn $ showVersion version

-- | If a input file is provided execute it,
--   else start the repl.
parseMain :: Parser (IO ())
parseMain = withSrc <$> parseInput
    where withSrc Nothing                        = Repl.main
          withSrc (Just (path, _        , True)) = Repl.mainFile path
          withSrc (Just (path, Just runs, _))    = Run.rundistest path runs
          withSrc (Just (path, Nothing  , _))    = Run.runIO path

parser :: Parser (IO ())
parser =  parseMain <|> parseVersion
      
main :: IO ()
main = join (Turtle.options desc parser)
