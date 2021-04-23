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
desc = "Wilkommen aus die quanteninterpretator"

-- | Parses an optional input file.
parseInput :: Parser (Maybe (String, Maybe Int))
parseInput = optional $ (,) <$> (encodeString <$> argPath "src" "The file to run")
                            <*> optional (optInt "runs" 'r' "Runs the program n times")

-- | Option to show the funq version.
parseVersion :: Parser (IO ())
parseVersion = switch "version" 'v' "Shows the interpreter version" $> ver
    where ver = putStrLn $ showVersion version

-- | If a input file is provided execute it,
--   else start the repl.
parseMain :: Parser (IO ())
parseMain = withSrc <$> parseInput
    where withSrc Nothing      = Repl.main
          withSrc (Just (path, Nothing))   = Run.runIO path
          withSrc (Just (path, Just runs)) = Run.rundistest path runs

parser :: Parser (IO ())
parser =  parseMain
      <|> parseVersion
      
main :: IO ()
main = join (Turtle.options desc parser)
