module TestRunner where

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}

import Interpreter.Main

import Control.Applicative hiding (empty)
import Control.Monad

import Data.Char
import Data.Function
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Monoid

testPath :: String -> String
testPath testName = "src/Interpreter/test-suite/" ++ testName

goodTests :: [(FilePath, String)]
goodTests = [
--     (testPath "equals1.fq", "1"), 
--     (testPath "id.fq",      "1"),
--    (testPath "let-tupple0.fq",     "0"),
--     (testPath "let-tupple1.fq",     "1"),
--    (testPath "let-tupple-Qbit0.fq","0"),
--    (testPath "let-tupple-Qbit1.fq","1"),
--    (testPath "plus.fq",    "0"),
--    (testPath "cnot2.fq","0"),
--    (testPath "second.fq","1"),
--    (testPath "cnot.fq",    "1"),
--    (testPath "pauliX.fq",  "0"),
    (testPath "secondq.fq", "0")
    ]

runTests :: IO ()
runTests = do 
    mapM_ runTest goodTests

runTest :: (FilePath, String) -> IO ()
runTest (path, expectedValue) = do 
    res <- run path
    if show res == expectedValue then putStrLn $ "Test for " ++ show path ++ " successful!" ++ "\n" 
    else putStrLn $ "Test for " ++ show path ++ " failed! Expected " ++ expectedValue ++ " but got " ++ show res ++ "\n"

