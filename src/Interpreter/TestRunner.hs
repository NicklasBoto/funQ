module Interpreter.TestRunner where

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

tests :: [(FilePath, String)]
tests = [
    ("cnot.fq",     "0"),
    ("equals.fq",   "1"),
    -- TODO ("fredkin.fq",  "0"), 
    ("id.fq",       "1"),
    ("let-tup-q.fq","0"),
    ("let-tup.fq",  "0"),
    ("pauliX.fq",   "1"), 
    ("pauliY.fq",   "1"),     
    ("pauliZ.fq",   "0"),   
    ("phase.fq",    "0"),  
    ("plus.fq",     "0"),
    -- ("qft1.fq",     "0"),
    -- ("qft2.fq",     "0"),
    ("second-q.fq", "0"),
    ("second.fq",   "1"),
    ("seventh.fq",  "0"),
    ("swap.fq",     "1"),
    ("swapTwice.fq","1"),
    -- ("tdagger.fq",  "1"),
    ("teleport.fq", "1"),
    ("third.fq",    "1")
    -- TODO ("toffoli.fq")
    ]

runTests :: IO ()
runTests = do 
    correct <- foldr runTest (return 0) tests
    putStrLn $ "\nSuccesful tests " ++ show correct ++ "/" ++ show (length tests) ++ "\n"

runTest :: (String, String) -> IO Int -> IO Int
runTest (fileName, expectedValue) b = do 
    putStrLn $ "file: " ++ fileName
    res <- run $ testPath fileName
    acc <- b
    if show res == expectedValue then do 
        putStrLn $ "Test for " ++ fileName ++ " successful!" 
        putStrLn $ "Got result " ++ show res ++ ", expected " ++ expectedValue ++ "\n"
        return $ acc + 1
    else do 
        putStrLn $ "Test for " ++ fileName ++ " failed! Expected " ++ expectedValue ++ " but got " ++ show res
        return acc