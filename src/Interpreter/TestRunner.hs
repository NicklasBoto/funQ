module Interpreter.TestRunner where

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}

import Interpreter.Main 
import Control.Monad.Except (runExceptT)

testPath :: String -> String
testPath testName = "src/Interpreter/test-suite/" ++ testName

tests :: [(FilePath, String)]
tests = [
    -- ("cnot.fq",     "0"),
    -- ("equals.fq",   "1"),
    -- ("id.fq",       "1"),
    -- ("let-tup-q.fq","0"),
    -- ("let-tup.fq",  "0"),
    -- ("pauliX.fq",   "1"), 
    -- ("pauliY.fq",   "1"),     
    -- ("pauliZ.fq",   "0"),   
    -- ("phase.fq",    "0"),  
    -- ("plus.fq",     "0"),
    -- ("second-q.fq", "0"),
    -- ("second.fq",   "1"),
    -- ("seventh.fq",  "0"),
    -- ("swap.fq",     "1"),
    -- ("swapTwice.fq","1"),
    -- ("third.fq",    "1"),
    ("teleport.fq", "1")
    -- ("tdagger.fq",  "1")
    -- ("qft1.fq",     "0"),
    -- ("qft2.fq",     "0"),
    -- TODO ("toffoli.fq")
 -- TODO ("fredkin.fq",  "0"), 
    ]

runTests :: IO ()
runTests = do 
    -- runExceptT tar en exceptT-transformerad monad och kör den, tar ur den inre monaden ur exceptT
    -- och returnar Either left right
    res <- runExceptT (foldr runTest (pure 0) tests)
    case res of 
        Left err -> print err
        Right correct -> do
            putStrLn $ "\nSuccesful tests " ++ show correct ++ "/" ++ show (length tests) ++ "\n"

runTest :: (String, String) -> Run Int -> Run Int
runTest (fileName, expectedValue) b = do 
    res <- run (testPath fileName)
    acc <- b
    if show res == expectedValue then do 
        return $ acc + 1
    else do 
        return acc