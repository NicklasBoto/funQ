{-# LANGUAGE FlexibleInstances #-}
module Main where
import QStateTests      ( runTests )
import GatesTests       ( runTests )
import InterpreterTests ( runTests )

main :: IO ()
main = do
    QStateTests.runTests
    GatesTests.runTests
    InterpreterTests.runTests
    return ()
  

