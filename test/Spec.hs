{-# LANGUAGE FlexibleInstances #-}

module Main where
import QStateTests      ( runTests )
import GatesTests       ( runTests )
import InterpreterTests ( runTests )
import TypeCheckTests   ( runTests )
import System.Exit ( exitFailure, exitSuccess )

main :: IO ()
main = sequence tests >>= \b -> if and b then exitSuccess else exitFailure
    where tests = [ QStateTests.runTests
                  , GatesTests.runTests
                  , InterpreterTests.runTests
                  , TypeCheckTests.runTests
                  ]
