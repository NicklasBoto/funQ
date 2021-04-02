{-# LANGUAGE FlexibleInstances #-}
module Main where

import QStateTests ( runTests )
import GatesTests ( runTests )

main :: IO ()
main = do
    QStateTests.runTests
    GatesTests.runTests
    return ()
  

