{-# LANGUAGE FlexibleInstances #-}
module Main ( main ) where

import ReversibilityTests ( runTests )
import GateSumToOneTests ( runTests )

main :: IO ()
main = do
    ReversibilityTests.runTests
    GateSumToOneTests.runTests
    return ()
