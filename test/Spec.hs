{-# LANGUAGE FlexibleInstances #-}
module Spec where

import QStateTests ( runTests )
import GatesTests ( runTests )

main :: IO ()
main = do
    QStateTests.runTests
    GatesTests.runTests

    return ()
  

