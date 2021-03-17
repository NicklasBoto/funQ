{-# LANGUAGE FlexibleInstances #-}
module Spec where

import FunQ ( new, Bit, QBit, hadamard )
import Test.QuickCheck
import Test.QuickCheck.Monadic as TM
import QM
import Numeric.LinearAlgebra as LA
import Internal.Gates (i)

import ReversibilityTests ( runTests )
import GateSumToOneTests ( runTests )
import QStateTests ( runTests )
import GatesTests ( runTests )

main :: IO ()
main = do
    QStateTests.runTests
    GatesTests.runTests

    return ()
  

