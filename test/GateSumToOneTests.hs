{-# LANGUAGE FlexibleInstances #-}
module GateSumToOneTests (
    runTests
) where

import Test.QuickCheck
import Gates
import QM ( QM, QBit, getState )
import Arbitraries ( QState(..) ) 
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO)
import Helpers ( run', addState, applyGate, cnot', getRandQbit )
import Numeric.LinearAlgebra ( Normed(norm_2) )

runTests :: IO ()
runTests = do 
    putStrLn "QuickCheck test unmodified QState sums to 1"
    quickCheck prop_sumOne

    --putStrLn "QuickCheck tests sum of QState = 1 after applying gates"
    mapM_ quickCheck [prop_sum_hadamard, prop_sum_cnot, prop_sum_pauliX, prop_sum_pauliY, prop_sum_pauliZ]

    return ()

-- | Checks that the sum of the squared elements in the vector sums to 1 
prop_gate_sum :: (QBit -> QM QBit) -> QState -> Property
prop_gate_sum g q = TM.monadicIO $ do
    run' $ addState q
    (_,size) <- run' getState
    qbt <- run' $ getRandQbit size
    s <- run' $ applyGate q g qbt
    let su = norm_2 $ state s
    TM.assert (su < 1.00001 && su > 0.9999) --due to rounding errors, cannot test == 1

-- Basic quickCheck test, that unmodified QState sums to 1
prop_sumOne :: QState -> Bool
prop_sumOne (QState v) = norm_2 v == 1

-- | Can be run with QuickCheck to test 
prop_sum_hadamard :: QState -> Property
prop_sum_hadamard = prop_gate_sum hadamard

prop_sum_cnot :: QState -> Property
prop_sum_cnot = prop_gate_sum cnot'

prop_sum_pauliX :: QState -> Property
prop_sum_pauliX = prop_gate_sum pauliX

prop_sum_pauliZ :: QState -> Property
prop_sum_pauliZ = prop_gate_sum pauliZ

-- Currently failing
prop_sum_pauliY :: QState -> Property
prop_sum_pauliY = prop_gate_sum pauliY
