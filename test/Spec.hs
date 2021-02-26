module Spec where

import Test.QuickCheck;
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, pick, pre, run)
import Funqy (new, newVector, tensorVector)
import QM 
import Gates
import Numeric.LinearAlgebra ((><),toList,(#>), C, Vector, realPart)

{-#LANGUAGE -FlexibleInstances#-}


main :: IO ()
main = do
    return ()

-- TODO:
-- eq qubits
-- arbitrary qubit
-- prop_paulixGate :: QBit n -> Bool
-- prop_paulixGate q = undefined -- q == pauliX pauliX q

-- prop_qubitEq1 :: Bool
-- prop_qubitEq1 = new 0 /= new 1
-- 
-- prop_qubitEq2 :: Bool
-- prop_qubitEq2 = new 1 /= new 0
-- 
-- prop_qubitEq3 :: Bool
-- prop_qubitEq3 = new 1 == new 1
-- 
-- prop_qubitEq4 :: Bool
-- prop_qubitEq4 = new 0 == new 0


hadamardTest :: QM (Vector C)
hadamardTest =  do
    q <- new 0 
    p <- new 1 
    hadamard q 
    (QState v) <- get 
    return v

cnotTest :: QM (Vector C)
cnotTest =  do
    q <- new 0 
    p <- new 1 
    cnot q p  
    (QState v) <- get 
    return v

-- Can be tested by > Quickcheck prop_gate_sum cnotTest/hadamardTest
prop_gate_sum :: QM (Vector C) -> Property 
prop_gate_sum gate = TM.monadicIO $ do
    v <- TM.run $ run gate
    let s = sum $ map (\x -> x^2) (toList $ v)
    TM.assert (realPart s < 1.0001 && realPart s > 0.9999) --due to rounding errors, cannot test == 1

    
-- Arbitrary instance for QState, would probably not be used
instance Arbitrary QState where
    arbitrary = do 
        (QState q) <- elements [newVector 0, newVector 1] -- ska vara hur många som helst
        (QState p) <- elements [newVector 0, newVector 1]
        return $ QState $ tensorVector q p 

prop_sumOne :: QState -> Bool 
prop_sumOne (QState v) = sum (toList v) == 1