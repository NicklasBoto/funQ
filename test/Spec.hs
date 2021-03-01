{-# LANGUAGE FlexibleInstances #-}
module Spec where

import Test.QuickCheck;
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, pick, pre, run, PropertyM)
import Internal.Core (newVector, tensorVector)
import Core
import QM
import Gates
import Numeric.LinearAlgebra ((><),toList,(#>), C, Vector, realPart, imagPart)
import Control.Monad.Random ( fromList, evalRandIO, getRandomR, liftIO, Rand)


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
hadamardTest = do
    q <- new 0
    p <- new 0
    hadamard q
    (QState v) <- get
    return v

--cnotTest :: QM (Vector C)
--cnotTest =  do
--    cnot (q, p)  
--    (QState v) <- get 
--    return v



-- | Checks that the sum of the squared elements in the vector sums to 1 
-- >>>  quickcheck $ prop_gate_sum gate h 
prop_gate_sum :: (QState -> QM (Vector C)) -> QState -> Property
prop_gate_sum f q = TM.monadicIO $ do
    v <- TM.run $ run $ f q
    let s = sum $ map (^2) (toList v)
    TM.assert (realPart s + imagPart s < 1.00001 && realPart s + imagPart s > 0.9999) --due to rounding errors, cannot test == 1

--  Takes an arbitrary QState, apply Hadamard gate on random qubit in state
h :: QState -> QM (Vector C)
h q = do
    put q
    (_,s) <- getState
    q' <- io $ evalRandIO $ getRandQbit s
    hadamard (Ptr q')
    (QState s) <- get
    return s

-- Takes an arbitrary QState, apply cnot gate on random qubit in state
c :: QState ->  QM (Vector C)
c q = do
    put q
    (_,s) <- getState
    q' <- io $ evalRandIO $ getRandQbit s
    let p = if q' /= 0 then q' + 1 else q' - 1
    cnot (Ptr q', Ptr p)
    (QState s) <- get
    return s

-- Returns index between zero and size of QState
getRandQbit size = getRandomR (0,size)

getV :: QState -> Vector C 
getV (QState q) = q 

-- Arbitrary instance for QState, would probably not be used
instance Arbitrary QState where
    arbitrary = do
        b <- elements [0,1]
        n <- elements [getV $ newVector b, tensorVector (getV $ newVector b) (getV $ newVector b)]
        m <- elements [getV $ newVector b, tensorVector (getV $ newVector b) (getV $ newVector b)]
        t <- elements [tensorVector m n] 
        return $ QState t

-- Basic quickCheck test, that unmodified QState sums to 1
prop_sumOne :: QState -> Bool
prop_sumOne (QState v) = sum (toList v) == 1

-- Not used for now
instance Arbitrary (QM QState) where
        arbitrary = do
        put <$> (arbitrary :: Gen QState)
        return get

-- Not used for now
instance Arbitrary (QM ()) where
  arbitrary = put <$> (arbitrary :: Gen QState)