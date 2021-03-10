{-# LANGUAGE FlexibleInstances #-}
module ReversibilityTests (
    runTests
) where

import Gates
import Core ( new )
import QM ( QState(state), QM, QBit, get, run )
import Numeric.LinearAlgebra ( toList, realPart )
import Control.Monad.Random ( liftM )
import Helpers ( applyTwice )

runTests :: IO ()
runTests = do 
    a <- test_rev_gates
    putStrLn $ "Testing reversibility of gates on single qubits: " ++ show a
    
    b <- test_rev_cnot
    putStrLn $ "Testing reversibility of cnot with two qubits: " ++ show b

    return ()

-- Test reversibility of gates 
-- | Given a gate that takes a single qbit, applies it and checks reversibility 
test_rev :: (QBit -> QM QBit) -> IO Bool
test_rev g = run $ do
    qbt <- new 0
    (b,a) <- applyTwice qbt g
    let bf = map realPart (toList $ state b)
    let af = map realPart (toList $ state a)
    let cmp =  zipWith (\ x y -> abs (x - y)) bf af
    return $ all (<0.0000001) cmp -- cannot be checked directly due to rounding errors

-- All other gates than hadamard could be tested with (state a == state b)
    
-- | Applies the reversibility tests to all gates that matches type signature of QBit -> QM QBit.
test_rev_gates :: IO Bool
test_rev_gates = liftM and $ mapM test_rev gates
    where gates = [hadamard, pauliX, pauliY, pauliZ, phase, phasePi8, identity]

-- | Test reversibility of cnot 
test_rev_cnot :: IO Bool 
test_rev_cnot = run $ do
    q1 <- new 1
    q2 <- new 0
    b <- get 
    cnot (q1,q2) >>= cnot 
    a <- get 
    return (state a == state b) 
