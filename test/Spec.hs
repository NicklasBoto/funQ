{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck;
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, pick, pre, run, PropertyM)
import Internal.Core (newVector, tensorVector)
import Core ( new )
import QM ( io, get, put, run, getState, QState(..), QM, QBit(..) )
import Gates
import Numeric.LinearAlgebra ((><),toList,(#>), C, Vector, realPart, imagPart, Additive (add), Normed(norm_2))
import Control.Monad.Random ( fromList, evalRandIO, getRandomR, liftIO, Rand, liftM)

main :: IO ()
main = do
    print "testing reversibility of gates on single qubits:"
    a <-test_rev_gates
    print a 
    print "testing reversibility of cnot with two qubits"
    c <-test_rev_cnot
    print c
    print "quickCheck tests testing sum of QState = 1"
    quickCheck prop_sumOne
    quickCheck prop_sum_hadamard 
    quickCheck prop_sum_cnot 
    quickCheck prop_sum_pauliX
    quickCheck prop_sum_pauliY
    quickCheck prop_sum_pauliZ
    return ()

--- Non-quickCheck tests

-- | Applies a given gate twice to a given qubit. Returns state before and after the operations
applyTwice :: QBit -> (QBit -> QM QBit) -> QM (QState,QState)
applyTwice qbt g = do
           before <- get
           once <- applyGate before g qbt
           twice <- applyGate once g qbt
           return (before,twice)


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

--- QuickCheck tests
-- >>> quickCheck prop_sumOne

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

-- | Checks that the sum of the squared elements in the vector sums to 1 
prop_gate_sum :: (QBit -> QM QBit) -> QState -> Property
prop_gate_sum g q = TM.monadicIO $ do
    run' $ addState q
    (_,size) <- run' getState
    qbt <- run' $ getRandQbit size
    s <- run' $ applyGate q g qbt
    let su = norm_2 $ state s
    TM.assert (su < 1.00001 && su > 0.9999) --due to rounding errors, cannot test == 1


--- Helpers

-- helper function to run QM computations within the property monad for quickcheck
run' = TM.run . run


-- | Adds given QState
addState :: QState -> QM QState
addState q = do
    put q
    get

--  | Helper function, apply the given gate on a random qubit in state, return the state
applyGate :: QState -> (QBit -> QM QBit) -> QBit -> QM QState
applyGate qs g qbt = do
    addState qs
    g qbt
    get

-- cnot to be used with applyGate for testing purpose
cnot' :: (QBit -> QM QBit)
cnot' q = do
    let p = if link q /= 0 then Ptr $ link q + 1 else Ptr $ link q - 1
    (q',p') <- cnot (q, p)
    return p' -- returns pointer to the first qubit only, dummy implementation for matching of types 

-- Returns index between zero and size of QState
--getRandQbit :: Int -> QM QBit
getRandQbit :: Int -> QM QBit
getRandQbit size = do
        i <- io $ evalRandIO $ getRandomR (0,size)
        return $ Ptr i


--- Arbitrary instances

-- Arbitrary instance for QState, not very pretty :) 
instance Arbitrary QState where
    arbitrary = do
        b <- elements [0,1]
        n <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        m <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        let s = elements [tensorVector m n]
        let t = elements [tensorVector (tensorVector m n) (tensorVector m n)]
        e <- frequency [(7,s),(3,t)]
        return $ QState e

-- Not used for now
instance Arbitrary (QM QBit) where
    arbitrary = elements [new 0, new 1]

-- Not used for now
instance Arbitrary (QM QState) where
        arbitrary = do
            put <$> (arbitrary :: Gen QState)
            return get

-- Not used for now
instance Arbitrary (QM ()) where
  arbitrary = put <$> (arbitrary :: Gen QState)