{-# LANGUAGE FlexibleInstances #-}
module Spec where

import Test.QuickCheck;
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, pick, pre, run, PropertyM)
import Internal.Core (newVector, tensorVector)
import Core
import QM
import Gates
import Numeric.LinearAlgebra ((><),toList,(#>), C, Vector, realPart, imagPart, Additive (add))
import Control.Monad.Random ( fromList, evalRandIO, getRandomR, liftIO, Rand)


main :: IO ()
main = do
    return ()

--- Non-quickCheck tests

-- | Checks that a given gate applied twice on a single quBit is same as before (Still not exact due to rounding error)
applyTwice :: QBit -> (QBit -> QM QBit) -> QM (QState,QState)
applyTwice qbt g = do
           (before,s) <- getState 
           once <- applyGate before g qbt
           twice <- applyGate once g qbt
           return (before,twice)
           

-- Test reversibility of gates 
test_rev_had :: IO Bool
test_rev_had = run $ do  
    qbt <- new 0 
    (b,a) <- applyTwice qbt hadamard
    let bf = map realPart (toList $ state b)
    let af = map realPart (toList $ state a)
    let cmp =  zipWith (\ x y -> abs (x - y)) bf af
    return $ all (<0.0000001) cmp -- cannot be checked directly due to rounding errors

test_rev_paulix :: IO Bool
test_rev_paulix = run $ do 
    qbt <- new 0 
    (b,a) <- applyTwice qbt pauliX
    let bf = map realPart (toList $ state b)
    let af = map realPart (toList $ state a)
    let cmp =  zipWith (\ x y -> abs (x - y)) bf af
    return (bf == af)
     

--- QuickCheck tests
-- >>> quickCheck prop_sumOne

-- Basic quickCheck test, that unmodified QState sums to 1
prop_sumOne :: QState -> Bool
prop_sumOne (QState v) = sum (toList v) == 1

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
    let su = sum $ map (^2) (toList $ state s)
    TM.assert (realPart su + imagPart su < 1.00001 && realPart su + imagPart su > 0.9999) --due to rounding errors, cannot test == 1


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

-- Arbitrary instance for QState, would probably not be used
instance Arbitrary QState where
    arbitrary = do
        b <- elements [0,1]
        n <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        m <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        t <- elements [tensorVector m n]
        return $ QState t

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