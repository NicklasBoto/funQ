{-# LANGUAGE OverloadedLists #-}

{-|
Module      : Internal.Core
Description : Core language internals
Stability   : experimental

Internal matrix and measurment operations
-}
module Internal.Core where

import Data.Bit ( Bit )
import QM ( QState(QState), Ix, stateSize )
import Data.Bits ( Bits((.&.)) )
import Numeric.LinearAlgebra
    ( Complex,
      magnitude,
      flatten,
      outer,
      normalize,
      size,
      toList,
      fromList,
      C,
      Vector )

-- | Appends state with tensor 
appendState :: QState -> QState -> QState
appendState (QState new) (QState [])    = QState new
appendState (QState new) (QState state) = QState $ tensorVector new state


-- | Tensor product between two vectors
tensorVector :: Vector C -> Vector C -> Vector C
tensorVector newVector oldVector = flatten $ outer oldVector newVector

-- | (probToBeZero, probToBeOne)
-- ix is index of qubit to measure
findProb :: Ix -> QState -> (Rational, Rational)
findProb ixMeas qs@(QState s) = (1 - prob, prob) 
    where match x y -- Matches indices with bitmask, if index matches then return the squared magnitude of the corresponding probability, else return 0
            | x .&. mask == mask = (toRational . (^2) . magnitude) y
            | otherwise = 0 -- This isn't so nice, we're creating useless data
            where mask = 2^(stateSize qs - ixMeas - 1) :: Int -- Create a mask to recognize correct indicies 
          prob = sum $ zipWith match [0..] (toList s) -- Applies match to the list of indices and probabilities, then sums the probabilities

--eliminateImpossibleProbabilities 
remBadProbs :: Bit -> Ix -> QState -> QState
remBadProbs bit ix qs@(QState s) = QState $ normalize newVector
    where
        size = stateSize qs

        newVector :: Vector C
        newVector = fromList $ zipWith (curry f) [0..] (toList s)

        f :: (Ix, Complex Double) -> Complex Double
        f (index, component)
          | maskMatch ix index size = if bit == 0 then 0 else component
          | bit == 1 = 0
          | otherwise = component

maskMatch :: Ix -> Ix -> Int -> Bool
maskMatch ixMeas ixComp size = ixComp .&. mask == mask
    where mask = 2^(size - ixMeas - 1)

-- | Vector state representations of qubits with 100% probaility
-- to collapse to their bit counterparts
newVector :: Bit -> QState
newVector 0 = QState [1, 0]
newVector 1 = QState [0, 1]
