{-# LANGUAGE        OverloadedLists #-}
{-# LANGUAGE        BlockArguments  #-}
{-# OPTIONS_HADDOCK not-home        #-}

{-|
Module      : Funqy
Description : Core language
Stability   : experimental

The core language operations.
-}
module Funqy (
    -- * QBit manipulation
      new
    , measure

    -- * Control functions
    , controlbit
    , (===)
    
    -- * Distribution functions
    , dist
    , ndist

    -- * The Quantum Monad
    , module QM

    -- * Bit
    , Data.Bit.Bit
) where

import QM
import Data.Bit ( Bit )
import Data.Bits ( Bits((.&.)) )
import Control.Monad ( replicateM, void ) 
import qualified Control.Monad.Random as Rand ( fromList, evalRandIO )
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

-- | Create new `QBit` from a bit.
-- maps \(0 \mapsto |0>\) and \(1 \mapsto |1>\)
new :: Bit -> QM QBit
new x = do
    (_,size) <- getState
    modify $ appendState (newVector x)
    return $ Ptr size

-- | Vector state representations of qubits with 100% probaility
-- to collapse to their bit counterparts
newVector :: Bit -> QState
newVector 0 = QState [1, 0]
newVector 1 = QState [0, 1]

-- | Performs a measurement operation, collapsing a `QBit` to a `Bit`.
-- Here comes a long explanation of the algorithm correcting the state
measure :: QBit -> QM Bit
measure (Ptr ix) = do
    state <- get
    let (p0, p1) = findProb ix state
    let dist = Rand.fromList [(0,p0), (1,p1)]
    res <- io $ Rand.evalRandIO dist
    let newState = remBadProbs res ix state
    put newState
    return res

-- | (probToBeZero, probToBeOne)
-- ix is index of qubit to measure
findProb :: Ix -> QState -> (Rational, Rational)
findProb ixMeas qs@(QState s) = (1 - prob, prob) 
    where match (x,y) -- Matches indices with bitmask, if index matches then return the squared magnitude of the corresponding probability, else return 0
            | x .&. mask == mask = (toRational . (^2) . magnitude) y
            | otherwise = 0 -- This isn't so nice, we're creating useless data
            where mask = 2^(stateSize qs - ixMeas - 1) :: Int -- Create a mask to recognize correct indicies 
          prob = sum $ zipWith (curry match) [0..] (toList s) -- Applies match to the list of indices and probabilities, then sums the probabilities

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

-- | Sets a classical bit as the controlbit for a quantum gate.
-- Making it run only when the classical bit is equal to one.
controlbit :: QM a -> Bit -> QM ()
controlbit m 1 = m >> return ()
controlbit m 0 = return ()

-- | Synonym for controlbit
(===) :: QM a -> Bit -> QM ()
(===) = controlbit

-- | Run a quantum program producing a single bit @reps@ times
-- and print the results
ndist :: Int -> QM Bit -> IO ()
ndist reps meas = do
    putStrLn $ "Runs : " ++ show reps
    ms <- replicateM reps (run meas)
    let is = map fromIntegral ms
    let ones = sum is :: Double
    let zeros = fromIntegral reps - ones
    let pones = 100 * (ones / fromIntegral reps)
    let pzeros = 100 - pones
    putStrLn $ "|0>  : " ++ show zeros ++ " (" ++ show pzeros ++ " %)"
    putStrLn $ "|1>  : " ++ show ones ++ " (" ++ show pones ++ " %)"

-- | Print results from a 100 runs of a program
dist :: QM Bit -> IO ()
dist = ndist 100
