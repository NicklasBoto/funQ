{-# LANGUAGE OverloadedLists #-}

{-|
Module      : Internal.Core
Description : Core language internals
Stability   : experimental

Internal matrix and measurment operations
-}
module Internal.Core where

import Data.Bit ( Bit )
import QM ( QState(QState), Ix, QBit (Ptr), stateSize )
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
import qualified Control.Monad.Random as Rand ( fromList, evalRandIO )

-- | Appends state with tensor 
appendState :: QState -> QState -> QState
appendState (QState new) (QState [])    = QState new
appendState (QState new) (QState state) = QState $ tensorVector new state


-- | Tensor product between two vectors
tensorVector :: Vector C -> Vector C -> Vector C
tensorVector newVector oldVector = flatten $ outer oldVector newVector

-- | Vector state representations of qubits with 100% probaility
-- to collapse to their bit counterparts
newVector :: Bit -> QState
newVector 0 = QState [1, 0]
newVector 1 = QState [0, 1]


-- Model probability as a rational number.
type Prob = Rational

-- | Finds the probability of the qubit measing to a 1.
--  Find all the amplitudes where that qubit is one and converts it to probabilities.
findQbitProb1 :: QBit -> QState -> Prob
findQbitProb1 qbit qstate = sum $ map ampToProb (findMarginAmps1 qbit qstate)

type Amplitude = Complex Double

-- | Finds the amplitudes from all the positions where that qubit is one.
findMarginAmps1 :: QBit -> QState -> [Amplitude]
findMarginAmps1 qbit qstate = map snd $ filter isMargin allAmps
  where
    allAmps :: [(Ix, Amplitude)]
    allAmps = qstateAmps qstate

    isMargin :: (Ix, Amplitude) -> Bool
    isMargin (ix, _) = maskMatch ix ixMask -- This implies that the amplitude deals with a case where the given qbit measures to a 1
    ixMask = qbitMask qbit qstate

-- | If all the bits in b are in a it is a match.
maskMatch :: Int -> Int -> Bool
maskMatch a b = a .&. b == b

-- | Given a qbit, finds its mask in the qstate.
-- E.g. an amplitude in a 3 qbits state could be |100>.
-- A mask of 100=8 is wanted if the zero'th qbit is in interest, same mask would work for |101>.
-- If qbit is 1 we want it to be 010=4...
qbitMask :: QBit -> QState -> Int
qbitMask (Ptr qbitIx) qstate = 2^(numQbits - 1 - qbitIx)
  where
    numQbits = stateSize qstate

-- | Given a complex amplitude, will return its probability.
ampToProb :: Amplitude -> Prob
ampToProb = toRational . (^2) . magnitude

-- | Removes the states that contradict the measurment from the qbit to bit, also normalizes the state to a length of one.
remImpossibleStates :: QState -> QBit -> Bit -> QState
remImpossibleStates qstate qbit bit = (QState . normalize . fromList . map transformAmp) amps
  where
    amps = qstateAmps qstate
    ixMask = qbitMask qbit qstate

    -- If the index is at a impossible state, then amplitude is set to 0, else it is kept.
    transformAmp :: (Ix, Amplitude) -> Amplitude
    transformAmp (ix, amp) | impossibleState ix = 0
                           | otherwise          = amp

    -- If the mask is a match we are at a position where that qubit is a 1, if the bit is measured
    -- as a 0 we are at impossible state. Or the opposite, if the position is where qbit is 0
    -- we are impossible if we measured a one.
    impossibleState :: Ix -> Bool
    impossibleState ix | maskMatch ix ixMask = bit == 0 
                       | otherwise           = bit == 1


-- | From a qstate, returns the amplitudes with its indexes. 
qstateAmps :: QState -> [(Ix, Amplitude)]
qstateAmps (QState stateVector) = zip [0..] (toList stateVector)

-- Uses random number generator to return a bit according to the probabilites given.
rngQbit :: Prob -> IO Bit
rngQbit p1 = Rand.evalRandIO $ Rand.fromList [(0, 1-p1), (1, p1)]