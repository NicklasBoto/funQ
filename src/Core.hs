{-# LANGUAGE        OverloadedLists #-}
{-# LANGUAGE        BlockArguments  #-}
{-# OPTIONS_HADDOCK not-home        #-}

{-|
Module      : Core
Description : Core language
Stability   : experimental

The core language operations.
-}
module Core (
    -- * QBit manipulation
      new
    , measure

    -- * Control functions
    , controlbit
    , (===)
    
    -- * Distribution functions
    , dist
    , ndist

    -- * Bit
    , Data.Bit.Bit
) where

import QM ( QM, QBit(Ptr), io, put, get, modify, run, getState )
import Internal.Core ( findQbitProb1, remImpossibleStates, Prob, appendState, newVector, rngQbit )
import Data.Bit ( Bit )
import Control.Monad ( replicateM )

-- | Create new `QBit` from a bit.
-- maps \(0 \mapsto |0>\) and \(1 \mapsto |1>\)
new :: Bit -> QM QBit
new x = do
    (_,size) <- getState
    modify $ appendState (newVector x)
    return $ Ptr size

-- | Performs a measurement operation, collapsing a `QBit` to a `Bit`.
-- The qubit will still exist in the quantum state, but be collapsed.
--
-- Finds qubit probability to collapse to a zero and one.
-- Uses random number generator to "measure it" to a zero or one.
-- Updates quantum state to remove impossible states, and normalizes it so probabilites sum to one.
measure :: QBit -> QM Bit
measure qbit = do
    state <- get
    let p1 = findQbitProb1 qbit state
    bit <- io $ rngQbit p1 -- Need to use io for randomness
    let newState = remImpossibleStates state qbit bit
    put newState
    return bit

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
