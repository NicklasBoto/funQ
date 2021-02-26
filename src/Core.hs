{-# LANGUAGE        OverloadedLists #-}
{-# LANGUAGE        BlockArguments  #-}
{-# OPTIONS_HADDOCK not-home        #-}

{-|
Module      : Funqy
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
import Internal.Core ( findProb, remBadProbs, appendState, newVector )
import Data.Bit ( Bit )
import Control.Monad ( replicateM )
import qualified Control.Monad.Random as Rand ( fromList, evalRandIO )


-- | Create new `QBit` from a bit.
-- maps \(0 \mapsto |0>\) and \(1 \mapsto |1>\)
new :: Bit -> QM QBit
new x = do
    (_,size) <- getState
    modify $ appendState (newVector x)
    return $ Ptr size


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
