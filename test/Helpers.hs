{-# LANGUAGE FlexibleInstances #-}
module Helpers (
    applyTwice,
    run',
    addState,
    applyGate,
    cnot',
    getRandQbit
) where

import Test.QuickCheck ();
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, run)
import Internal.Core (newVector, tensorVector)
import Core ( new )
import QM ( io, get, put, run, getState, QState(..), QM, QBit(..) )
import Gates
import Numeric.LinearAlgebra ( toList, realPart, Normed(norm_2) )
import Control.Monad.Random ( evalRandIO, getRandomR, liftM )

-- | Applies a given gate twice to a given qubit. Returns state before and after the operations
applyTwice :: QBit -> (QBit -> QM QBit) -> QM (QState,QState)
applyTwice qbt g = do
    before <- get
    once <- applyGate before g qbt
    twice <- applyGate once g qbt
    return (before,twice)


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
