{-# LANGUAGE        OverloadedLists #-}
{-# LANGUAGE        BlockArguments  #-}
{-# OPTIONS_HADDOCK not-home        #-}

{-|
Module      : QM
Description : The Quantum Monad
Stability   : experimental

Definition of the quantum monad (`QM`) and helper functions
-}
module Lib.QM (
    -- * The Quantum Monad
      QM
    , run
    , runDebug
    , eval
    , put
    , get
    , modify
    , io

    -- * Quantum State representation 
    , QState(..)

    -- * QBit
    , Ix
    , QBit(..)

    -- * Helpers
    , checkState
    , getState
    , stateSize
    ) where

import Numeric.LinearAlgebra
    ( size, toList, C, Vector, magnitude )
import Data.List ( intercalate )
import qualified Control.Monad.Random as Rand ( fromList, evalRandIO )

-- | Internal index type, to indicate it is a qubit index.
type Ix = Int

-- | Pointer to a qubit in QState.
-- Represents the linking function in QLambda
newtype QBit = Ptr { link :: Ix }
    deriving Show

-- | The program state, a complex vector representation of
-- the qubits in the system
newtype QState = QState { state :: Vector C }

instance Show QState where
    show (QState q) = "== QState: " ++ s ++ " ==\n"
                   ++ intercalate "\n" (map show $ toList q) ++ "\n"
        where s = show $ size q

instance Eq QState where
    (==) (QState q1) (QState q2) = and $ zipWith (~=) (toList q1) (toList q2)

-- | Compareas two complex numbers for equality to the 6th decimal
(~=) :: C -> C -> Bool
(~=) a b = bm - eqMargin <= am && am <= bm + eqMargin
  where am = magnitude a
        bm = magnitude b
        eqMargin = 0.000001

-- | The Quantum Monad
-- Keeps a state of the complex vector representation while allowing
-- pseudo-random number generation
newtype QM a = QM { runQM :: QState -> IO (a, QState) }

instance Show (QM a) where
    show _q = "Please use the function 'run' to perform the simulation"

instance Functor QM where
    fmap f m = m >>= return . f

instance Applicative QM where
    pure a = QM \s -> return (a,s)
    {-# INLINE pure #-}
    QM af <*> QM ax = QM \s -> do
        (f, s') <- af s
        (x, s'') <- ax s'
        return (f x, s'')

instance Monad QM where
    return = pure
    m >>= k = QM \s -> do
        (a, s') <- runQM m s
        runQM (k a) s'

-- | Perform IO action inside the quantum monad
io :: IO a -> QM a
io m = QM \s -> do
    a <- m
    return (a,s)
{-# INLINE io #-}

-- | Replace the quantum state
put :: QState -> QM ()
put s = QM \_ -> return ((), s)
{-# INLINE put #-}

-- | Fetch the quantum state
get :: QM QState
get = QM \s -> return (s,s)
{-# INLINE get #-}

-- | Apply a function to the quantum state
modify :: (QState -> QState) -> QM ()
modify f = QM \s -> return ((), f s)
{-# INLINE modify #-}

-- | Run quantum program
eval :: QM a -> IO (a, QState)
eval qm = runQM qm (QState [])
{-# INLINE eval #-}

-- | Run quantum program, discarding it's final state
run :: QM a -> IO a
run qm = fst <$> eval qm
{-# INLINE run #-}

-- | Run quantum program, prints the final quantum state and returns the result
runDebug :: QM a -> IO a
runDebug qm = do
    (a, s) <- eval qm
    print s
    return a

-- | Given a QState, returns how many qbits it consists of.
stateSize :: QState -> Ix
stateSize q = case size (state q) of
                0 -> 0
                x -> log2 x

-- | Normal integer log with base 2.
log2 :: Integral a => a -> Ix
log2 = floor . logBase 2 . fromIntegral

-- | Returns the quantum state together with it's size
getState :: QM (QState, Int)
getState = do
    s <- get
    let size = stateSize s
    return (s, size)

-- | Print the quantum state during operation
checkState :: QM ()
checkState = do
    state <- get
    io $ print state
