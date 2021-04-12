{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : FunQ
Description : Main library
Stability   : experimental

Exports the language and simulator
-}
module FunQ 
    ( -- * Core operations
      new
    , measure
    , ndist
    , dist
    , controlbit

    -- * Core types
    , QBit
    , Bit
    , QM
    , io
    
    -- * Gates
    , pauliX
    , pauliY
    , pauliZ
    , hadamard
    , phase
    , phasePi8
    , cnot
    , identity
    , swap
    , tdagger
    , fredkin
    , toffoli
    , urot
    , crot
    , qft
    , qftDagger
    , controlT

    -- * Simulators
    , run
    , runDebug

    -- * Utils
    , bell
    , bellMeasure

    ) where

import Control.Monad ( replicateM, mapM )
import Lib.QM ( QM, QBit, run, runDebug, io, checkState)
import Lib.Core
    ( Bit,
      new,
      measure,
      controlbit,
      ndist,
      dist )
import Lib.Gates
    ( cnot,
      hadamard,
      identity,
      pauliX,
      pauliY,
      pauliZ,
      phase,
      phasePi8,
      swap,
      tdagger,
      fredkin,
      toffoli,
      urot,
      crot,
      qft, controlT, qftDagger )

-- | Prepares bell state
bell :: (Bit, Bit) -> QM (QBit, QBit)
bell (a,b) = do
    qa <- new a
    qb <- new b
    hadamard qa
    cnot (qa, qb)

-- | Performs bell measurement
bellMeasure :: (QBit, QBit) -> QM (Bit, Bit)
bellMeasure (x,y) = do
    cnot (x,y)
    hadamard x
    m_x <- measure x
    m_y <- measure y
    return (m_x, m_y)

data Gate a = H a | CNOT a a | TOFF a a a

type family Tuple t :: * where
  Tuple [a,a] = (a,a)
  Tuple [a,a,a] = (a,a,a)
  Tuple [a,a,a,a] = (a,a,a,a)

-- class Runnable a where
--   run :: a -> QM a

-- tuple :: Tuple a -> QM (QBit, QBit)
-- tuple = undefined

-- tuple :: [QBit] -> Tuple t
-- tuple lst = undefined

-- test :: QM ()
-- test = do
--   q <- new 1
--   q' <- new 0
--   cnot $ tuple [q, q']
--   checkState