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

    -- * Simulators
    , run
    , runDebug

    -- * Utils
    , bell
    , bellMeasure

    ) where

import Control.Monad ( replicateM, mapM )
import Lib.QM ( QM, QBit, run, runDebug )
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
      qft )

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
    return (m_x,m_y)

