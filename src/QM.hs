
{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        ConstraintKinds                       #-}
{-# LANGUAGE        RecordWildCards                       #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        NamedFieldPuns                        #-}
{-# LANGUAGE        TypeOperators                         #-}
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        DerivingVia                           #-}
{-# LANGUAGE        Rank2Types                            #-}
{-# LANGUAGE        PolyKinds                             #-}
{-# LANGUAGE        DataKinds                             #-}
{-# LANGUAGE        GADTs                                 #-}
{-# OPTIONS_GHC     -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK not-home                              #-}

module QM where

import Numeric.LinearAlgebra.Static
import Control.Monad.State 
import Control.Monad.Except
import GHC.TypeLits
import Data.Bit
import Prelude
import QData
import QOps 

data Qptr
--data QState = QS (QBit 1, Qstate)
type QState (d :: Nat) = C d 
data Error = Error
    deriving Show

-- data Tup a = T a a
--            | R a (Tup a)
--            deriving Show


data Tup a = T (QBit 1) (QBit 1)
             | Rec (QBit 1) (Tup (QBit 1))
        deriving (Show, Eq)
          

-- [[1,0] >< [1,0]] hadamard p0
-- [[0.7,0.7] >< [1,0]]
-- [0.7,0.0,0.7,0.0] cnot <p0,p1>
-- [0.7,0.0,0.0,0.7]
-- measure p0


-- | The program state.
data ProgramSt r = P
  { q :: QState 1
  , l :: Nat -> Qptr
  , m :: r
  }

data QM 

--b00 = cnot $ hadamard (new 0) >< new 0

--f = do
--    q <- link 0 --hur avgör vi storleken på q?
--    -- let? 
--    hadamard q --> (I<2*2> >< H<2*2>) 
--
---- type Q = QM s (StateT () (ExceptT Error IO))
-- 
--
--new :: Bit 1 -> QM ()
--new b = modify P{q = Q b}