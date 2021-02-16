{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        ConstraintKinds                       #-}
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

{-|
Module      : QData
Description : Basic datatypes and typeclasses
Stability   : experimental

This is the core module of the language. This module contains the definitions
of all the types exposed to the user.
-}
module QData 
  ( -- * Core types
    QBit(..)
  , Bit
  , Gate(..)
  , T

  -- * Product type
  , type (><)
  , Prod(..)

  -- * Helpers
  , fromMatrix
  ) where

import qualified Data.Bit as B (Bit (..))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (^))
import Numeric.LinearAlgebra (flatten, ident, kronecker, outer, toList)
import qualified Numeric.LinearAlgebra as LA ((><))
import Numeric.LinearAlgebra.Static as V
    ( C, M, Sized(create, extract), Sq, (#>), mul, app )
import Prelude

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState (d :: Nat) = C d

-- | The product type family. Represents all types @Nat -> *@ that
-- has a product operation, producing the sum of their type indexed size.
type family (p :: b) >< (q :: b) :: b

-- | Class `Prod` defines the product operation on sized types
class Prod (p :: Nat -> *) where
  infixl 7 ><
  (><) :: (KnownNat n, KnownNat m, ((p n >< p m) ~ p (n + m))) =>
    p n ->
    p m ->
    p n >< p m

-- | Vector state representation of qubit state.
-- Dependent on the number of bits @n@ where the vector becomes
--   \[ \otimes_{i=0}^{n-1} \mathbb{C}^2 \].
-- The `QBit` type wraps a statically sized complex vector  
newtype QBit (n :: Nat) = Q { getState :: QState (2 ^ n) }
  deriving Show

type instance (QBit n) >< (QBit m) = QBit (n + m)

-- | The product type for QBits is defined as the tensor product
instance Prod QBit where
  (Q p) >< (Q q) = Q
      let pv = extract p
          pq = extract q
          v = flatten $ outer pv pq
       in case create v of
            Just v' -> v'
            Nothing ->
              errorWithoutStackTrace $
                "Incorrect vectors " ++ show p ++ " and " ++ show q

-- | Type indexed bit strings. Should behave like a list of bits.
data Bit (n :: Nat) where
  (:+) :: B.Bit -> Bit n -> Bit (n + 1)
  NoBit :: Bit 0

deriving instance Show (Bit n)
type instance (Bit n) >< (Bit m) = Bit (n + m)

-- | Ease of use case where a single bit string behaves like bit
--
-- @
-- new 0
-- -- instead of
-- new (0 :+ NoBit)
-- @
instance Num (Bit 1) where
  fromInteger x | x == 0     =  B.Bit False :+ NoBit 
                | x == 1     =  B.Bit True :+ NoBit
                | otherwise  =  errorWithoutStackTrace "Cannot derive bits from non-binary values"
  (a :+ NoBit) * (b :+ NoBit) = (a * b) :+ NoBit
  (a :+ NoBit) + (b :+ NoBit) = (a + b) :+ NoBit
  (a :+ NoBit) - (b :+ NoBit) = (a - b) :+ NoBit
  negate = id
  abs    = id
  signum = id

-- | Ease of use case for pattern matching on single bits
instance Eq (Bit 1) where
  (a :+ NoBit) == (b :+ NoBit) = a == b

-- | Matrix gate representation. 
-- Also wraps a function acting on the `QBit` type
data Gate (n :: Nat) = Gate 
  { matrix :: V.M (2^n) (2^n)
  , run    :: QBit n -> QBit n
  }

type instance (Gate n) >< (Gate m) = Gate (n + m)

-- | The product type for Gates is defined as the kronecker product
-- This combines the action of two gates, running in paralell
--
-- >>> run (hadamard >< id) (new 0 >< new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
-- 
-- >>> run hadamard (new 0) >< run id (new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
instance Prod Gate where
  m >< n = fromMatrix
              let pm = extract $ matrix n
                  qm = extract $ matrix n
              in case create $ kronecker pm qm of
                  Just m  -> m
                  Nothing -> errorWithoutStackTrace
                    $ "Incorrect matrices " ++ show m ++ " and " ++ show n

-- | Converts a unitary matrix to the gate type
fromMatrix :: KnownNat n => M (2^n) (2^n) -> Gate n
fromMatrix mx = Gate
    { matrix = mx
    , run    = \(Q q) -> Q $ app mx q
    }

instance KnownNat n => Show (Gate n) where
  show Gate{matrix} = show matrix

-- | The unit type \(* : \top\)
type T = ()
