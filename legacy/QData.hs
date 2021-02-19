{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
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
  , Bit(..)
  , Gate(..)
  , T

  -- * Product type
  , type (><)
  , Prod(..)

  -- * Helpers
  , bits
  , fromMatrix
  , i
  ) where

import qualified Data.Bit as B (Bit (..))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (^), type(-))
import Numeric.LinearAlgebra (flatten, ident, kronecker, outer, toList)
import qualified Numeric.LinearAlgebra as LA ((><))
import Numeric.LinearAlgebra.Static as V
  ( C, M, Sized(create, extract), Sq, (#>), mul, app )
import Prelude
import GHC.Exts as E
import Data.Bits
import Foreign.Storable
import qualified Data.Complex

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

instance KnownNat n => Eq (QBit n) where
  (Q q) == (Q p) = extract q == extract p

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
  Sing :: B.Bit -> Bit 1

infixr 6 :+
type instance (Bit n) >< (Bit m) = Bit (n + m)

instance Show (Bit n) where
  show (Sing x) = show x
  show (x :+ xs) = show x ++ show xs

-- | Ease of use case where a single bit string behaves like bit
--
-- @
-- new 0
-- -- instead of
-- new (0 :+ NoBit)
-- @
instance Num (Bit 1) where
  fromInteger x | x == 0     =  Sing $ B.Bit False
                | x == 1     =  Sing $ B.Bit True
                | otherwise  =  errorWithoutStackTrace "Cannot derive bits from non-binary values"
  (Sing a) * (Sing b) = Sing (a * b)
  (Sing a) + (Sing b) = Sing (a + b)
  (Sing a) - (Sing b) = Sing (a - b)
  negate = id
  abs    = id
  signum = id

-- | Ease of use case for pattern matching on single bits
instance Eq (Bit 1) where
  (Sing a) == (Sing b) = a == b

-- cc :: (KnownNat n, KnownNat m) => Bit n -> Bit m -> Bit (n + m)
-- cc (Sing a) (Sing b) = a :+ Sing b
-- cc (a :+ (as :: Bit (n - 1))) bs = a :+ (cc as bs)

bits :: (Storable a, Bits a) => a -> [B.Bit]
bits x = map (B.Bit . testBit x) [0..8*sizeOf x-1]

-- | Matrix gate representation. 
-- Also wraps a function acting on the `QBit` type
data Gate (n :: Nat) = Gate 
  { matrix :: V.M (2^n) (2^n)
  , run    :: QBit n -> QBit n
  }

type instance (Gate n) >< (Gate m) = Gate (n + m)

instance KnownNat n => Semigroup (Gate n) where
  Gate{matrix=a} <> Gate{matrix=b} = fromMatrix $ mul a b

-- | The product type for Gates is defined as the kronecker product
-- This combines the action of two gates, running in paralell
--
-- >>> run (hadamard >< id) (new 0 >< new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
-- 
-- >>> run hadamard (new 0) >< run id (new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
instance Prod Gate where
  p >< q = fromMatrix
              let pm = extract $ matrix p
                  qm = extract $ matrix q
              in case create $ pm `kronecker` qm of
                  Just m  -> m
                  Nothing -> errorWithoutStackTrace
                    $ "Incorrect matrices " ++ show p ++ " and " ++ show q

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

-- | The imaginary unit
i :: Data.Complex.Complex Double
i = 0 Data.Complex.:+ 1