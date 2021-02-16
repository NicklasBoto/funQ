{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        ConstraintKinds                       #-}
{-# LANGUAGE        RecordWildCards                       #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        TypeOperators                         #-}
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        DerivingVia                           #-}
{-# LANGUAGE        Rank2Types                            #-}
{-# LANGUAGE        PolyKinds                             #-}
{-# LANGUAGE        DataKinds                             #-}
{-# LANGUAGE        GADTs                                 #-}
{-# OPTIONS_GHC     -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK not-home                              #-}

-- |
-- Module      : QData
-- Description : qfunc datatypes
-- Stability   : experimental
--
-- utförlig beskrivning
module QData where

import qualified Data.Bit as B (Bit (..))
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (^))
import Numeric.LinearAlgebra (flatten, ident, kronecker, outer, toList)
import qualified Numeric.LinearAlgebra as LA ((><))
import Numeric.LinearAlgebra.Static as V
    ( R, Sized(create, extract), Sq, (#>) )
import Prelude

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState (d :: Nat) = R d

-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Nat) = Q { getState :: QState (2 ^ n) }
  deriving (Show)

data Bit (n :: Nat) where
  (:::) :: B.Bit -> Bit n -> Bit (n + 1)
  NoBit :: Bit 0

deriving instance Show (Bit n)

instance Num (Bit 1) where
  fromInteger x | x == 0     =  B.Bit False ::: NoBit 
                | x == 1     =  B.Bit True ::: NoBit
                | otherwise  =  errorWithoutStackTrace "Cannot derive bits from non-binary values"
  (a ::: NoBit) * (b ::: NoBit) = (a * b) ::: NoBit
  (a ::: NoBit) + (b ::: NoBit) = (a + b) ::: NoBit
  (a ::: NoBit) - (b ::: NoBit) = (a - b) ::: NoBit
  negate = id
  abs    = id
  signum = id

-- | Experimental product type family
type family (p :: b) >< (q :: b) :: b

type instance (QBit n) >< (QBit m) = QBit (n + m)
type instance (Bit  n) >< (Bit  m) = Bit  (n + m)
type instance (Gate n) >< (Gate m) = Gate (n + m)

class Prod (p :: Nat -> *) where
  infixl 7 ><
  (><) :: (KnownNat n, KnownNat m, ((p n >< p m) ~ p (n + m))) =>
    p n ->
    p m ->
    p n >< p m

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

-- | The unit type \(* : \top\)
type T = ()

data Gate (n :: Nat) = Gate 
  { matrix :: Sq (2^n)
  , run    :: QBit n -> QBit n
  }

fromMatrix :: KnownNat n => Sq (2^n) -> Gate n
fromMatrix mx = Gate
    { matrix = mx
    , run    = \(Q q) -> Q $ mx #> q
    }

instance KnownNat n => Show (Gate n) where
  show Gate{..} = show matrix

-- | Combine the action of two gates
--
-- >>> gate (combine matrixH matrixI) (new 0 >< new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
-- 
-- >>> hadamard (new 0) >< id (new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
combine :: (KnownNat n, KnownNat m) => Gate n -> Gate m -> Gate (n + m)
combine p q = fromMatrix
              let pm = extract $ matrix p
                  qm = extract $ matrix q
              in case create $ pm `kronecker` qm of
                  Just m  -> m
                  Nothing -> errorWithoutStackTrace
                    $ "Incorrect matrices " ++ show p ++ " and " ++ show q

instance Prod Gate where
  (><) = combine

-- | Matrix gate representation
-- type Gate (n :: Nat) = Sq (2 ^ n)
