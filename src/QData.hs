{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        ConstraintKinds                       #-}
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
    ( R, Sized(create, extract), Sq )
import Prelude
--import Control.Monad.State (StateT, MonadState(put), gets, MonadIO(liftIO), modify, evalStateT)
--import Control.Monad.Except ( runExceptT, MonadError(catchError, throwError), ExceptT )





-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState (d :: Nat) = R d

-- | Program state
data ProgramSt = ProgramSt {
      q  :: QState 1,
      l  :: T,
      m  :: T        -- can we have a common type for all terms?
}

--type QuantMon = StateT ProgramSt (ExceptT IO) 


-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Nat) = Q {getState :: QState (2 ^ n)}
  deriving (Show)

-- | Representation of a classical bit 
data Bit (n :: Nat) where 
  (:::) :: B.Bit -> Bit n -> Bit (n + 1)
  NoBit :: Bit 0 
deriving instance Show (Bit n)

-- Ensures that only 0 and 1 bits can be created
instance Num (Bit 1) where
  fromInteger x | x == 0     =  B.Bit False ::: NoBit 
                | x == 1     =  B.Bit True ::: NoBit
                | otherwise  =  errorWithoutStackTrace "Cannot derive bits from non-binary values" 

-- | Experimental product type family
type family (p :: b) >< (q :: b) :: b

type instance (QBit n) >< (QBit m) = QBit (n + m)

type instance (Bit n) >< (Bit m) = Bit (n + m)

class Prod (p :: Nat -> *) where
  infixl 7 ><
  (><) ::
    (KnownNat n, KnownNat m, ((p n >< p m) ~ p (n + m))) =>
    p n ->
    p m ->
    p n >< p m


instance Prod QBit where
  (Q p) >< (Q q) =
    Q
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

-- data Gate (n :: Nat) = Gate { gmatrix :: Sq (2^n), runGate :: QBit n -> QBit n }

-- fromMatrix :: Sq (2^n) -> Gate n
-- fromMatrix mx = Gate
--     { gmatrix = mx
--     , runGate = \(Q q) -> Q $ mx #> q
--     }

-- | Matrix gate representation
type Gate (n :: Nat) = Sq (2 ^ n)