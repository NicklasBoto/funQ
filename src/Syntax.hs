{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        OverloadedLists                       #-}
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

{-|
Module      : Syntax
Description : QBit definition
Stability   : experimental

Contains all of the language as of now, but will be split in the future
-}
module Syntax where -- strictly export safe functions

import Numeric.LinearAlgebra.Static as V hiding ( outer )
import Numeric.LinearAlgebra ( flatten, outer, kronecker, ident, toList )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits ( Nat, type (+), type (^),  KnownNat, natVal )
import GHC.Exts ( IsList(..), Item )
import qualified Data.Bit as B ( Bit(..) )
import Data.Proxy ( Proxy(..) )
import Prelude
import Control.Monad.Random as Rand

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState (d :: Nat) = R d

-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes 
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Nat) = Q { getState :: QState (2^n) }
    deriving Show

data Bit (n :: Nat) where
    (:::) :: B.Bit -> Bit n -> Bit (n + 1)
    NoBit :: Bit 0

deriving instance Show (Bit n)

instance Num (Bit 1) where
    fromInteger x = B.Bit (odd x) ::: NoBit

-- | Experimental product type family
type family (p :: b) >< (q :: b) :: b
type instance (QBit n) >< (QBit m) = QBit (n + m)
type instance (Bit n) >< (Bit m) = Bit (n + m)

class Prod (p :: Nat -> *) where
    infixl 7 ><
    (><) :: (KnownNat n, KnownNat m, ((p n >< p m) ~ p (n + m))) 
         => p n -> p m -> p n >< p m

instance Prod QBit where
    -- | We define the tensor product of qbits as the flattened
    --   outer product of the state vectors. Note the summed qubit size.
    (Q p) >< (Q q) = Q
        let pv = extract p
            pq = extract q
            v  = flatten $ outer pv pq
        in case create v of
            Just v' -> v'
            Nothing -> errorWithoutStackTrace
                $ "Incorrect vectors " ++ show p ++ " and " ++ show q

-- | Highly experimental inner type for some stateful monad
data ProgramState =
        ProgramState { vector :: *
                     , lambda :: *
                     , linker :: *
                     }

-- | The unit type \(* : \top\)
type T = ()

-- data Gate (n :: Nat) = Gate { gmatrix :: Sq (2^n), runGate :: QBit n -> QBit n }

-- fromMatrix :: Sq (2^n) -> Gate n
-- fromMatrix mx = Gate 
--     { gmatrix = mx
--     , runGate = \(Q q) -> Q $ mx #> q
--     }

-- | Matrix gate representation
type Gate (n :: Nat) = Sq (2^n)

-- | Constructs new qubits
new :: Bit 1 -> QBit 1
new (0 ::: NoBit) = Q $ V.vector [ 1
                                 , 0 ]

new (1 ::: NoBit) = Q $ V.vector [ 0
                                 , 1 ]

-- | Collapses a qubit state (of size 1) to a single bit
measure :: QBit n -> Bit n
measure = undefined

-- | Combine the action of two gates
--
-- >>> gate (combine matrixH matrixI) (new 0 >< new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
-- 
-- >>> hadamard (new 0) >< id (new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
combine :: (KnownNat n, KnownNat m) => Gate n -> Gate m -> Gate (n + m)
combine p q = let pm = extract p
                  qm = extract q
              in case create $ pm `kronecker` qm of
                  Just  m -> m
                  Nothing -> errorWithoutStackTrace
                    $ "Incorrect matrices " ++ show p ++ " and " ++ show q

-- | Create quantum gate from its matrix representation
gate :: KnownNat n => Gate n -> (QBit n -> QBit n)
gate mx (Q q) = Q $ mx #> q

isUnitary :: forall n . KnownNat n => Gate n -> Bool
isUnitary m = mcm == mi && mmc == mi
    where mcm = extract $ tr m V.<> m
          mmc = extract $ m V.<> tr m
          mi  = extract (matrixI :: Gate n)

matrixN :: Gate 1
matrixN = matrix [ 0 , 1 
                 , 1 , 0 ]

-- | Hadamard matrix
matrixH :: Gate 1
matrixH = sqrt 0.5 * matrix [ 1 ,  1
                            , 1 , -1 ]

-- | Hadamard gate acting on single qubits
hadamard :: QBit 1 -> QBit 1
hadamard = gate matrixH

-- | CNOT matrix
matrixC :: Gate 2
matrixC = matrix [ 1, 0, 0, 0
                 , 0, 1, 0, 0
                 , 0, 0, 0, 1
                 , 0, 0, 1, 0 ]

-- | CNOT gate acting on two qubits
cnot :: QBit 2 -> QBit 2
cnot = gate matrixC


measureN :: QBit 1 -> IO Bit 
measureN = evalRandIO 
        . Rand.fromList 
        . zip [0,1] 
        . map (toRational . (^2)) 
        . toList 
        . extract 
        . getState

measureLA :: QBit 1 -> IO Bit
measureLA (Q q) = (evalRandIO 
               . Rand.fromList 
               . zip [0,1] 
               . map toRational) 
               [prob  (V.vector [1,0]), prob (V.vector [0,1])]
    where projOp b = V.mul (V.col b) (V.row b)
          prob b = ((^2) . V.norm_0) $ V.mul (projOp b) (V.col q)

-- | The identity matrix
matrixI :: forall (n :: Nat) . KnownNat n => Gate n
matrixI = let dim = natVal (Proxy :: Proxy n)
          in case create $ ident $ fromInteger $ 2^dim of
              Just  i -> i
              Nothing -> errorWithoutStackTrace
                "Could not deduce matrix dimensions"
