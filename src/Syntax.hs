{-# LANGUAGE        UndecidableInstances                  #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        OverloadedLists                       #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        TypeOperators                         #-}
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        PolyKinds                             #-}
{-# LANGUAGE        DataKinds                             #-}
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
import Numeric.LinearAlgebra ( flatten, outer, kronecker )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits ( Nat, type (+), type (^), KnownNat )
import Data.Bit
import Prelude

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState (d :: Nat) = R d  

-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes 
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Nat) = Q { getState :: QState (2^n) }
    deriving Show

-- | Highly experimental inner type for some stateful monad
data ProgramState = 
        ProgramState { vector :: *
                     , lambda :: *
                     , linker :: *
                     }

-- | The product type \(\otimes\)
infixr 0 |><|
data m |><| n = m :>< n

-- | Experimental product type family
type family (p :: b) >< (q :: b) :: b
type instance (QBit n) >< (QBit m) = QBit (n + m)
type instance Bit >< Bit = Bit

-- | The unit type \(* : \top\)
type T = ()

-- | Matrix gate representation
type Gate n = L (2^n) (2^n)

-- | Constructs new qubits
new :: Bit -> QBit 1
new 0 = Q $ V.vector [ 1
                     , 0 ] 

new 1 = Q $ V.vector [ 0
                     , 1 ]

-- | Collapses a qubit state (of size 1) to a single bit
measure :: QBit 1 -> Bit
measure = undefined

-- | We define the tensor product of qbits as the flattened
--   outer product of the state vectors. Note the summed qubit size.
infixl 7 ><
(><) :: (KnownNat n, KnownNat m) => QBit n -> QBit m -> QBit (n + m)
(Q p) >< (Q q) = Q
    let pv = extract p
        pq = extract q
        v  = flatten $ outer pv pq
    in case create v of
        Just v' -> v'
        Nothing -> errorWithoutStackTrace 
            $ "Incorrect vectors " ++ show p ++ " and " ++ show q

-- | Combine the action of two gates
--
-- @
-- matrixI :: Gate 1
-- matrixI = matrix [ 1 , 0 
--                  , 0 , 1 ]
-- 
-- comb = combine matrixH matrixI
-- @
-- 
-- >>> gate comb (new 0 >< new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
-- 
-- >>> hadamard (new 0) >< new 0
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
combine :: (KnownNat n, KnownNat m) => Gate n -> Gate m -> Gate (n + m)
combine p q = let pm = extract p
                  qm = extract q
              in case create $ pm `kronecker` qm of
                  Just m -> m
                  Nothing -> errorWithoutStackTrace
                    $ "Incorrect matrices " ++ show p ++ " and " ++ show q

-- | Create quantum gate from its matrix representation
gate :: KnownNat n => Gate n -> (QBit n -> QBit n)
gate mx (Q q) = Q $ mx #> q

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
