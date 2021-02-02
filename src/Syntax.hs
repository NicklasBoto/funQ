{-# LANGUAGE        UndecidableInstances #-}
{-# LANGUAGE        NoImplicitPrelude    #-}
{-# LANGUAGE        OverloadedLists      #-}
{-# LANGUAGE        TypeOperators        #-}
{-# LANGUAGE        TypeFamilies         #-}
{-# LANGUAGE        PolyKinds            #-}
{-# LANGUAGE        DataKinds            #-}
{-# OPTIONS_HADDOCK not-home             #-}

{-|
Module      : Syntax
Description : QBit definition
Stability   : experimental

Contains all of the language as of now, but will be split in the future
-}
module Syntax where -- strictly export safe functions

import Numeric.LinearAlgebra
    ( (#>), flatten, outer, Matrix, Linear(scale), C, Vector, R, kronecker )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits ( Nat, type (+) )
import Data.Bit
import Prelude

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState = Vector R

-- type Nat = Word
-- type family (a :: Nat) + (b :: Nat) :: Nat
-- type instance x + y = x + y

-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes 
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Nat) = Q { getState :: QState } -- static vector size 2^n
        deriving Show 
        -- TODO define better `Show` instance. 
        -- Q [a, b, c, d] --> a|00> + b|01> + c|10> + d|11>

-- type family (m :: a) >< (n :: a) :: a
-- type instance (QBit m) >< (QBit n) = QBit (n + m)
-- type instance Bit >< Bit = Bit

-- | Highly experimental inner type for some stateful monad
data ProgramState = 
        ProgramState { vector :: QState
                     , lambda :: *
                     , linker :: *
                     }

-- | The product type \(\otimes\)
infixr 0 ><
data m >< n = m :>< n

-- | The unit type \(* : \top\)
type T = ()

-- | Constructs new qubits
new :: Bit -> QBit 1
new 0 = Q [ 1
          , 0 ] 

new 1 = Q [ 0
          , 1 ]

-- | Collapses a qubit state (of size 1) to a single bit
measure :: QBit 1 -> Bit
measure = undefined

-- | We define the tensor product of qbits as the flattened
--   outer product of the state vectors. Note the summed qubit size.
tensorProduct :: QBit n -> QBit m -> QBit (n + m)
tensorProduct (Q p) (Q q) = Q . flatten $ p `outer` q

-- | Infix synonym for `tensorProduct`
(><) :: QBit n -> QBit m -> QBit (n + m)
(><) = tensorProduct

-- | Kronecker product. Used to run gates in parallel.
(|><|) :: Matrix R -> Matrix R -> Matrix R
(|><|) = kronecker

-- | Create quantum gate from its matrix representation
gate :: Matrix R -> QBit n -> QBit n
gate mx (Q q) = Q $ mx #> q

-- | Hadamard gate acting on single qubits
hadamard :: QBit 1 -> QBit 1
hadamard (Q q) = Q $ mx #> q
        where mx = scale (sqrt 0.5) $ (2 LA.>< 2) [ 1,  1
                                                  , 1, -1 ]

-- | CNOT gate acting on two qubits
cnot :: QBit 2 -> QBit 2
cnot (Q q) = Q $ mx #> q
        where mx = (4 LA.>< 4) [ 1, 0, 0, 0
                               , 0, 1, 0, 0
                               , 0, 0, 0, 1
                               , 0, 0, 1, 0 ]