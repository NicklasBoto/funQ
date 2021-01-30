{-# LANGUAGE        NoImplicitPrelude #-}
{-# LANGUAGE        OverloadedLists   #-}
{-# LANGUAGE        KindSignatures    #-}
{-# LANGUAGE        TypeOperators     #-}
{-# LANGUAGE        DataKinds         #-}
{-# OPTIONS_HADDOCK not-home          #-}

{-|
Module      : Syntax
Description : QBit definition
Stability   : experimental

Contains all of the language as of now, but will be split in the future
-}
module Syntax where -- strictly export safe functions

import Numeric.LinearAlgebra
    ( (#>), flatten, outer, Linear(scale), C, Vector )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits
import Prelude

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState = Vector C

-- | Bit constructors, largely useless. Might define as a restricted `Int` instead.
data Bit = Z | O

-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes 
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Nat) = Q { getState :: QState }
        deriving Show 
        -- TODO define better `Show` instance. 
        -- Q [a, b, c, d] --> a|00> + b|01> + c|10> + d|11>

-- | Highyl experimental inner type for some stateful monad
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
new Z = Q [ 1
          , 0 ] 

new O = Q [ 0
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