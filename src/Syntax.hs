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
    ( (#>), (><), flatten, outer, Linear(scale), C, Vector )
import GHC.TypeLits ( Nat )
import Prelude

-- | The type of the quantum state. \(Q\) in \(\left[Q, L^*, \Lambda \right]\).
type QState = Vector C

-- | Bit constructors, largely useless. Might define as a restricted `Int` instead.
data Bit = Z | O

-- | The size that QBit depends on.
data Bits = N Nat
          | Bits :+ Bits

-- | Vector state representation of qubit state.
--   Dependent on the number of bits @n@ where the vector becomes 
--   \( \otimes_{i=0}^{n-1} \mathbb{C}^2 \)
newtype QBit (n :: Bits) = Q { getState :: QState }
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

-- | Constructs new qubits
new :: Bit -> QBit (N 1)
new Z = Q [ 1
          , 0 ] 

new O = Q [ 0
          , 1 ]

-- | Collapses a qubit state (of size 1) to a single bit
measure :: QBit (N 1) -> Bit
measure = undefined

-- | We define the tensor product of qbits as the flattened
--   outer product of the state vectors. Note the summed qubit size.
tensorP :: QBit n -> QBit m -> QBit (n :+ m)
tensorP (Q p) (Q q) = Q $ flatten $ p `outer` q

-- | Hadamard gate acting on single qubits
hadamard :: QBit (N 1) -> QBit (N 1)
hadamard (Q q) = Q $ mx #> q
        where mx = scale (sqrt 0.5) $ (2 >< 2) [ 1,  1
                                               , 1, -1 ]

-- | CNOT gate acting on two qubits
cnot :: QBit (N 2) -> QBit (N 2)
cnot (Q q) = Q $ mx #> q
        where mx = (4 >< 4) [ 1, 0, 0, 0
                            , 0, 1, 0, 0
                            , 0, 0, 0, 1
                            , 0, 0, 1, 0 ]


-- ======= NOTES ===========

-- M = (\y.\x.y)p = (\x.y)[y := p] = \x.p
-- (\x.p)Om 
-- FV(M) = { y }

-- Y := \f.(\x. f (x x))(\x. f (x x))
-- Y g 
-- > (\x. g (x x))(\x. g (x x))
--                ~~~~~~~~~~~~^
-- > g ((\x. g (x x)) (\x. g (x x)))
-- > g (Y g)
-- > Y g --> g (Y g) --> g (g (Y g)) ...

-- Y g