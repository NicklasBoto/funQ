{-# OPTIONS_HADDOCK not-home        #-}

{-| 
Module      : Gates
Description : Gate library
Stability   : experimental

Module containing unitary gates and their matrix representations.
-}
module Gates (
    -- * Unitary gates
      pauliX
    , pauliY
    , pauliZ
    , hadamard
    , phase
    , phasePi8
    , cnot
    , identity
    , swap
) where

import Internal.Gates ( i, applyGate, runGate, controlMatrix )
import QM ( QM, QState(QState), QBit(..), getState, put, get)
import Numeric.LinearAlgebra
    ( Complex(..), (#>), (><), ident, kronecker, Matrix, Linear(scale), C, ident )


-- | CNOT gate
-- 
-- \[ \text{CNOT} = \begin{bmatrix} 
--    1 & 0 & 0 & 0 \\
--    0 & 1 & 0 & 0 \\
--    0 & 0 & 0 & 1 \\ 
--    0 & 0 & 1 & 0 
--  \end{bmatrix}
-- \]
-- 
-- ![cnot](images/cnot.PNG)
cnot :: (QBit, QBit) -> QM (QBit, QBit)
cnot (c, t) = do
  (_, size) <- getState
  let matrixX = (2 >< 2) [ 0, 1, 1, 0 ]
  let g = controlMatrix size c t matrixX
  applyGate g
  return (c,t)

-- | Pauli-X gate
--
-- \[ \text{X} = \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![pauliX](images/x.PNG)
pauliX :: QBit -> QM QBit
pauliX = runGate $ (2 >< 2)
  [ 0 , 1
  , 1 , 0 ]

-- | Pauli-Y gate
--
-- \[ \text{Y} = \begin{bmatrix}
--    0 & -i \\
--    i & 0
-- \end{bmatrix} \]
--
-- ![pauliY](images/y.PNG)
pauliY :: QBit -> QM QBit
pauliY = runGate $ (2 >< 2)
  [ 0 , -i
  , i ,  0 ]

-- | Pauli-Z gate
--
-- \[ \text{Z} = \begin{bmatrix}
--    1 & 0 \\
--    0 & -1
-- \end{bmatrix} \]
--
-- ![pauliZ](images/z.PNG)
pauliZ :: QBit -> QM QBit
pauliZ = runGate $ (2 >< 2)
  [ 1 ,  0
  , 0 , -1 ]

-- | Hadamard gate
-- 
-- \[ \text{X} = \frac1{\sqrt2} \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![hadamard](images/h.PNG)
hadamard :: QBit -> QM QBit
hadamard = runGate $ scale (sqrt 0.5) $ (2 >< 2)
    [ 1 ,  1
    , 1 , -1 ]

-- | Phase gate
--
-- \[ \text{S} = \begin{bmatrix}
--    1 & 0 \\
--    0 & i
-- \end{bmatrix} \]
--
-- ![phase](images/s.PNG)
phase :: QBit -> QM QBit
phase = runGate $ (2 >< 2)
  [ 1 , 0
  , 0 , i ]

-- | Pi/8 gate (T gate)
--
-- \[ \text{T} = \begin{bmatrix}
--    1 & 0 \\
--    0 & e^{i\pi/4}
-- \end{bmatrix} \]
--
-- ![pi8](images/t.PNG)
phasePi8 :: QBit -> QM QBit
phasePi8 = runGate $ (2 >< 2)
  [ 1 , 0
  , 0 , p ]
  where p = exp (i * pi / 8)

-- | Identity gate
--
-- \[ \text{I} = \begin{bmatrix}
--    1 & 0 \\
--    0 & 1
-- \end{bmatrix} \]
--
identity :: QBit -> QM QBit
identity = runGate $ (2 >< 2)
  [ 1 , 0
  , 0 , 1 ]

-- | SWAP gate
-- 
-- \[ \text{SWAP} = \begin{bmatrix} 
--    1 & 0 & 0 & 0 \\
--    0 & 0 & 1 & 0 \\
--    0 & 1 & 0 & 0 \\ 
--    0 & 0 & 0 & 1 
--  \end{bmatrix}
-- \]
-- 
-- ![swap](images/swap.PNG)
swap :: (QBit, QBit) -> QM (QBit, QBit)
swap (p,q) = do
  cnot (p,q)
  cnot (q,p)
  cnot (p,q)