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
    , tdagger
    , fredkin
    , toffoli
) where

import Internal.Gates
    ( ccontrolMatrix,
      controlMatrix,
      runGate,
      applyGate,
      hmat,
      pXmat,
      pYmat,
      pZmat,
      phasemat,
      idmat ) 
import QM ( QM, QState(QState), QBit(..), getState, put, get)
import Numeric.LinearAlgebra
    ( Complex(..), (#>), (><), ident, kronecker, Matrix, Linear(scale), C, ident, tr )

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
  let g = controlMatrix size c t pXmat 
  applyGate g
  return (c,t)

-- toffoli :: (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
-- toffoli (c1,c2,t) = do
--   (_, size) <- getState
--   let matrixX = (2 >< 2) [ 0, 1, 1, 0 ]
--   let g = ccontrolMatrix size c1 c2 t matrixX
--   applyGate g
--   return (c1,c2,t)

-- | Toffoli gate
--
-- \[ \begin{bmatrix}
--    1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 \\ 
--    0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 \\ 
--    0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 \\ 
--    0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 \\ 
--    0 & 0 & 0 & 0 & 1 & 0 & 0 & 0 \\ 
--    0 & 0 & 0 & 0 & 0 & 1 & 0 & 0 \\ 
--    0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 \\ 
--    0 & 0 & 0 & 0 & 0 & 0 & 1 & 0
-- \end{bmatrix} \]
--
--  ![toffoli](images/toffoli.PNG)
toffoli :: (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
toffoli (c1,c2,t) = do
  (_, size) <- getState
  let g = ccontrolMatrix size c1 c2 t pXmat 
  applyGate g
  return (c1,c2,t)

-- | Pauli-X gate
--
-- \[ \text{X} = \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![pauliX](images/x.PNG)
pauliX :: QBit -> QM QBit
pauliX = runGate pXmat

-- | Pauli-Y gate
--
-- \[ \text{Y} = \begin{bmatrix}
--    0 & -i \\
--    i & 0
-- \end{bmatrix} \]
--
-- ![pauliY](images/y.PNG)
pauliY :: QBit -> QM QBit
pauliY = runGate pYmat 

-- | Pauli-Z gate
--
-- \[ \text{Z} = \begin{bmatrix}
--    1 & 0 \\
--    0 & -1
-- \end{bmatrix} \]
--
-- ![pauliZ](images/z.PNG)
pauliZ :: QBit -> QM QBit
pauliZ = runGate pZmat 

-- | Hadamard gate
-- 
-- \[ \text{X} = \frac1{\sqrt2} \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![hadamard](images/h.PNG)
hadamard :: QBit -> QM QBit
hadamard = runGate hmat

-- | Phase gate
--
-- \[ \text{S} = \begin{bmatrix}
--    1 & 0 \\
--    0 & i
-- \end{bmatrix} \]
--
-- ![phase](images/s.PNG)
phase :: QBit -> QM QBit
phase = runGate $ phasemat pi/2

-- | Pi/8 gate (T gate)
--
-- \[ \text{T} = \begin{bmatrix}
--    1 & 0 \\
--    0 & e^{i\pi/4}
-- \end{bmatrix} \]
--
-- ![pi8](images/t.PNG)
phasePi8 :: QBit -> QM QBit
phasePi8 = runGate $ phasemat pi/4

-- | Hermetian adjoint of T gate (`phasePi8`)
tdagger :: QBit -> QM QBit
tdagger = runGate $ phasemat (-pi/4)

-- | Identity gate
--
-- \[ \text{I} = \begin{bmatrix}
--    1 & 0 \\
--    0 & 1
-- \end{bmatrix} \]
--
identity :: QBit -> QM QBit
identity = runGate idmat

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

-- | Fredkin gate
fredkin :: (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
fredkin (c,p,q) = do
  cnot (q,p)
  toffoli (c,p,q)
  cnot (q,p)
  return (c,p,q)
