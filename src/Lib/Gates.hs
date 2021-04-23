{-# OPTIONS_HADDOCK not-home        #-}

{-| 
Module      : Gates
Description : Gate library
Stability   : experimental

Module containing unitary gates and their matrix representations.
-}
module Lib.Gates (
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
    , urot
    , crot
    , qft
    , qftDagger
    , cphase
    , ccphase
) where

import Lib.Internal.Gates
    ( applyGate,
      applyParallel,
      ccontrolMatrix,
      changeAt,
      controlMatrix,
      i,
      notAdjacent,
      qftMatrix,
      runGate,
      hmat,
      phasemat,
      pXmat,
      pYmat,
      pZmat,
      idmat) 
import Lib.QM ( QM, QState(QState), QBit(..), getState, put, get )
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
--    1 & 1 \\
--    1 & -1
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
phase = runGate $ phasemat (1/4)

-- | Pi/8 gate (T gate)
--
-- \[ \text{T} = \begin{bmatrix}
--    1 & 0 \\
--    0 & e^{i\pi/4}
-- \end{bmatrix} \]
--
-- ![pi8](images/t.PNG)
phasePi8 :: QBit -> QM QBit
phasePi8 = runGate $ phasemat (1/8)

-- | Hermetian adjoint of T gate (`phasePi8`)
tdagger :: QBit -> QM QBit
tdagger = runGate $ phasemat (-1/8)

-- | Identity gate
--
-- \[ \text{I} = \begin{bmatrix}
--    1 & 0 \\
--    0 & 1
-- \end{bmatrix} \]
--

cphase :: (QBit, QBit) -> Double -> QM (QBit, QBit)
cphase (c, t) p = do
  (_, size) <- getState
  let g = controlMatrix size c t (phasemat (p :: Double)) 
  applyGate g
  return (c,t)

ccphase :: (QBit, QBit, QBit) -> Double -> QM (QBit, QBit, QBit)
ccphase (c1,c2,t) p = do
  (_, size) <- getState
  let g = ccontrolMatrix size c1 c2 t (phasemat (p :: Double)) 
  applyGate g
  return (c1,c2,t)

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

-- | UROT gate
urot :: Int -> QBit -> QM QBit
urot k = runGate $ (2 >< 2)
  [ 1, 0,
    0, p ]
  where p = exp ((2*pi*i) / (2^k))

-- | Controlled UROT
crot :: Int -> (QBit, QBit) -> QM (QBit, QBit)
crot k (c, t) = do
  (_, size) <- getState
  let p = exp ((2*pi*i) / (2^k))
  let matrixRot = (2 >< 2) [ 1, 0, 0, p ]
  let g = controlMatrix size c t matrixRot
  applyGate g
  return (c,t)

-- | Quantum fourier transform
qft :: Int -> [QBit] -> QM [QBit]
qft _ [] = errorWithoutStackTrace "Cannot perform QFT on zero qubits"
qft n qs@((Ptr q):_)
  | notAdjacent (map link qs) =
     errorWithoutStackTrace "Cannot perform QFT on non-adjacent qubits"
  | otherwise = do
      (_, size) <- getState
      let matrixQFT = qftMatrix (2 ^ n)
      let ids = replicate (size - n + 1) (ident 2)
      let masqwe = changeAt matrixQFT q ids
      applyGate $ foldr1 applyParallel masqwe
      return qs

-- | Inverse quantum fourier transform
qftDagger :: Int -> [QBit] -> QM [QBit]
qftDagger _ [] = errorWithoutStackTrace "Cannot perform QFT on zero qubits"
qftDagger n qs@((Ptr q):_)
  | notAdjacent (map link qs) =
     errorWithoutStackTrace "Cannot perform QFT on non-adjacent qubits"
  | otherwise = do
      (_, size) <- getState
      let matrixQFT = tr $ qftMatrix (2 ^ n)
      let ids = replicate (size - n + 1) (ident 2)
      let masqwe = changeAt matrixQFT q ids
      applyGate $ foldr1 applyParallel masqwe
      return qs
