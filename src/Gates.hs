{-# LANGUAGE FlexibleInstances      #-}
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
    , urot
    , crot
    , qft
    , Gate(..)
    , gate
    , inverse
    , controlled
) where

import Internal.Gates
    ( applyGate,
      applyParallel,
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
      idmat,
      Gate(..),
      gateMatrix,
      withSize, parallel ) 
import QM ( QM, QState(QState), QBit(..), getState, put, get )
import Data.Bit ( Bit )
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
cnot = controlled X

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
toffoli = gate (C (C X))

-- | Pauli-X gate
--
-- \[ \text{X} = \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![pauliX](images/x.PNG)
pauliX :: QBit -> QM QBit
pauliX = gate X

-- | Pauli-Y gate
--
-- \[ \text{Y} = \begin{bmatrix}
--    0 & -i \\
--    i & 0
-- \end{bmatrix} \]
--
-- ![pauliY](images/y.PNG)
pauliY :: QBit -> QM QBit
pauliY = gate Y

-- | Pauli-Z gate
--
-- \[ \text{Z} = \begin{bmatrix}
--    1 & 0 \\
--    0 & -1
-- \end{bmatrix} \]
--
-- ![pauliZ](images/z.PNG)
pauliZ :: QBit -> QM QBit
pauliZ = gate Z

-- | Hadamard gate
-- 
-- \[ \text{X} = \frac1{\sqrt2} \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![hadamard](images/h.PNG)
hadamard :: QBit -> QM QBit
hadamard = gate H

-- | Phase gate
--
-- \[ \text{S} = \begin{bmatrix}
--    1 & 0 \\
--    0 & i
-- \end{bmatrix} \]
--
-- ![phase](images/s.PNG)
phase :: QBit -> QM QBit
phase = gate S

-- | Pi/8 gate (T gate)
--
-- \[ \text{T} = \begin{bmatrix}
--    1 & 0 \\
--    0 & e^{i\pi/4}
-- \end{bmatrix} \]
--
-- ![pi8](images/t.PNG)
phasePi8 :: QBit -> QM QBit
phasePi8 = gate T

-- | Hermetian adjoint of T gate (`phasePi8`)
tdagger :: QBit -> QM QBit
tdagger = inverse T

-- | Identity gate
--
-- \[ \text{I} = \begin{bmatrix}
--    1 & 0 \\
--    0 & 1
-- \end{bmatrix} \]
--
identity :: QBit -> QM QBit
identity = gate I

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
swap = gate SWAP

-- | Fredkin gate
fredkin :: (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
fredkin (c,p,q) = do
  cnot (q,p)
  toffoli (c,p,q)
  cnot (q,p)
  return (c,p,q)

-- | UROT gate
urot :: Int -> QBit -> QM QBit
urot k = gate (RPHI p)
  where p = (2*pi) / (2^k)

-- | Controlled UROT
crot :: Int -> (QBit, QBit) -> QM (QBit, QBit)
crot k = controlled (RPHI p)
  where p = (2*pi) / (2^k)

-- | Quantum fourier transform
qft :: [QBit] -> QM [QBit]
qft [] = errorWithoutStackTrace "Cannot perform QFT on zero qubits"
qft qs@((Ptr q):_)
  | notAdjacent (map link qs) =
     errorWithoutStackTrace "Cannot perform QFT on non-adjacent qubits"
  | otherwise = do
      (_, size) <- getState
      let n = length qs
      let matrixQFT = qftMatrix (2 ^ n)
      let ids = replicate (size - n + 1) (ident 2)
      let masqwe = changeAt matrixQFT q ids
      applyGate $ foldr1 applyParallel masqwe
      return qs

-- | Inverse quantum fourier transform
qftDagger :: [QBit] -> QM [QBit]
qftDagger [] = errorWithoutStackTrace "Cannot perform QFT on zero qubits"
qftDagger qs@((Ptr q):_)
  | notAdjacent (map link qs) =
     errorWithoutStackTrace "Cannot perform QFT on non-adjacent qubits"
  | otherwise = do
      (_, size) <- getState
      let n = length qs
      let matrixQFT = tr $ qftMatrix (2 ^ n)
      let ids = replicate (size - n + 1) (ident 2)
      let masqwe = changeAt matrixQFT q ids
      applyGate $ foldr1 applyParallel masqwe
      return qs

class Runnable q where
  gate    :: Gate -> q -> QM q
  inverse :: Gate -> q -> QM q

-- | All one-qubit gates
instance Runnable QBit where
  gate QFT = \q -> head <$> qft [q]
  gate g   = runGate $ withSize 2 g
  inverse  = runGate . tr . withSize 2

-- | All n-qubit gates
instance Runnable [QBit] where
  gate QFT = qft
  gate g = \qs -> do
    (_,size) <- getState
    let gates = replicate size g
    applyGate $ parallel size $ zip gates qs
    return qs
  
  inverse QFT = qftDagger
  inverse g = \qs -> do
    (_,size) <- getState
    let gates = replicate size g
    applyGate $ tr $ parallel size $ zip gates qs
    return qs

instance Runnable (QBit, QBit) where
  gate QFT (p,q) = do
    [x,y] <- qft [p,q]
    return (x,y)
  gate (C g) q = controlled g q
  gate CNOT  q = controlled X q
  gate SWAP (p,q) = do
    cnot (p,q)
    cnot (q,p)
    cnot (p,q)

  inverse (C g) (c,t) = do
    (_,size) <- getState
    let matrix = tr $ controlMatrix size [c,t] g
    applyGate matrix
    return (c,t)

instance Runnable (QBit, QBit, QBit) where
  gate (C (C g)) q@(c1,c2,t) = do
    (_,size) <- getState
    let matrix = controlMatrix size [c1,c2,t] g
    applyGate matrix
    return q
  gate TOFFOLI q = gate (C (C X)) q

  inverse (C (C g)) q@(c1,c2,t) = do
    (_,size) <- getState
    let matrix = tr $ controlMatrix size [c1,c2,t] g
    applyGate matrix
    return q
  inverse TOFFOLI q = inverse (C (C X)) q

class Control c where
  controlled :: Gate -> (c,QBit) -> QM (c,QBit)

instance Control Bit where
  controlled g (1,q) = gate g q >> return (1,q)
  controlled g (0,q) = gate I q >> return (0,q)

instance Control QBit where
  controlled g (c,t) = do
    (_,size) <- getState
    let matrix = controlMatrix size [c,t] g
    applyGate matrix
    return (c,t)
