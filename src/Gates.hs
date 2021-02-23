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
) where

import QM ( QM, QState(QState), QBit(..), getState, put, get)
import Numeric.LinearAlgebra
    ( Complex(..), (#>), (><), ident, kronecker, Matrix, Linear(scale), C, ident )


-- | The imaginary unit
i :: Complex Double
i = 0 :+ 1

applyParallell :: Matrix C -> Matrix C -> Matrix C
applyParallell = kronecker

-- | Apply gate to the current quantum state
applyGate :: Matrix C -> QM ()
applyGate g = do
    (QState v) <- get
    put $ QState $ g #> v

-- | Insert an element at a specific index in a list.
insertAt :: a -> Int -> [a] -> [a]
insertAt x index [] = [x]
insertAt x index xs = l ++ [x] ++ r
    where (l, r) = splitAt index xs

-- | Apply a 2x2 gate, to a specific qubit.
--
-- It will update the qstate. 
runGate :: Matrix C -> (QBit -> QM QBit)
runGate g x = do
    (state, size) <- getState
    let ids = replicate (size - 1) (ident 2)
    let list = insertAt g (link x) ids
    let m = foldr1 applyParallell list
    applyGate m
    return x

-- | Projection of the zero basis vector
proj0 :: Matrix C
proj0 = (2 >< 2)
  [ 1 , 0
  , 0 , 0 ]

-- | Projection of the one basis vector
proj1 :: Matrix C
proj1 = (2 >< 2)
  [ 0 , 0
  , 0 , 1 ]

-- | Produce matrix running a gate controlled by another bit
controlMatrix :: Int -> QBit -> QBit -> Matrix C -> Matrix C
controlMatrix size (Ptr c) (Ptr t) g = fl + fr
  where idsl = replicate (size - 1) (ident 2)
        idsr = replicate (size - 2) (ident 2)
        l = insertAt proj0 c idsl
        rc = insertAt proj1 c idsr
        r = insertAt g t rc
        fl = foldr1 applyParallell l
        fr = foldr1 applyParallell r

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
cnot :: QBit -> QBit -> QM (QBit, QBit)
cnot c t = do
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
