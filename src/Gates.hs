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
) where

import QM ( QM, QState(QState), QBit(link), i, getState, put, get )
import Numeric.LinearAlgebra
    ( (#>), (><), ident, kronecker, Matrix, Linear(scale), C )

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
runGate :: Matrix C -> (QBit -> QM ())
runGate g x = do
    (state, size) <- getState
    let ids = replicate (size - 1) (ident 2)
    let list = insertAt g (link x) ids
    let m = foldr1 applyParallell list
    applyGate m

-- | Pauli-X gate
--
-- \[ \text{X} = \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![pauliX](images/x.PNG)
pauliX :: QBit -> QM ()
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
pauliY :: QBit -> QM ()
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
pauliZ :: QBit -> QM ()
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
hadamard :: QBit -> QM ()
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
phase :: QBit -> QM ()
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
phasePi8 :: QBit -> QM ()
phasePi8 = runGate $ (2 >< 2)
  [ 1 , 0
  , 0 , p ]
  where p = exp (i * pi / 8)