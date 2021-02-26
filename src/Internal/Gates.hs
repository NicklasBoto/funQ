
{-| 
Module      : Internal.Gates
Description : Gate library internals
Stability   : experimental

Internal matrix operations
-}
module Internal.Gates where

import QM ( QM, QState(QState), QBit(..), put, get, getState )
import Numeric.LinearAlgebra
    ( (#>), (><), ident, kronecker, Complex(..), Matrix, C, toInt)

-- | The imaginary unit
i :: Complex Double
i = 0 :+ 1

applyParallel :: Matrix C -> Matrix C -> Matrix C
applyParallel = kronecker

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
    let m = foldr1 applyParallel list
    applyGate m
    return x

-- run specified gates in parallel
-- TODO
parallel :: [Matrix C] -> QM ()
parallel = undefined

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
        fl = foldr1 applyParallel l
        fr = foldr1 applyParallel r

-- | Produce a matrix running a gate controlled by two other bits
ccontrolMatrix :: Int -> QBit -> QBit -> QBit -> Matrix C -> Matrix C
ccontrolMatrix size (Ptr c1) (Ptr c2) (Ptr t) g = fl + fr
  where idsl = replicate (size - 2) (ident 2)
        idsr = replicate (size - 3) (ident 2)
        lc = insertAt proj0 c1 idsl
        l = insertAt proj0 c2 lc
        rc1 = insertAt proj1 c1 idsr
        rc2 = insertAt proj1 c2 rc1
        r = insertAt g t rc2
        fl = foldr1 applyParallel l
        fr = foldr1 applyParallel r