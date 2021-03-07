
{-# LANGUAGE OverloadedLists #-}

{-| 
Module      : Internal.Gates
Description : Gate library internals
Stability   : experimental

Internal matrix operations
-}
{-# LANGUAGE FlexibleInstances #-}
module Internal.Gates where

import QM ( QM, QState(QState), QBit(..), put, get, getState ) 
import Numeric.LinearAlgebra
    ( (#>),
      (><),
      dispcf,
      ident,
      kronecker,
      Complex(..),
      Matrix,
      Linear(scale),
      C )
   
import Numeric.LinearAlgebra.Data
    ( (><), dispcf, ident, Complex(..), Matrix, C )
instance {-# OVERLAPS#-} Show (Matrix C) where
  show mx = dispcf 3 mx
  
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

-- | Changes an element at an index in a list.
--  index 0 will change the first element.
changeAt :: a -> Int -> [a] -> [a]
changeAt x index [] = error "changeAt: Can't change an element in an empty list"
changeAt x 0 (_:ys) = x:ys
changeAt x index (y:ys) = y : changeAt x (index - 1)  ys

-- | Apply a 2x2 gate, to a specific qubit.
--
-- It will update the qstate. 
runGate :: Matrix C -> (QBit -> QM QBit)
runGate g x = do
    (state, size) <- getState
    let ids = replicate size (ident 2)
    let list = changeAt g (link x) ids
    let m = foldr1 applyParallel list
    applyGate m
    return x

-- run specified gates in parallel
-- TODO: unfinished
parallel :: [(Matrix C, QBit)] -> QM [QBit]
parallel as = do
  (state, size) <- getState
  let ids = replicate size $ ident 2
  let list = foldr f1 ids as
  let m = foldr1 applyParallel list
  applyGate m
  return $ map snd as
  where f1 :: (Matrix C, QBit) -> [Matrix C] -> [Matrix C]
        f1 (mx, Ptr q) nx = changeAt mx q nx

-- | Produce matrix running a gate controlled by another bit
controlMatrix :: Int -> QBit -> QBit -> Matrix C -> Matrix C
controlMatrix size (Ptr c) (Ptr t) g = fl + fr
  where idsl = replicate size (ident 2)
        idsr = replicate size (ident 2)
        l = changeAt proj0 c idsl
        rc = changeAt proj1 c idsr
        r = changeAt g t rc
        fl = foldr1 applyParallel l
        fr = foldr1 applyParallel r

-- | Produce a matrix running a gate controlled by two other bits
ccontrolMatrix :: Int -> QBit -> QBit -> QBit -> Matrix C -> Matrix C
ccontrolMatrix size (Ptr c1) (Ptr c2) (Ptr t) g = f00 + f01 + f10 + f11
  where ids = replicate size (ident 2)
        m00c = changeAt proj0 c2 $ changeAt proj0 c1 ids
        m01c = changeAt proj1 c2 $ changeAt proj0 c1 ids
        m10c = changeAt proj0 c2 $ changeAt proj1 c1 ids
        m11c = changeAt proj1 c2 $ changeAt proj1 c1 $ changeAt g t ids
        f00  = foldr1 applyParallel m00c
        f01  = foldr1 applyParallel m01c
        f10  = foldr1 applyParallel m10c
        f11  = foldr1 applyParallel m11c

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

-- | Hadamard matrix
hmat :: Matrix C
hmat = scale (sqrt 0.5) $ (2 >< 2)
    [ 1 ,  1
    , 1 , -1 ]

-- | CNOT matrix
cmat :: Matrix C
cmat = (4 >< 4) 
  [ 1, 0, 0, 0
  , 0, 1, 0, 0
  , 0, 0, 0, 1
  , 0, 0, 1, 0]

-- | Generic phase matrix, takes in phase change as radians
phasemat :: Double -> Matrix C 
phasemat r = (2 >< 2)
  [ 1, 0 
  , 0, p]
  where p = exp (i*(r :+ 0))

-- | PauliX matrix
pXmat :: Matrix C
pXmat = (2 >< 2) 
  [ 0, 1
  , 1, 0 ]

-- | PauliY matrix
pYmat :: Matrix C
pYmat = (2 >< 2) 
  [ 0, -i
  , i,  0 ]

-- | PauliZ matrix
pZmat :: Matrix C
pZmat = (2 >< 2)
  [ 1 ,  0
  , 0 , -1 ]

-- | Identity matrix
idmat :: Matrix C 
idmat = (2 >< 2)
  [ 1 , 0
  , 0 , 1 ]