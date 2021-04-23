{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : FunQ
Description : Main library
Stability   : experimental

Exports the language and simulator
-}
module FunQ
    ( -- * Core operations
      new
    , measure
    , ndist
    , dist
    , controlbit

    -- * Core types
    , QBit
    , Bit
    , QM
    , io

    -- * Gates
    , pauliX
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

    -- * Simulators
    , run
    , runDebug

    -- * Utils
    , bell
    , bellMeasure

    ) where

import Control.Monad ( replicateM, mapM, zipWithM )
import Control.Monad.ListM (zipWithM3)
import Lib.QM ( QM, QBit, run, runDebug, io, checkState)
import Lib.Core
    ( Bit,
      new,
      measure,
      controlbit,
      ndist,
      dist )
import Lib.Gates
    ( cnot,
      hadamard,
      identity,
      pauliX,
      pauliY,
      pauliZ,
      phase,
      phasePi8,
      swap,
      tdagger,
      fredkin,
      toffoli,
      urot,
      crot,
      qft, qftDagger,
      cphase,
      ccphase )

-- | Prepares bell state
bell :: (Bit, Bit) -> QM (QBit, QBit)
bell (a,b) = do
    qa <- new a
    qb <- new b
    hadamard qa
    cnot (qa, qb)

-- | Performs bell measurement
bellMeasure :: (QBit, QBit) -> QM (Bit, Bit)
bellMeasure (x,y) = do
    cnot (x,y)
    hadamard x
    m_x <- measure x
    m_y <- measure y
    return (m_x, m_y)

inputs :: [((Bit,Bit,Bit),(Bit,Bit,Bit))]
inputs = [(op a, op b) | a <- [0..2^3-1], b <- [0..2^3-1]]
  where op = toTup . fillzeros . toBin


toTup [a,b,c] = (a,b,c)
toBin :: Int -> [Bit]
toBin 0 = []
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
toBin n | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
fillzeros as = if length as == 3 then as else replicate (3 - length as) 0 ++ as

inputsA :: [(Bit,Bit,Bit)]
inputsA = map fst inputs

inputsB :: [(Bit,Bit,Bit)]
inputsB = map snd inputs

inputsX :: [(Bit,Bit,Bit)]
inputsX = concat $ replicate 8 [(toTup . fillzeros. toBin) a | a <- [0..2^3-1]]

testadder :: Int -> IO [()]
testadder n = zipWithM (runtestnew ((doublefst . head) inputsX)) (take n (drop 8 inputsA)) (take n inputsB)
  where doublefst (a,b,c) = a

-- runtest :: Bit -> (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> IO ()
-- runtest x a b = do
--   res <- (run . measureAll) (cAddA2 (new 1) a (cAddA1 (new 0) a (cAdd (new 0) a (setup b))))
--   prettyprint a b res

runtestnew :: Bit -> (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> IO ()
runtestnew x a b = do
  let n = setup3 (1,1,0)
  (c,b2,b1,b0) <- (run . measureAll4) $ cAddN (cAddMod (setup b) a (new 1) n) n (new 1)
  prettyprint a b (b2,b1,b0)

prettyprint :: (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> IO ()
prettyprint a b r = putStrLn $ show a ++ "\t" ++ show b ++ "\t" ++ show r ++ "\t" ++
                                   show (correct r) ++ "\t" ++ show (toInt a) ++ "\t" ++ show (toInt b) ++ "\t" ++ show (toInt r)
  where correct r = (4*toInt a + toInt b) `mod` 2^3 == toInt r
        toInt :: (Bit,Bit,Bit) -> Int
        toInt = toDec . reverse . toList
        toList :: (Bit,Bit,Bit) -> [Bit]
        toList (a,b,c) = [a,b,c]
        toDec :: [Bit] -> Int
        toDec []     = 0
        toDec (b:bs) = fromEnum b + 2*toDec bs

-- | Adds n-bit integer a to n-qubit integer b modulo n
cAddMod :: QM (QBit, QBit, QBit, QBit) -> (Bit,Bit,Bit) -> QM QBit -> QM (QBit, QBit, QBit) -> QM (QBit, QBit, QBit, QBit)
cAddMod b a x n = cSubN (cAdd b a x) n x


cAdd :: QM (QBit, QBit, QBit, QBit) -> (Bit,Bit,Bit) -> QM QBit -> QM (QBit, QBit, QBit, QBit)
cAdd b (a2,a1,a0) x = do
  (c,b2,b1,b0) <- b
  x1 <- x
  qft 3 [b2,b1,b0]
  swap (b2,b0)
  if a2 == 1 then cphase (x1,b2) (1/2) else cphase (x1,b2) 0
  if a1 == 1 then cphase (x1,b1) (1/2) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b0) (1/2) else cphase (x1,b0) 0
  if a1 == 1 then cphase (x1,b2) (1/4) else cphase (x1,b2) 0
  if a0 == 1 then cphase (x1,b1) (1/4) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b2) (1/8) else cphase (x1,b2) 0
  swap (b2,b0)
  qftDagger 3 [b2,b1,b0]
  return (c,b2,b1,b0)

cAddN :: QM (QBit, QBit, QBit, QBit) -> QM (QBit, QBit, QBit) -> QM QBit -> QM (QBit, QBit, QBit, QBit)
cAddN b n x = do
  (c,b2,b1,b0) <- b
  (n2,n1,n0) <- n
  x1 <- x
  qft 3 [b2,b1,b0]
  swap (b2,b0)
  ccphase (x1,n2,b2) (1/2)
  ccphase (x1,n1,b1) (1/2)
  ccphase (x1,n0,b0) (1/2)
  ccphase (x1,n1,b2) (1/4)
  ccphase (x1,n0,b1) (1/4)
  ccphase (x1,n0,b2) (1/8)
  swap (b2,b0)
  qftDagger 3 [b2,b1,b0]
  return (c,b2,b1,b0)

cSub :: QM (QBit,QBit, QBit, QBit) -> (Bit,Bit,Bit) -> QM QBit -> QM (QBit, QBit, QBit, QBit)
cSub b (a2,a1,a0) x = do
  (c,b2,b1,b0) <- b
  x1 <- x
  qft 3 [b2,b1,b0]
  swap (b2,b0)
  if a2 == 1 then cphase (x1,b2) (-1/2) else cphase (x1,b2) 0
  if a1 == 1 then cphase (x1,b1) (-1/2) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b0) (-1/2) else cphase (x1,b0) 0
  if a1 == 1 then cphase (x1,b2) (-1/4) else cphase (x1,b2) 0
  if a0 == 1 then cphase (x1,b1) (-1/4) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b2) (-1/8) else cphase (x1,b2) 0
  swap (b2,b0)
  qftDagger 3 [b2,b1,b0]
  cnot (b2,c)
  return (c,b2,b1,b0)

cSubN :: QM (QBit,QBit, QBit, QBit) -> QM (QBit,QBit, QBit) -> QM QBit -> QM (QBit, QBit, QBit, QBit)
cSubN b n x = do
  (c,b2,b1,b0) <- b
  (n2,n1,n0) <- n
  x1 <- x
  qft 3 [b2,b1,b0]
  swap (b2,b0)
  ccphase (x1,n2,b2) (-1/2)
  ccphase (x1,n1,b1) (-1/2)
  ccphase (x1,n0,b0) (-1/2)
  ccphase (x1,n1,b2) (-1/4)
  ccphase (x1,n0,b1) (-1/4)
  ccphase (x1,n0,b2) (-1/8)
  swap (b2,b0)
  qftDagger 3 [b2,b1,b0]
  cnot (b2,c)
  return (c,b2,b1,b0)



cAddA1 :: QM QBit -> (Bit,Bit,Bit) -> QM (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
cAddA1 x (a2,a1,a0) b = do
  (b2,b1,b0) <- b
  x1 <- x
  qft 3 [b2,b1,b0]
  swap (b2,b0)
  if a2 == 1 then cphase (x1,b2) (2/2) else cphase (x1,b2) 0
  if a1 == 1 then cphase (x1,b1) (2/2) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b0) (2/2) else cphase (x1,b0) 0
  if a1 == 1 then cphase (x1,b2) (2/4) else cphase (x1,b2) 0
  if a0 == 1 then cphase (x1,b1) (2/4) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b2) (2/8) else cphase (x1,b2) 0
  swap (b2,b0)
  qftDagger 3 [b2,b1,b0]
  return (b2,b1,b0)

cAddA2 :: QM QBit -> (Bit,Bit,Bit) -> QM (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
cAddA2 x (a2,a1,a0) q = do
  (b2,b1,b0) <- q
  x1 <- x
  qft 3 [b2,b1,b0]
  swap (b2,b0)
  if a2 == 1 then cphase (x1,b2) (4/2) else cphase (x1,b2) 0
  if a1 == 1 then cphase (x1,b1) (4/2) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b0) (4/2) else cphase (x1,b0) 0
  if a1 == 1 then cphase (x1,b2) (4/4) else cphase (x1,b2) 0
  if a0 == 1 then cphase (x1,b1) (4/4) else cphase (x1,b1) 0
  if a0 == 1 then cphase (x1,b2) (4/8) else cphase (x1,b2) 0
  swap (b2,b0)
  qftDagger 3 [b2,b1,b0]
  return (b2,b1,b0)

setup :: (Bit, Bit, Bit) -> QM (QBit,QBit,QBit,QBit)
setup (b2,b1,b0) = do
  c <- new 0
  b21 <- new b2
  b11 <- new b1
  b01 <- new b0
  return (c,b21,b11,b01)

setup3 :: (Bit, Bit, Bit) -> QM (QBit,QBit,QBit)
setup3 (b2,b1,b0) = do
  b21 <- new b2
  b11 <- new b1
  b01 <- new b0
  return (b21,b11,b01)

measureAll :: QM (QBit, QBit, QBit) -> QM (Bit,Bit,Bit)
measureAll q = do
  (b2,b1,b0) <- q
  out2 <- measure b2
  out1 <- measure b1
  out0 <- measure b0
  return (out2,out1,out0)

-- cAdd4 :: QM QBit -> (Bit,Bit,Bit) -> QM (QBit, QBit, QBit) -> QM (QBit, QBit, QBit, QBit)
-- cAdd4 x (a2,a1,a0) q = do
--   (b2,b1,b0) <- q
--   x1 <- x
--   qft 3 [b2,b1,b0]
--   swap (b2,b0)

--   if a2 == 1 then cphase (x1,b2) (1/2) else cphase (x1,b2) 0
--   if a1 == 1 then cphase (x1,b1) (1/2) else cphase (x1,b1) 0
--   if a0 == 1 then cphase (x1,b0) (1/2) else cphase (x1,b0) 0
--   if a1 == 1 then cphase (x1,b2) (1/4) else cphase (x1,b2) 0
--   if a0 == 1 then cphase (x1,b1) (1/4) else cphase (x1,b1) 0
--   if a0 == 1 then cphase (x1,b2) (1/8) else cphase (x1,b2) 0

--   swap (b2,b0)
--   qftDagger 3 [b2,b1,b0]
--   q1 <- new 0
--   return (b2,b1,b0,q1)

-- cAdd5 :: QM QBit -> (Bit,Bit,Bit,Bit) -> QM (QBit,QBit, QBit, QBit) -> QM (QBit,QBit, QBit, QBit, QBit)
-- cAdd5 x (a3,a2,a1,a0) q = do
--   (b3,b2,b1,b0) <- q
--   x1 <- x
--   qft 4 [b3,b2,b1,b0]
--   swap (b3,b0)
--   swap (b2,b1)
  -- if a3 == 1 then cphase (x1,b3) (1/2)  else cphase (x1,b3) 0
  -- if a2 == 1 then cphase (x1,b2) (1/2)  else cphase (x1,b2) 0
  -- if a1 == 1 then cphase (x1,b1) (1/2)  else cphase (x1,b1) 0
  -- if a0 == 1 then cphase (x1,b0) (1/2)  else cphase (x1,b0) 0

  -- if a2 == 1 then cphase (x1,b3) (1/4)  else cphase (x1,b3) 0
  -- if a1 == 1 then cphase (x1,b2) (1/4)  else cphase (x1,b2) 0
  -- if a0 == 1 then cphase (x1,b1) (1/4)  else cphase (x1,b1) 0

  -- if a1 == 1 then cphase (x1,b3) (1/8)  else cphase (x1,b3) 0
  -- if a0 == 1 then cphase (x1,b2) (1/8)  else cphase (x1,b2) 0

  -- if a0 == 1 then cphase (x1,b3) (1/16) else cphase (x1,b3) 0

  -- swap (b2,b1)
  -- swap (b3,b0)
  -- qftDagger 4 [b3,b2,b1,b0]
  -- q1 <- new 0
  -- return (b3,b2,b1,b0,q1)

-- cAdd6 :: QM QBit -> (Bit,Bit,Bit) -> QM (QBit,QBit,QBit, QBit, QBit) -> QM (QBit,QBit,QBit, QBit, QBit, QBit)
-- cAdd6 x (a2,a1,a0) q = do
--   (b4,b3,b2,b1,b0) <- q
--   x1 <- x
--   qft 4 [b3,b2,b1,b0]
--   swap (b4,b0)
--   swap (b3,b1)

--   if a4 == 1 then cphase (x1,b4) (1/2)  else cphase (x1,b4) 0
--   if a3 == 1 then cphase (x1,b3) (1/2)  else cphase (x1,b3) 0
--   if a2 == 1 then cphase (x1,b2) (1/2)  else cphase (x1,b2) 0
--   if a1 == 1 then cphase (x1,b1) (1/2)  else cphase (x1,b1) 0
--   if a0 == 1 then cphase (x1,b0) (1/2)  else cphase (x1,b0) 0

--   if a3 == 1 then cphase (x1,b4) (1/4)  else cphase (x1,b4) 0
--   if a2 == 1 then cphase (x1,b3) (1/4)  else cphase (x1,b3) 0
--   if a1 == 1 then cphase (x1,b2) (1/4)  else cphase (x1,b2) 0
--   if a0 == 1 then cphase (x1,b1) (1/4)  else cphase (x1,b1) 0

--   if a2 == 1 then cphase (x1,b3) (1/8)  else cphase (x1,b3) 0
--   if a1 == 1 then cphase (x1,b2) (1/8)  else cphase (x1,b2) 0
--   if a0 == 1 then cphase (x1,b1) (1/8)  else cphase (x1,b1) 0

--   if a1 == 1 then cphase (x1,b3) (1/16) else cphase (x1,b3) 0
--   if a0 == 1 then cphase (x1,b2) (1/16) else cphase (x1,b2) 0

--   if a0 == 1 then cphase (x1,b3) (1/32) else cphase (x1,b3) 0

--   swap (b3,b1)
--   swap (b4,b0)
--   qftDagger 5 [b4,b3,b2,b1,b0]
--   q1 <- new 0
--   return (b4,b3,b2,b1,b0,q1)



measureAll4 :: QM (QBit, QBit, QBit, QBit) -> QM (Bit,Bit,Bit,Bit)
measureAll4 q = do
  (b3,b2,b1,b0) <- q
  out3 <- measure b3
  out2 <- measure b2
  out1 <- measure b1
  out0 <- measure b0
  return (out3, out2,out1,out0)
