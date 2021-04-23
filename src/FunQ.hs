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
testadder n = zipWithM (runtest ((doublefst . head) inputsX)) (take n (drop 8 inputsA)) (take n inputsB)
  where doublefst (a,b,c) = a

runtest :: Bit -> (Bit, Bit, Bit) -> (Bit, Bit, Bit) -> IO ()
runtest x a b = do
  res <- (run . measureAll) (cAdd (new 0) a (cAdd (new 1) a (cAdd (new 1) a (setup b))))
  prettyprint res
  where prettyprint r = putStrLn $ show a ++ "\t" ++ show b ++ "\t" ++ show r ++ "\t" ++
                                   show (correct r) ++ "\t" ++ show (toInt a) ++ "\t" ++ show (toInt b) ++ "\t" ++ show (toInt r)
        correct r = (2*(toInt a) + toInt b) `mod` 2^3 == toInt r
        toInt :: (Bit,Bit,Bit) -> Int
        toInt = toDec . reverse . toList
        toList :: (Bit,Bit,Bit) -> [Bit]
        toList (a,b,c) = [a,b,c]
        toDec :: [Bit] -> Int
        toDec []     = 0
        toDec (b:bs) = fromEnum b + 2*toDec bs

cAdd :: QM QBit -> (Bit,Bit,Bit) -> QM (QBit, QBit, QBit) -> QM (QBit, QBit, QBit)
cAdd x (a2,a1,a0) q = do
  (b2,b1,b0) <- q
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
  return (b2,b1,b0)

setup :: (Bit, Bit, Bit) -> QM (QBit,QBit,QBit)
setup (b2,b1,b0) = do
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
