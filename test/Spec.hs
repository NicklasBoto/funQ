{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Spec where

import FunQ ( new, Bit )
import Test.QuickCheck
    ( forAll,
      elements,
      Testable(property),
      Property,
      choose,
      vectorOf,
      (.&&.),
      quickCheck )
import Test.QuickCheck.Monadic as TM
    ( forAllM, run, assert, monadicIO, PropertyM )
import QM ( run, QState(QState), get )
import Numeric.LinearAlgebra as LA
    ( Normed(norm_0),
      C,
      magnitude,
      det,
      (#>),
      (<.>),
      (><),
      cols,
      rows,
      toColumns,
      toLists,
      conj,
      ident,
      (<>),
      Matrix,
      Linear(scale) )
import Internal.Gates (i)

-- | Sample tests
main :: IO ()
main = do
  quickCheck $ testNorm 5
  quickCheck $ isUnitary hmat

-- | Generates QStates of lengths between 1 and n and checks that their norm is 1
testNorm :: Int -> Property
testNorm n = monadicIO $
  TM.forAllM (choose (1,n) >>= flip vectorOf (elements [0,1])) assertive
  where assertive :: [Bit] -> PropertyM IO ()
        assertive bs = do
          (QState s) <- TM.run . QM.run $ mapM_ new bs >> get
          assert $ norm_0 s == 1

-- | Checks that the given matrix is unitary
-- TODO: unfinished
isUnitary :: Matrix C -> Property
isUnitary mx = foldl (.&&.) isSquare [ innerHolds mx , isConjugate, normal, eigenspacesO, diagonalizable ]
  where isConjugate =  property $ eqAlmostMat (mx LA.<> conj mx) (ident (rows mx))
        isSquare = property $ rows mx == cols mx
        normal = property $ eqAlmostMat (conj mx LA.<> mx) (mx LA.<> conj mx)
        detIsOne = property $ eqAlmost (abs (det mx)) 1
        eigenspacesO = property True
        diagonalizable = property True

-- | Checks that the inner product between matrix transformations holds
innerHolds :: Matrix C -> Property
innerHolds mx = forAll genNs test
  where test bs = eqAlmost (magnitude ((<.>) ((#>) mx (randV 0 bs)) ((#>) mx (randV 1 bs)))) (magnitude ((<.>) (randV 0 bs) (randV 1 bs)))
        randV n bs = toColumns (ident (rows mx)) !! (bs !! n)
        genNs = vectorOf 2 (choose (0, rows mx - 1))
        eqAlmost a b = b-0.000001 <= a && a <= b+0.000001

-- | Compareas two complex numbers for equality to the 6th decimal
eqAlmost :: C -> C -> Bool
eqAlmost a b = bm-0.000001 <= am && am <= bm+0.000001
  where am = magnitude a
        bm = magnitude b

-- | Compares two complex matrices for equality, using eqAlmost. 
eqAlmostMat :: Matrix C -> Matrix C -> Bool
eqAlmostMat mx nx = all (==True) $ zipWith eqAlmost list1 list2
  where  list1 = (concat . toLists) mx
         list2 = (concat . toLists) nx


-- | Test matrices for isUnitary
-- | Hadamard matrix
hmat :: Matrix C
hmat = LA.scale (sqrt 0.5) $ (2 LA.>< 2)
    [ 1 ,  1
    , 1 , -1 ]

-- | PhasePi8 matrix
p8mat :: Matrix C
p8mat = (2 LA.>< 2)
  [ 1 , 0
  , 0 , p ]
  where p = exp (i * pi / 4)