{-# LANGUAGE FlexibleInstances #-}
module Spec where

import FunQ ( new, Bit, QBit, hadamard )
import Test.QuickCheck
import Test.QuickCheck.Monadic as TM
import QM
import Numeric.LinearAlgebra as LA
import Internal.Gates (i)

import ReversibilityTests ( runTests )
import GateSumToOneTests ( runTests )

main :: IO ()
main = do
    ReversibilityTests.runTests
    GateSumToOneTests.runTests
    return ()


-- | Maximum allowed length of QState
maxAllowedN :: Int
maxAllowedN = 10

-- | The margin allowed for equality checking
eqMargin :: Double
eqMargin = 0.000001

-- | The error message returned when maxAllowedN is exceeded
maxExceededErrorMsg :: String
maxExceededErrorMsg = "your computer almost just died. either you forgot to input a value, or you want to raise the value for allowed inputs"

-- | Sample tests
main :: IO ()
main = do
  quickCheck $ prop_unitary hmat
  quickCheck $ prop_hadamard 8
  quickCheck $ prop_norm 8

-- | Checks that the QState of arbitrary size after a hadamard gate is applied keeps a good norm and
-- that the QState vector only contains two amplitudes at 1/sqrt(2).
-- This function in particular may kill your computer if it's let to run without max bounds.
prop_hadamard :: Int -> Property
prop_hadamard n
  | n > maxAllowedN = errorWithoutStackTrace maxExceededErrorMsg
  | otherwise = monadicIO $ do
    m <- pick $ choose (1,n)
    TM.forAllM (genBits n) $ assertive m
      where assertive :: Int -> [Bit] -> PropertyM IO ()
            assertive m bs = do
              qs@(QState s) <- qrun $ mapM new bs >>= \x -> hadamard (x !! div (length bs-1) m) >> get
              assert $ goodNorm qs && length (filter ((1/sqrt 2) ~=) (toList s)) == 2

-- | Checks that the norm of generated QStates, of 1 < lengths < n , is one
prop_norm :: Int -> Property
prop_norm n
  | n > maxAllowedN = errorWithoutStackTrace maxExceededErrorMsg
  | otherwise = monadicIO $
  TM.forAllM (genBits n) assertive
  where assertive :: [Bit] -> PropertyM IO ()
        assertive bs = do
          qs <- qrun $ mapM_ new bs >> get
          assert $ goodNorm qs

-- | Checks that the given matrix is unitary
prop_unitary :: Matrix C -> Property
prop_unitary mx = foldl (.&&.) isSquare [ isConjugate, innerHolds mx , normal ]
  where isSquare    = property $ rows mx == cols mx
        isConjugate = property $ (#=) (mx LA.<> conj mx) (ident (rows mx))
        normal      = property $ (#=) (conj mx LA.<> mx) (mx LA.<> conj mx)
        detIsOne    = property $ (~=) (abs (det mx)) 1

-- | Checks that the inner product holds during matrix transformations
innerHolds :: Matrix C -> Property
innerHolds mx = forAll genNs test
  where test bs = (~=) ((#>) mx (randV 0 bs) <.> (#>) mx (randV 1 bs)) (randV 0 bs <.> randV 1 bs)
        randV n bs = toColumns (ident (rows mx)) !! (bs !! n)
        genNs = vectorOf 2 (choose (0, rows mx - 1))

-- | Compareas two complex numbers for equality to the 6th decimal
(~=) :: C -> C -> Bool
(~=) a b = bm - eqMargin <= am && am <= bm + eqMargin
  where am = magnitude a
        bm = magnitude b

-- | Compares two complex matrices for equality, using eqAlmost. 
(#=) :: Matrix C -> Matrix C -> Bool
(#=) mx nx = all (==True) $ zipWith (~=) list1 list2
  where list1 = (concat . toLists) mx
        list2 = (concat . toLists) nx

-- | Checks that the norm of the given QState is 1
goodNorm :: QState -> Bool
goodNorm (QState s) = (~=) (norm_2 s :+ 0) 1

-- | Generates a bit string of given length
genBits :: Int -> Gen [Bit]
genBits n = vectorOf n (elements [0,1])

-- | Runs a QM program in PropertyM
qrun :: QM a -> PropertyM IO a
qrun = TM.run . QM.run

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
