module GatesTests (
    runTests
) where

import FunQ ( new, Bit, QBit, hadamard )
import Test.QuickCheck
import Test.QuickCheck.Monadic as TM
import QM
import Numeric.LinearAlgebra as LA
import Internal.Gates (i)
import Helpers
import Gates
import TestCore


runTests :: IO ()
runTests = do 
    putStrLn "QuickCheck tests unmodified QState sums to 1"
    quickCheck prop_sumOne

    putStrLn "QuickCheck tests sum of QState = 1 after applying gates"
    mapM_ quickCheck [prop_sum_hadamard, prop_sum_cnot, prop_sum_pauliX, prop_sum_pauliY, prop_sum_pauliZ]

    putStrLn "QuickCheck tests that matrices are unitary"
    quickCheck $ prop_unitary hmat


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


-- | Checks that the sum of the squared elements in the vector sums to 1 
prop_gate_sum :: (QBit -> QM QBit) -> QState -> Property
prop_gate_sum g q = TM.monadicIO $ do
    run' $ addState q
    (_,size) <- run' getState
    qbt <- run' $ getRandQbit size
    s <- run' $ applyGate q g qbt
    let su = norm_2 $ state s
    TM.assert (su < 1.00001 && su > 0.9999) --due to rounding errors, cannot test == 1

-- Basic quickCheck test, that unmodified QState sums to 1
prop_sumOne :: QState -> Bool
prop_sumOne (QState v) = norm_2 v == 1

-- | Can be run with QuickCheck to test 
prop_sum_hadamard :: QState -> Property
prop_sum_hadamard = prop_gate_sum hadamard

prop_sum_cnot :: QState -> Property
prop_sum_cnot = prop_gate_sum cnot'

prop_sum_pauliX :: QState -> Property
prop_sum_pauliX = prop_gate_sum pauliX

prop_sum_pauliZ :: QState -> Property
prop_sum_pauliZ = prop_gate_sum pauliZ

-- Currently failing
prop_sum_pauliY :: QState -> Property
prop_sum_pauliY = prop_gate_sum pauliY