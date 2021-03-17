module TestCore ( 
    QState(..),
    applyTwice,
    run',
    addState,
    applyGate,
    cnot',
    getRandQbit,
    (~=),
    (#=),
    genBits,
    qrun,
    hmat,
    p8mat,
    Bit,
    new,
    QM,
    QM.run,
    getState,
    QBit,
    get,
    liftM,
    module Gates,
) where  
    -- TODO: would like TestCore to import everything needed
    -- for testing in all other files so just they have to import TestCore
    -- and that's it

import Test.QuickCheck;
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, run)
import Internal.Core
import Core
import Gates
import Control.Monad.Random
import FunQ
import Test.QuickCheck.Monadic as TM
import QM
import Numeric.LinearAlgebra as LA
import Internal.Gates (i)

-- Arbitrary instance for QState, not very pretty :) 
instance Arbitrary QState where
    arbitrary = do
        b <- elements [0,1]
        n <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        m <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        let s = elements [tensorVector m n]
        let t = elements [tensorVector (tensorVector m n) (tensorVector m n)]
        e <- frequency [(7,s),(3,t)]
        return $ QState e


-- | Applies a given gate twice to a given qubit. Returns state before and after the operations
applyTwice :: QBit -> (QBit -> QM QBit) -> QM (QState,QState)
applyTwice qbt g = do
    before <- get
    once <- applyGate before g qbt
    twice <- applyGate once g qbt
    return (before,twice)


-- helper function to run QM computations within the property monad for quickcheck
run' = TM.run . QM.run

-- | Adds given QState
addState :: QState -> QM QState
addState q = do
    put q
    get

--  | Helper function, apply the given gate on a random qubit in state, return the state
applyGate :: QState -> (QBit -> QM QBit) -> QBit -> QM QState
applyGate qs g qbt = do
    addState qs
    g qbt
    get

-- cnot to be used with applyGate for testing purpose
cnot' :: (QBit -> QM QBit)
cnot' q = do
    let p = if link q /= 0 then Ptr $ link q + 1 else Ptr $ link q - 1
    (q',p') <- cnot (q, p)
    return p' -- returns pointer to the first qubit only, dummy implementation for matching of types 

-- Returns index between zero and size of QState
--getRandQbit :: Int -> QM QBit
getRandQbit :: Int -> QM QBit
getRandQbit size = do
        i <- io $ evalRandIO $ getRandomR (0,size)
        return $ Ptr i


-- | Compareas two complex numbers for equality to the 6th decimal
(~=) :: C -> C -> Bool
(~=) a b = bm - eqMargin <= am && am <= bm + eqMargin
  where am = magnitude a
        bm = magnitude b

-- | The margin allowed for equality checking
eqMargin :: Double
eqMargin = 0.000001

-- | Compares two complex matrices for equality, using eqAlmost. 
(#=) :: Matrix C -> Matrix C -> Bool
(#=) mx nx = all (==True) $ zipWith (~=) list1 list2
  where list1 = (concat . toLists) mx
        list2 = (concat . toLists) nx

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
