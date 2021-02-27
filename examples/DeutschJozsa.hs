-- | The Deustch-Jozsa Oracle algorithm
module DeutschJozsa where

import FunQ
import Control.Monad ( replicateM )

type Oracle = ([QBit], QBit) -> QM ([QBit], QBit)

-- | An oracle with a balanced function
balanced :: Oracle
balanced (xs,y) = do
    mapM_ pauliX xs
    mapM_ (\q -> cnot (q,y)) xs
    mapM_ pauliX xs
    return (xs, y)

-- | An oracle with a constant function
constant :: Oracle
constant (xs,y) = do
    zs <- replicateM (length xs) (new 0)
    mapM_ swap $ zip xs zs
    mapM_ (\q -> cnot (q,y)) xs
    mapM_ swap $ zip xs zs
    return (xs, y)

-- | Will return a list of ones if balanced and list of zeros if constant.
-- Size is the number of qubit inputs to the oracle.
deutschJozsa :: Int -> Oracle -> QM [Bit]
deutschJozsa size oracle = do
    xs <- replicateM size (new 0)
    y <- new 1
    mapM_ hadamard xs
    hadamard y
    oracle (xs, y)
    mapM_ hadamard xs
    mapM measure xs
