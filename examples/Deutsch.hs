-- | The Deustch Oracle algorithm
module Deutsch where

import FunQ

type Oracle = (QBit, QBit) -> QM (QBit, QBit)

-- | An oracle with a constant function
constant :: Oracle
constant (x,y) = do
    z <- new 0
    swap (z, x)
    cnot (x, y)
    swap (z, x)
    return (x, y) 

-- | An oracle with a balanced function
balanced :: Oracle
balanced (x,y) = do
    pauliX x
    cnot (x,y)
    pauliX x
    return (x, y)

-- | Will return a 1 if balanced and 0 if constant.
deutsch :: Oracle -> QM Bit
deutsch oracle = do
    x <- new 0
    y <- new 1
    hadamard x
    hadamard y
    oracle (x, y)
    hadamard x
    measure x