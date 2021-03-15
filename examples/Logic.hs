module Logic where

import Prelude hiding ( and, or )
import Data.Bit ( unBit )
import QM
import Gates
import Core

and :: (QBit, QBit) -> QM (QBit, QBit, QBit)
and (a,b) = do
    result <- new 0
    toffoli (a,b,result)

or :: (QBit, QBit) -> QM (QBit, QBit, QBit)
or (a,b) = do
    result <- new 1
    pauliX a
    pauliX b
    toffoli (a,b,result)
    pauliX a
    pauliX b
    return (a,b,result)

implies :: (QBit, QBit) -> QM (QBit, QBit, QBit)
implies (a, b) = do
    pauliX a
    or (a,b)

test :: Bit -> Bit -> QM Bit
test a b = do
    q <- new b
    p <- new a
    (_,_,r) <- and (p,q)
    measure r

swaptest :: Bit -> QM Bit
swaptest c = do
    cq <- new c
    a <- new 0
    b <- new 1
    hadamard a
    fredkin (cq, a, b)
    measure a