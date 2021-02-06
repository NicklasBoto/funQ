{-# LANGUAGE DataKinds #-}

module Plus where

import StaticSyntax ( QBit, Bit, new, hadamard, measure )
import Numeric.LinearAlgebra.Data ( toList )
import Control.Monad.Random as Rand

measure :: QBit 1 -> IO Bit 
measure = evalRandIO 
        . Rand.fromList 
        . zip [0,1] 
        . map (toRational . (^2)) 
        . toList 
        . extract 
        . getState

plus :: Bit -> Bit -> Bit
plus 1 0 = 1
plus 0 1 = 1
plus _ _ = 0

m :: IO Bit
m = (\x -> plus x x) <$> measure (hadamard (new 0))