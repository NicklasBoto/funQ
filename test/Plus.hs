{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Plus where

import Syntax hiding ( measure )
import Data.Bit
import Numeric.LinearAlgebra.Data ( toList )
import Numeric.LinearAlgebra.Static
import Control.Monad.Random as Rand
import GHC.TypeLits
import System.IO.Unsafe

measure :: (KnownNat n, KnownNat (2^n)) => QBit n -> IO Int
measure = evalRandIO
        . Rand.fromList
        . zip [0..]
        . map (toRational . (^2))
        . toList
        . extract
        . getState

plus :: Bit -> Bit -> Bit
plus 1 0 = 1
plus 0 1 = 1
plus _ _ = 0

-- m :: IO Bit
-- m = (\x -> x + x) <$> (measure (hadamard (new 0)))

decide :: String -> String -> IO ()
decide a b = measure (hadamard $ new 0) >>= \case
        0 -> putStrLn a
        1 -> putStrLn b