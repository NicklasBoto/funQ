{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        OverloadedLists                       #-}
{-# LANGUAGE        ConstraintKinds                       #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        TypeOperators                         #-}
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        DerivingVia                           #-}
{-# LANGUAGE        Rank2Types                            #-}
{-# LANGUAGE        PolyKinds                             #-}
{-# LANGUAGE        DataKinds                             #-}
{-# LANGUAGE        GADTs                                 #-}
{-# OPTIONS_GHC     -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK not-home                              #-}

{-|
Module      : QData
Description : qfunc datatypes
Stability   : experimental

The basic language operations.
-}
module QOps 
        ( -- * Q/Bit conversions
          new
        , measure
        ) where

import Numeric.LinearAlgebra.Static as V hiding ( outer )
import Numeric.LinearAlgebra ( flatten, outer, kronecker, ident, toList, asColumn, asRow, magnitude )
import qualified Numeric.LinearAlgebra as LA ( (><), fromList )
import GHC.TypeLits ( Nat, type (+), type (^),  KnownNat, natVal )
import qualified Data.Bit as B ( Bit(..) )
import Data.Proxy ( Proxy(..) )
import Prelude
import Control.Monad.Random as Rand ( fromList, evalRandIO )
import QData ( Bit, QBit(..) )

-- | Constructs new qubits
new :: Bit 1 -> QBit 1
new 0 = Q $ V.fromList [ 1
                       , 0 ]

new 1 = Q $ V.fromList [ 0
                       , 1 ]

-- | Collapses a qubit state (of size 1) to a single bit
measure :: KnownNat n => QBit n -> IO Int
measure = measureI

-- | Measurement using list operations
measureI :: KnownNat n => QBit n -> IO Int
measureI = evalRandIO 
        . Rand.fromList 
        . zip [0..] 
        . map (toRational . (^2) . magnitude) 
        . toList 
        . extract 
        . getState

-- | Measurement using list operations
measureN :: QBit 1 -> IO (Bit 1)
measureN = evalRandIO 
        . Rand.fromList 
        . zip [0,1] 
        . map (toRational . (^2) . magnitude) 
        . toList 
        . extract 
        . getState

-- | Measurement using vector operations
measureLA :: QBit 1 -> IO (Bit 1)
measureLA (Q q) = (evalRandIO 
               . Rand.fromList 
               . zip [0,1] 
               . map toRational) 
               [prob  (LA.fromList [1,0]), prob (LA.fromList [0,1])]
    where projOp b = kronecker (asColumn b) (asRow b)
          prob b = ((^2) . norm_0) $ kronecker (projOp b) ((asColumn . extract) q)