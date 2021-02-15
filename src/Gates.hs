
{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        LiberalTypeSynonyms                   #-}
{-# LANGUAGE        StandaloneDeriving                    #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        FlexibleInstances                     #-}
{-# LANGUAGE        OverloadedLists                       #-}
{-# LANGUAGE        ConstraintKinds                       #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        TypeOperators                         #-} -- ?
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        DerivingVia                           #-}
{-# LANGUAGE        Rank2Types                            #-}
{-# LANGUAGE        PolyKinds                             #-}
{-# LANGUAGE        DataKinds                             #-}
{-# LANGUAGE        GADTs                                 #-}
{-# OPTIONS_GHC     -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK not-home                              #-}

module Gates where

import QData
import Numeric.LinearAlgebra.Static as V hiding ( outer )
import Numeric.LinearAlgebra ( flatten, outer, kronecker, ident, toList )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits ( Nat, type (+), type (^),  KnownNat, natVal )
import GHC.Exts ( IsList(..), Item )
import Data.Proxy ( Proxy(..) )
import Prelude

-- | Combine the action of two gates
--
-- >>> gate (combine matrixH matrixI) (new 0 >< new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
-- 
-- >>> hadamard (new 0) >< id (new 0)
-- Q {getState = (vector [0.7071067811865476,0.0,0.7071067811865476,0.0] :: R 4)}
combine :: (KnownNat n, KnownNat m) => Gate n -> Gate m -> Gate (n + m)
combine p q = let pm = extract p
                  qm = extract q
              in case create $ pm `kronecker` qm of
                  Just  m -> m
                  Nothing -> errorWithoutStackTrace
                    $ "Incorrect matrices " ++ show p ++ " and " ++ show q

-- | Create quantum gate from its matrix representation
gate :: KnownNat n => Gate n -> (QBit n -> QBit n)
gate mx (Q q) = Q $ mx #> q

matrixN :: Gate 1
matrixN = matrix [ 0 , 1 
                 , 1 , 0 ]

-- | Hadamard matrix
matrixH :: Gate 1
matrixH = sqrt 0.5 * matrix [ 1 ,  1
                            , 1 , -1 ]

-- | Hadamard gate acting on single qubits
hadamard :: QBit 1 -> QBit 1
hadamard = gate matrixH

-- | CNOT matrix
matrixC :: Gate 2
matrixC = matrix [ 1, 0, 0, 0
                 , 0, 1, 0, 0
                 , 0, 0, 0, 1
                 , 0, 0, 1, 0 ]

-- | CNOT gate acting on two qubits
cnot :: QBit 2 -> QBit 2
cnot = gate matrixC

-- | The identity matrix
matrixI :: forall (n :: Nat) . KnownNat n => Gate n
matrixI = let dim = natVal (Proxy :: Proxy n)
          in case create $ ident $ fromInteger $ 2^dim of
              Just  i -> i
              Nothing -> errorWithoutStackTrace
                "Could not deduce matrix dimensions"