
{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        Rank2Types                            #-}
{-# LANGUAGE        DataKinds                             #-}
{-# OPTIONS_GHC     -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK not-home                              #-}

module Gates where

import QData
import Numeric.LinearAlgebra.Static as V hiding ( outer )
import Numeric.LinearAlgebra ( flatten, outer, kronecker, ident, toList )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits ( Nat, type (+), type (^),  KnownNat, natVal )
import Data.Proxy ( Proxy(..) )
import Prelude

matrixN :: Gate 1
matrixN = 
  fromMatrix $ fromList [ 0 , 1 
                        , 1 , 0 ]

-- | Hadamard matrix
matrixH :: Gate 1
matrixH = 
  fromMatrix $ sqrt 0.5 * fromList [ 1 ,  1
                                   , 1 , -1 ]

-- | Hadamard gate acting on single qubits
hadamard :: QBit 1 -> QBit 1
hadamard = run matrixH

-- | CNOT matrix
matrixC :: Gate 2
matrixC = 
  fromMatrix $ fromList [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 0, 1
                        , 0, 0, 1, 0 ]

-- | CNOT gate acting on two qubits
cnot :: QBit 2 -> QBit 2
cnot = run matrixC

-- | The identity matrix
matrixI :: forall (n :: Nat) . KnownNat n => Gate n
matrixI = fromMatrix let dim = natVal (Proxy :: Proxy n)
          in case create $ ident $ fromInteger $ 2^dim of
              Just  i -> i
              Nothing -> errorWithoutStackTrace
                "Could not deduce matrix dimensions"