
{-# LANGUAGE        ScopedTypeVariables                   #-}
{-# LANGUAGE        NoImplicitPrelude                     #-}
{-# LANGUAGE        BlockArguments                        #-}
{-# LANGUAGE        TypeFamilies                          #-}
{-# LANGUAGE        Rank2Types                            #-}
{-# LANGUAGE        DataKinds                             #-}
{-# OPTIONS_GHC     -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK not-home                              #-}

{-| 
Module      : Gates
Description : Gate library
Stability   : experimental

Module containing unitary gates and their matrix representations.
-}
module Gates where

import QData
import Numeric.LinearAlgebra.Static as V hiding ( outer )
import Numeric.LinearAlgebra ( flatten, outer, kronecker, ident, toList )
import qualified Numeric.LinearAlgebra as LA ( (><) )
import GHC.TypeLits ( Nat, type (+), type (^),  KnownNat, natVal )
import Data.Proxy ( Proxy(..) )
import Prelude hiding ( id )

-- | Pauli-X gate
--
-- \[ \text{X} = \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![pauliX](images/x.PNG)
pauliX :: Gate 1
pauliX = fromMatrix $ V.matrix 
  [ 0 , 1 
  , 1 , 0 ]

-- | Hadamard gate
-- 
-- \[ \text{X} = \frac1{\sqrt2} \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![hadamard](images/h.PNG)

hadamard :: Gate 1
hadamard = fromMatrix $ sqrt 0.5 * V.matrix 
  [ 1 ,  1
  , 1 , -1 ]

-- | CNOT gate
-- 
-- \[ \text{CNOT} = \begin{bmatrix} 
--    1 & 0 & 0 & 0 \\
--    0 & 1 & 0 & 0 \\
--    0 & 0 & 0 & 1 \\ 
--    0 & 0 & 1 & 0 
--  \end{bmatrix}
-- \]
-- 
-- ![cnot](images/cnot.PNG)
cnot :: Gate 2
cnot = fromMatrix $ V.matrix 
  [ 1, 0, 0, 0
  , 0, 1, 0, 0
  , 0, 0, 0, 1
  , 0, 0, 1, 0 ]

-- | Toffoli gate
--
-- \[ \begin{bmatrix}
--    1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 \\ 
--    0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 \\ 
--    0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 \\ 
--    0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 \\ 
--    0 & 0 & 0 & 0 & 1 & 0 & 0 & 0 \\ 
--    0 & 0 & 0 & 0 & 0 & 1 & 0 & 0 \\ 
--    0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 \\ 
--    0 & 0 & 0 & 0 & 0 & 0 & 1 & 0
-- \end{bmatrix} \]
--
--  ![toffoli](images/toffoli.PNG)
toffoli :: Gate 3
toffoli = fromMatrix $ V.matrix 
  [ 1, 0, 0, 0, 0, 0, 0, 0 
  , 0, 1, 0, 0, 0, 0, 0, 0 
  , 0, 0, 1, 0, 0, 0, 0, 0 
  , 0, 0, 0, 1, 0, 0, 0, 0 
  , 0, 0, 0, 0, 1, 0, 0, 0 
  , 0, 0, 0, 0, 0, 1, 0, 0 
  , 0, 0, 0, 0, 0, 0, 0, 1 
  , 0, 0, 0, 0, 0, 0, 1, 0 ]

-- | The identity gate
identity :: forall (n :: Nat) . KnownNat n => Gate n
identity = fromMatrix let dim = natVal (Proxy :: Proxy n)
          in case create $ ident $ fromInteger $ 2^dim of
              Just  i -> i
              Nothing -> errorWithoutStackTrace
                "Could not deduce matrix dimensions"