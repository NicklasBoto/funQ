
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
pauliX = fromMatrix $ fromList
  [ 0 , 1 
  , 1 , 0 ]

-- | Pauli-Y gate
--
-- \[ \text{Y} = \begin{bmatrix}
--    0 & -i \\
--    i & 0
-- \end{bmatrix} \]
--
-- ![pauliY](images/y.PNG)
pauliY :: Gate 1
pauliY = fromMatrix $ fromList
  [ 0 , -i
  , i ,  0 ]

-- | Pauli-Z gate
--
-- \[ \text{Z} = \begin{bmatrix}
--    1 & 0 \\
--    0 & -1
-- \end{bmatrix} \]
--
-- ![pauliZ](images/z.PNG)
pauliZ :: Gate 1
pauliZ = fromMatrix $ fromList
  [ 1 ,  0 
  , 0 , -1 ]

-- | Hadamard gate
-- 
-- \[ \text{X} = \frac1{\sqrt2} \begin{bmatrix}
--    0 & 1 \\
--    1 & 0
-- \end{bmatrix} \]
--
-- ![hadamard](images/h.PNG)

hadamard :: Gate 1
hadamard = fromMatrix $ sqrt 0.5 * fromList
  [ 1 ,  1
  , 1 , -1 ]


-- | Phase gate
--
-- \[ \text{S} = \begin{bmatrix}
--    1 & 0 \\
--    0 & i
-- \end{bmatrix} \]
--
-- ![phase](images/s.PNG)
phase :: Gate 1
phase = fromMatrix $ fromList
  [ 1 , 0 
  , 0 , i ]

-- | Pi/8 gate (T gate)
--
-- \[ \text{T} = \begin{bmatrix}
--    1 & 0 \\
--    0 & e^{i\pi/4}
-- \end{bmatrix} \]
--
-- ![pi8](images/t.PNG)
phasePi8 :: Gate 1
phasePi8 = fromMatrix $ fromList
  [ 1 , 0
  , 0 , p ]
  where p = exp (i * pi / 8)


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
cnot = fromMatrix $ fromList
  [ 1, 0, 0, 0
  , 0, 1, 0, 0
  , 0, 0, 0, 1
  , 0, 0, 1, 0 ]

-- | SWAP gate
-- 
-- \[ \text{SWAP} = \begin{bmatrix} 
--    1 & 0 & 0 & 0 \\
--    0 & 0 & 1 & 0 \\
--    0 & 1 & 0 & 0 \\ 
--    0 & 0 & 0 & 1 
--  \end{bmatrix}
-- \]
-- 
-- ![swap](images/swap.PNG)
swap :: Gate 2
swap = fromMatrix $ fromList
  [ 1, 0, 0, 0
  , 0, 0, 1, 0
  , 0, 1, 0, 0
  , 0, 0, 0, 1 ]

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
toffoli = fromMatrix $ fromList
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

-- | Control a gate with a classical bit
controlbit :: KnownNat n => Gate n -> Bit 1 -> Gate n
controlbit g 1 = g
controlbit g 0 = identity

beamsplitter :: Gate 1
beamsplitter = fromMatrix $ sqrt 0.5 * fromList
  [ 1 , i 
  , i , 1 ]