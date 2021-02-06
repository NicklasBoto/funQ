{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}

module Teleport where

import Syntax
import Prelude

factor :: QBit 2 -> (QBit 1 >< QBit 1)
factor q = undefined 

epr :: T -> QBit 2
epr _x = cnot $ hadamard (new 0) >< new 0

bellMeasure :: QBit 1 -> (QBit 1 -> Bit >< Bit)
bellMeasure q2 q1 = let (x :>< y) = factor . gate mx $ cnot (q1 >< q2) in measure x :>< measure y
        where   hmx = scale (sqrt 0.5) $ (2 LA.>< 2) [ 1,  1
                                                     , 1, -1 ]
                imx = (2 LA.>< 2) [ 1, 0 
                                  , 0, 1 ]
                mx  = hmx |><| imx

u00 :: QBit 1 -> QBit 1
u00 = gate $ (2 LA.>< 2) [ 1, 0
                         , 0, 1 ]

u01 :: QBit 1 -> QBit 1
u01 = gate $ (2 LA.>< 2) [ 0, 1
                         , 1, 0 ]
                         
u10 :: QBit 1 -> QBit 1
u10 = gate $ (2 LA.>< 2) [ 1,  0
                         , 0, -1 ]

u11 :: QBit 1 -> QBit 1
u11 = gate $ (2 LA.>< 2) [ 0,  1
                         , -1, 0 ]

u :: QBit 1 -> (Bit >< Bit) -> QBit 1
u q (x :>< y) = if unBit x 
        then if unBit y then u11 q else u10 q
        else if unBit y then u01 q else u00 q 

teleport :: (QBit 1 -> Bit >< Bit) >< ((Bit >< Bit) -> QBit 1)
teleport = let (x :>< y) = factor $ epr () 
                in let f = bellMeasure x 
                        in let g = u y 
                        in (f :>< g)