{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Teleport where

import Prelude
import QData
import Gates
import QOps
import GHC.TypeLits

ifThenElse :: Bit 1 -> a -> a -> a
ifThenElse 1 a b = a
ifThenElse 0 a b = b

epr :: QBit 2
epr = run cnot $ run hadamard (new 0) >< new 0

u00, u01, u10, u11 :: Gate 1
u00 = identity
u01 = pauliX
u10 = pauliZ
u11 = pauliZ <> pauliX

u :: (Bit 1, Bit 1) -> Gate 1
u (x,y) = if x then if y then u11 else u10
               else if y then u01 else u00 

bellBasis :: Gate 2
bellBasis = (hadamard >< identity) <> cnot

teleport :: QBit 1 -> QBit 3
teleport q = s
        where b00 = run (cnot <> hadamard >< identity) (new 0 >< new 0)
              s   = run (cnot >< identity) (q >< b00)

-- bellMeasure :: qbit 1 -> (qbit 1 -> bit 2)    
-- u :: qbit -> (bit 2 -> qbit)
-- 
-- bellMeasure x :: qbit 1 -> bit 2 
-- u y :: bit 2 -> qbit

-- (u y) . (bellMeasure x) :: bit 2 -> bit 2


-- let (x,y) = cnot ((><) p q) in (meas (H x, measy))

--(bellMeasure x)              . (u y) (epr ())
-- 

-- u :: QBit 1 -> (Bit >< Bit) -> QBit 1
-- u q (x :>< y) = if unBit x 
--         then if unBit y then u11 q else u10 q
--         else if unBit y then u01 q else u00 q 

-- u :: QBit 1 -> (Bit 1 >< Bit 1 -> QBit 1)
-- u q (x :+ y) = if x then 
--                   if y then run u11 q else run u10 q --rätt storlek?
--                else if y then run u01 q else run u00 q



{-
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
-}