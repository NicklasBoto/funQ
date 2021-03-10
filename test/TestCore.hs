module TestCore ( QState(..) ) where  
    -- TODO: would like TestCore to import everything needed
    -- for testing in all other files so just they have to import TestCore
    -- and that's it

import Test.QuickCheck;
import qualified Test.QuickCheck.Monadic as TM (assert, monadicIO, run)
import Internal.Core (newVector, tensorVector)
import Core ( new )
import QM ( io, get, put, run, getState, QState(..), QM, QBit(..) )
import Gates
import Numeric.LinearAlgebra ( toList, realPart, Normed(norm_2) )
import Control.Monad.Random ( evalRandIO, getRandomR, liftM )

-- Arbitrary instance for QState, not very pretty :) 
instance Arbitrary QState where
    arbitrary = do
        b <- elements [0,1]
        n <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        m <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        let s = elements [tensorVector m n]
        let t = elements [tensorVector (tensorVector m n) (tensorVector m n)]
        e <- frequency [(7,s),(3,t)]
        return $ QState e

-- 
-- 
-- -- Not used for now
-- instance Arbitrary (QM QBit) where
--     arbitrary = elements [new 0, new 1]
-- 
-- -- Not used for now
-- instance Arbitrary (QM QState) where
--         arbitrary = do
--             put <$> (arbitrary :: Gen QState)
--             return get
-- 
-- -- Not used for now
-- instance Arbitrary (QM ()) where
--   arbitrary = put <$> (arbitrary :: Gen QState)